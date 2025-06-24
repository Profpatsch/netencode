{-# LANGUAGE QuasiQuotes #-}

module Deployment where

import Data.Aeson qualified as Json
import Data.Aeson.BetterErrors qualified as Json
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Char qualified as Char
import Data.Error.Tree (ErrorTree, prettyErrorTree)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Json qualified
import Label
import MyPrelude
import Pretty
import System.Directory qualified as Dir
import System.Environment qualified as Env
import System.Exit (ExitCode (..))
import System.Exit qualified as Exit
import System.Process.Typed qualified as Process
import Prelude hiding (log)

main :: T2 "dockerImageName" Text "dockerhubRepository" Text -> IO ()
main config = do
  -- Then get the event description for this job
  summary <- getGithubActionStepSummaryFile

  -- ATTN: The parser contains all IO that is done (to prevent returning a lot of data and then unpacking it again)
  res <- getGithubActionEvent eventParser

  -- Finally run our parsed actions & print the output that should be put in the job summary
  toPrint <- res.eventBody.toPrintIO
  Text.writeFile summary toPrint
  where
    eventParser ::
      Label "eventName" Text ->
      Json.ParseT
        ErrorTree
        Identity
        (T2 "eventName" Text "toPrintIO" (IO Text))
    eventParser ev = do
      toPrint <- case ev.eventName of
        -- We are in the release action
        "release" ->
          do
            -- https://docs.github.com/en/webhooks-and-events/webhooks/webhook-events-and-payloads?actionType=released#release
            obj <- Json.asObject
            (release, author) <- Json.key "release" $ do
              name <- Json.key "name" (Json.perhaps Json.asText)
              body <- Json.key "body" (Json.perhaps Json.asText)
              tag_name <- Json.key "tag_name" Json.asText
              target_commitish <- Json.key "target_commitish" Json.asText
              html_url <- Json.key "html_url" Json.asText
              author <-
                Json.key
                  "author"
                  ( Json.perhaps $ do
                      typ <- Json.keyMay "type" Json.asText
                      login <- Json.key "login" Json.asText
                      url <- Json.keyMay "url" Json.asText
                      avatar_url <- Json.keyMay "avatar_url" Json.asText
                      pure $ Author {..}
                  )
              pure (Release {..}, author)
            pure $ do
              -- We pass in the filename we are gonna upload in the upload-artifact action
              dockerImageArtifactFileName <-
                getDockerImageArtifactFilename
              pushReport <- buildAndPushDockerImage config config release
              Dir.copyFile pushReport.dockerImage.storePath dockerImageArtifactFileName
              pure $
                printReleaseEvent
                  pushReport
                  release
                  (T2 (label @"author" author) (label @"fullEvent" obj))
        -- We want to build a docker image
        -- (TODO: in case we ever want to use workflow_dispatch for two things,
        -- we should refactor the deployment script to take an argument of what action is to be done,
        -- instead of indirectly assuming from the github events like we do right now.)
        "workflow_dispatch" -> do
          obj <- Json.asObject
          gitRef <- Json.keyLabel @"gitRef" "ref" Json.asText
          pure $ do
            -- We pass in the filename we are gonna upload in the upload-artifact action
            dockerImageArtifactFileName <-
              getDockerImageArtifactFilename
            shortGitHash <- getShortGitHashForRef gitRef
            dockerImage <-
              buildDockerImage $
                T2
                  (label @"dockerImageTag" shortGitHash)
                  (getLabel @"dockerImageName" config)
            Dir.copyFile dockerImage.storePath dockerImageArtifactFileName
            pure @IO
              [fmt|
# Build Docker Image Action Overview

Action Event [had type `{ev.eventName}`](https://docs.github.com/en/webhooks-and-events/webhooks/webhook-events-and-payloads).

We attached the docker image to this run’s artifacts.
You can download it, `unzip` it(!!), and import it with `docker load -i <file>`.
The image will be imported as `{dockerImage.dockerImageNameAndTag}`.

Full event body:

```json
{obj & Json.Object & Pretty.showPrettyJson}
```
|]
        _ ->
          do
            obj <- Json.asObject
            pure $
              pure @IO
                [fmt|
# Action Overview

Action Event [had type `{ev.eventName}`](https://docs.github.com/en/webhooks-and-events/webhooks/webhook-events-and-payloads).

Full event body:

```json
{obj & Json.Object & Pretty.showPrettyJson}
```
|]
      pure $
        T2
          (label @"eventName" ev.eventName)
          (label @"toPrintIO" toPrint)

    dockerRegistryDomain :: DockerRegistry -> Text
    dockerRegistryDomain Dockerhub = "hub.docker.com"

    prettyReleaseAuthor :: Maybe Author -> Text
    prettyReleaseAuthor = \case
      Nothing -> "(unknown author)"
      Just a -> do
        let mayUrlName :: Text = case a.url of
              Nothing -> [fmt|`{a.login}`|]
              Just url -> [fmt|[{a.login}]({url})|]
        let mayAvatar :: Text = case a.avatar_url of
              Nothing -> ""
              -- the `&s=30` is a hack to resize the avatar to a small inline image (via github’s image resize url functionality)
              -- via https://gist.github.com/uupaa/f77d2bcf4dc7a294d109?permalink_comment_id=2180065#gistcomment-2180065
              Just avatar -> [fmt|![Avatar of user]({avatar}&s=30)|]
        case a.typ of
          Nothing -> [fmt|Github account {mayUrlName} {mayAvatar}|]
          Just typ -> [fmt|Github {typ} {mayUrlName} {mayAvatar}|]

    printReleaseEvent ::
      PushReport ->
      Release ->
      T2
        "author"
        (Maybe Author)
        "fullEvent"
        Json.Object ->
      Text
    printReleaseEvent pushReport release other = do
      let name = release.name & fromMaybe "(unnamed release)"
          bodyPlain = release.body & fromMaybe ""
          body =
            if not (bodyPlain & Text.any Char.isLetter)
              then "(no body)"
              else bodyPlain

      [fmt|
# Release for tag `{release.tag_name}`

Release was done on git commit-ish (i.e. branch): `{release.target_commitish}`
by {prettyReleaseAuthor other.author}

We successfully pushed the docker image to `{pushReport.remoteImageName}`
on the docker registry [{dockerRegistryDomain pushReport.dockerRegistry}]({dockerRegistryDomain pushReport.dockerRegistry}).

[Link to release]({release.html_url})

# Release info

## {name}

{body}

---

# Full `release` event object:

```json
{other.fullEvent & Json.Object & Pretty.showPrettyJson }
```
|]

-- | A github release object
data Release = Release
  { -- | name of release
    name :: Maybe Text,
    -- | markdown body of release text
    body :: Maybe Text,
    -- | name of the git tag
    tag_name :: Text,
    -- | Commit-ish (e.g. branch) the git tag sits on top of
    target_commitish :: Text,
    -- | The URL to the github page of the release
    html_url :: Text
  }

-- | A github release Author
data Author = Author
  { -- | The type of the github account of the author, if known (e.g “Bot” or “User”, …)
    typ :: Maybe Text,
    -- | The github login name of the account
    login :: Text,
    -- | The URL to the author’s github page
    url :: Maybe Text,
    -- | The URL to the author’s avatar image
    avatar_url :: Maybe Text
  }

-- | Get the filename to copy the docker image to (so that it can be uploaded as workflow artifact)
getDockerImageArtifactFilename :: IO FilePath
getDockerImageArtifactFilename =
  Env.lookupEnv "DOCKER_IMAGE_ARTIFACT_FILENAME"
    <&> annotate "Unable to read DOCKER_IMAGE_ARTIFACT_FILENAME for where to copy the docker image to."
    >>= unwrapIOError

-- | Get the filename to pipe a step summary into
-- see https://docs.github.com/en/actions/using-workflows/workflow-commands-for-github-actions#adding-a-job-summary
getGithubActionStepSummaryFile :: IO FilePath
getGithubActionStepSummaryFile =
  Env.lookupEnv "GITHUB_STEP_SUMMARY"
    <&> annotate "Unable to read GITHUB_STEP_SUMMARY for writing summary for this build step."
    >>= unwrapIOError

-- | Parse the github action event from the relevant file
-- see https://docs.github.com/en/actions/learn-github-actions/variables
-- and https://docs.github.com/en/actions/using-workflows/events-that-trigger-workflows
getGithubActionEvent :: (Label "eventName" Text -> Json.ParseT ErrorTree Identity actionEvent) -> IO (T2 "eventName" Text "eventBody" actionEvent)
getGithubActionEvent parsers = do
  evName :: Label "eventName" Text <-
    Env.lookupEnv "GITHUB_EVENT_NAME"
      <&> annotate "Unable to read GITHUB_EVENT_NAME for getting the event name that triggered this build step."
      >>= unwrapIOError
        <&> stringToText
        <&> label @"eventName"
  evPath :: FilePath <-
    Env.lookupEnv "GITHUB_EVENT_PATH"
      <&> annotate "Unable to read GITHUB_EVENT_PATH for getting the event body that triggered this build step."
      >>= unwrapIOError
  let parser = parsers evName

  ev <-
    Bytes.Lazy.readFile evPath
      <&> Json.parse parser
      <&> first (Json.jsonParseErrorToErrorTree [fmt|Unable to parse file GITHUB_EVENT_PATH "{evPath}" with GITHUB_EVENT_NAME "{evName.eventName}"|])
      <&> first (newError . prettyErrorTree)
      >>= unwrapIOError

  pure $ T2 (getLabel @"eventName" evName) (label @"eventBody" ev)

data PushReport = PushReport
  { remoteImageName :: Text,
    dockerRegistry :: DockerRegistry,
    dockerImage :: Label "storePath" FilePath
  }

data DockerRegistry
  = Dockerhub

-- | Build docker image and return its store path
buildDockerImage ::
  ( HasField "dockerImageTag" image Text,
    HasField "dockerImageName" image Text
  ) =>
  image ->
  IO
    ( T2
        "storePath"
        FilePath
        "dockerImageNameAndTag"
        Text
    )
buildDockerImage image = do
  path <-
    runCommandExpect0
      "nix-build"
      ( concat
          [ ["--out-link", "./docker-image"],
            ["--attr", "docker-image"],
            ["--argstr", "dockerImageName", image.dockerImageName],
            ["--argstr", "dockerImageTag", image.dockerImageTag],
            ["default.nix"]
          ]
      )
      <&> bytesToTextUtf8Lenient
      <&> textToString
      <&> label @"storePath"
  pure $ T2 path (label @"dockerImageNameAndTag" [fmt|{image.dockerImageName}:{image.dockerImageTag}|])

buildAndPushDockerImage ::
  ( HasField "dockerImageName" image Text,
    HasField "dockerhubRepository" config Text
  ) =>
  config ->
  image ->
  Release ->
  IO PushReport
buildAndPushDockerImage config image release = do
  logInfo [fmt|Building Docker image|]
  dockerImage <-
    buildDockerImage $
      T2
        (label @"dockerImageName" image.dockerImageName)
        (label @"dockerImageTag" release.tag_name)
  -- First we load the image into the docker daemon
  runCommandExpect0NoStdout
    "docker"
    [ "load",
      "-i",
      "./docker-image"
    ]

  -- Push a given image to AWS/ECR using the local docker daemon.
  -- Hint: Login to aws ecr must be done before executing this script

  let dockerRegistry = Dockerhub
  let localImageName =
        [fmt|{image.dockerImageName}:{release.tag_name}|]
  let remoteImageName =
        [fmt|{config.dockerhubRepository}:{release.tag_name}|]
  runCommandExpect0NoStdout
    "docker"
    [ "tag",
      localImageName,
      remoteImageName
    ]
  logInfo [fmt|Pushing docker image "{localImageName}" to acws/ecr as "{remoteImageName}|]
  case dockerRegistry of
    Dockerhub ->
      runCommandExpect0NoStdout
        "docker"
        [ "push",
          remoteImageName
          -- dockerhub is the default so we don’t need to specify a registry here
        ]
  pure
    PushReport
      { dockerImage = getLabel @"storePath" dockerImage,
        ..
      }

-- | Get the git hash for the given ref; assumes the git repository is checked out and we are in it.
getShortGitHashForRef :: (HasField "gitRef" r Text) => r -> IO Text
getShortGitHashForRef dat =
  runCommandExpect0
    "git"
    [ "rev-parse",
      "--short",
      dat.gitRef
    ]
    <&> bytesToTextUtf8Lenient

-- | Given a a command, the executable and arguments,
-- spawn the tool as subprocess and collect its stdout (stderr will go to our stderr).

-- Will strip the stdout of trailing newlines.
--
-- If the executable is not a path, it will be resolved via the @PATH@ environment variable.
runCommand :: FilePath -> [Text] -> IO (Exit.ExitCode, ByteString)
runCommand executable args = do
  let bashArgs = prettyArgsForBash ((executable & stringToText) : args)
  logInfo [fmt|Running: $ {bashArgs}|]
  Process.proc
    executable
    (args <&> textToString)
    & Process.readProcessStdout
    <&> second toStrictBytes
    <&> second stripWhitespaceFromEnd

-- TODO: This is reversing the whole string *twice*. Can we strip from end without doing that?
stripWhitespaceFromEnd :: ByteString -> ByteString
stripWhitespaceFromEnd = ByteString.reverse . ByteString.dropWhile (\w -> w == charToWordUnsafe '\n') . ByteString.reverse

-- | Given a a command, the executable and arguments,
-- spawn the tool as subprocess and run it to conclusion.
--
-- If the executable is not a path, it will be resolved via the @PATH@ environment variable.
runCommandNoStdout :: FilePath -> [Text] -> IO Exit.ExitCode
runCommandNoStdout executable args = do
  let bashArgs = prettyArgsForBash ((executable & stringToText) : args)
  logInfo [fmt|Running: $ {bashArgs}|]
  Process.proc
    executable
    (args <&> textToString)
    & Process.runProcess

-- | Like 'runCommand' but exit if the command returns a non-0 status.
runCommandExpect0 :: FilePath -> [Text] -> IO ByteString
runCommandExpect0 executable args =
  runCommand executable args >>= \case
    (ex, out) -> do
      checkStatus0 executable ex
      pure out

-- | Like 'runCommandNoStdout' but exit if the command returns a non-0 status.
runCommandExpect0NoStdout :: FilePath -> [Text] -> IO ()
runCommandExpect0NoStdout executable args =
  runCommandNoStdout executable args >>= \case
    ex -> checkStatus0 executable ex

-- | Check whether a command exited 0 or crash.
checkStatus0 :: FilePath -> Exit.ExitCode -> IO ()
checkStatus0 executable = \case
  ExitSuccess -> pure ()
  ExitFailure status -> do
    logCritical [fmt|Command `{executable}` did not exit with status 0 (success), but status {status}|]

-- | Pretty print a command line in a way that can be copied to bash.
prettyArgsForBash :: [Text] -> Text
prettyArgsForBash = Text.intercalate " " . map simpleBashEscape

-- | Simple escaping for bash words. If they contain anything that’s not ascii chars
-- and a bunch of often-used special characters, put the word in single quotes.
simpleBashEscape :: Text -> Text
simpleBashEscape t = do
  case Text.find (not . isSimple) t of
    Just _ -> escapeSingleQuote t
    Nothing -> t
  where
    -- any word that is just ascii characters is simple (no spaces or control characters)
    -- or contains a few often-used characters like - or .
    isSimple c =
      Char.isAsciiLower c
        || Char.isAsciiUpper c
        || Char.isDigit c
        -- These are benign, bash will not interpret them as special characters.
        || List.elem c ['-', '.', ':', '/']
    -- Put the word in single quotes
    -- If there is a single quote in the word,
    -- close the single quoted word, add a single quote, open the word again
    escapeSingleQuote t' = "'" <> Text.replace "'" "'\\''" t' <> "'"

-- | Log the message on the normal logging level & exit the program
logCritical :: Text -> IO a
logCritical msg = do
  putStderrLn msg
  liftIO $ Exit.exitWith (ExitFailure 1)

logInfo :: Text -> IO ()
logInfo msg = do
  putStderrLn msg
