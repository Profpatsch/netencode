{-# LANGUAGE QuasiQuotes #-}

module Tool where

import Data.Aeson.BetterErrors qualified as Json
import Data.Error.Tree
import Data.Map.Strict qualified as Map
import Json qualified
import Label
import MyPrelude
import System.Environment qualified as Env
import System.Exit qualified as Exit
import System.FilePath ((</>))
import System.FilePath qualified as File
import System.Posix qualified as Posix
import ValidationParseT

data Tool = Tool
  { -- | absolute path to the executable
    toolPath :: FilePath
  }
  deriving stock (Show)

-- | Reads all tools from the @toolsEnvVar@ variable or aborts.
readTools ::
  Label "toolsEnvVar" Text ->
  -- | Parser for Tools we bring with us at build time.
  --
  -- These are executables that we need available, and that we have to ship with the distribution of the program.
  ToolParserT IO tools ->
  IO tools
readTools env toolParser =
  Env.lookupEnv (env.toolsEnvVar & textToString) >>= \case
    Nothing -> do
      Exit.die [fmt|Please set {env.toolsEnvVar} to a directory with all tools we need at runtime (see `Tools` in the code).|]
    Just toolsDir ->
      (Posix.fileExist toolsDir & ifTrueOrErr () [fmt|{env.toolsEnvVar} directory does not exist: {toolsDir}|])
        & thenValidateM
          ( \() ->
              (Posix.getFileStatus toolsDir <&> Posix.isDirectory)
                & ifTrueOrErr () [fmt|{env.toolsEnvVar} does not point to a directory: {toolsDir}|]
          )
        & thenValidateM
          (\() -> toolParser.unToolParser (ExeDir toolsDir))
        <&> first (nestedMultiError [fmt|Could not find all tools in {env.toolsEnvVar}|])
        >>= \case
          Failure err -> Exit.die (err & prettyErrorTree & textToString)
          Success t -> pure t

-- | Reads all tools from the @toolsEnvVar@ variable or aborts. Reads a json object instead of a directory as with 'readTools'.
readToolsJson ::
  Label "toolsEnvVar" Text ->
  -- | Parser for Tools we bring with us at build time.
  --
  -- These are executables that we need available, and that we have to ship with the distribution of the program.
  ToolParserT IO tools ->
  IO tools
readToolsJson env toolParser =
  Env.lookupEnv (env.toolsEnvVar & textToString) >>= \case
    Nothing -> do
      Exit.die [fmt|Please set {env.toolsEnvVar} to a directory with all tools we need at runtime (see `Tools` in the code).|]
    Just toolsJson ->
      toolsJson
        & stringToBytesUtf8
        & toLazyBytes
        & Json.parse (Json.eachInObject Json.asString)
        & first (Json.jsonParseErrorToErrorTree [fmt|Could not parse json object in {env.toolsEnvVar}|])
        & orFail
        & thenValidateM (\tools -> toolParser.unToolParser $ JsonEnv (tools & Map.fromList))
        <&> first (nestedMultiError [fmt|Could not find all tools in {env.toolsEnvVar}|])
        >>= \case
          Failure err -> Exit.die (err & prettyErrorTree & textToString)
          Success t -> pure t

newtype ToolParserT m a = ToolParserT
  { unToolParser ::
      Input ->
      m (Validation (NonEmpty ErrorTree) a)
  }
  deriving
    (Functor, Applicative)
    via (ValidationParseTreeT Input m)

-- | We have two possible ways (TODO: remove the dir way) of adding tools, one is a directory of symlinked exes, the other a json from name to path.
data Input
  = -- | A directory with named executables.
    ExeDir FilePath
  | -- | A json expression in an environment variable that maps from name to absolute path.
    JsonEnv (Map Text FilePath)

-- | Given a file path and the name of the tool executable, see whether it is an executable and return its full path.
readTool :: Text -> ToolParserT IO Tool
readTool exeName = ToolParserT $ \case
  ExeDir toolDir -> do
    let toolPath :: FilePath = toolDir </> (exeName & textToString)
    let read' = True
    let write = False
    let exec = True
    Posix.fileExist toolPath
      & ifTrueOrErr () [fmt|Tool path does not exist: {toolPath}|]
      & thenValidateM
        ( \() ->
            Posix.fileAccess toolPath read' write exec
              & ifTrueOrErr (Tool {..}) [fmt|Tool is not readable/executable: {toolPath}|]
        )
  JsonEnv obj -> do
    let read' = True
    let write = False
    let exec = True
    obj
      & Map.lookup exeName
      & annotate [fmt|Tool does not exist in json object: "{exeName}"|]
      & orFail
      & thenValidateM (\toolPath -> ifTrueOrErr toolPath [fmt|Tool path is not absolute: {toolPath}|] (pure $ File.isAbsolute toolPath))
      & thenValidateM (\toolPath -> ifTrueOrErr toolPath [fmt|Tool path does not exist: {toolPath}|] (Posix.fileExist toolPath))
      & thenValidateM
        ( \toolPath ->
            Posix.fileAccess toolPath read' write exec
              & ifTrueOrErr (Tool {..}) [fmt|Tool is not readable/executable: {toolPath}|]
        )

orFail :: (Applicative m) => Either ErrorTree a -> m (Validation (NonEmpty ErrorTree) a)
orFail = \case
  Left err -> pure $ Failure $ singleton err
  Right val -> pure $ Success val

-- | helper
ifTrueOrErr :: (Functor f) => a -> Text -> f Bool -> f (Validation (NonEmpty ErrorTree) a)
ifTrueOrErr true err io =
  io <&> \case
    True -> Success true
    False -> Failure $ singleton $ singleError $ newError err
