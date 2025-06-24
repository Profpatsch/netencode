{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Parse where

import Control.Category qualified
import Control.Selective (Selective)
import Data.Error.Tree
import Data.Functor.Compose
import Data.Map.Strict qualified as Map
import Data.Semigroup.Traversable
import Data.Semigroupoid qualified as Semigroupoid
import Data.String (IsString (..))
import Data.Text qualified as Text
import FieldParser (FieldParser)
import FieldParser qualified as Field
import Language.Haskell.TH.Syntax qualified as TH
import MyPrelude
import Validation (partitionValidations)
import Prelude hiding (init, maybe)
import Prelude qualified

-- | A generic applicative “vertical” parser.
-- Similar to `FieldParser`, but made for parsing whole structures and collect all errors in an `ErrorTree`.
newtype Parse from to = Parse (from -> Validation ParseError to)
  deriving
    (Functor, Applicative, Selective)
    via ( Compose
            ((->) from)
            (Validation ParseError)
        )

data ParseError = ParseError
  { ctx :: [Text],
    trees :: NonEmpty ErrorTree
  }
  deriving stock (Show)

instance IsString ParseError where
  fromString s = ParseError {ctx = [], trees = s & stringToText & newError & singleError & singleton}

-- | Pretty-print a Context for use in parser error messages.
--
-- TODO: Ideally, we wouldn’t have manually print the context in our error messages, instead attach the context to the error tree automatically.
showContext :: [Text] -> Text
showContext ctx = ctx & Text.intercalate "."

-- | Merge two ParseErrors by attaching the context to their error subtrees and emptying the current context
instance Semigroup ParseError where
  p1 <> p2 = ParseError {ctx = [], trees = mergeParseErrorContextIntoTree p1 <> mergeParseErrorContextIntoTree p2}

-- | We are merging two ParseErrors, so we attach the inner context of each to the error tree list if not empty.
mergeParseErrorContextIntoTree :: ParseError -> NonEmpty ErrorTree
mergeParseErrorContextIntoTree p = case p.ctx of
  IsNonEmpty c -> singleton $ nestedMultiError (c & toList & showContext & newError) p.trees
  IsEmpty -> p.trees

addParseErrrorContext :: Text -> ParseError -> ParseError
addParseErrrorContext n p = p {ctx = n : p.ctx}

collectParseErrorContext :: Error -> NonEmpty ParseError -> ParseError
collectParseErrorContext err p =
  p
    & sconcat
    & ( \ParseError {..} ->
          ParseError {trees = singleton $ nestedMultiError err trees, ..}
      )

instance Semigroupoid Parse where
  o p2 p1 = Parse $ \from -> case runParse' p1 from of
    Failure err -> Failure err
    Success to1 -> runParse' p2 to1

instance Category Parse where
  (.) = Semigroupoid.o
  id = Parse $ \t -> Success t

instance Profunctor Parse where
  lmap f (Parse p) = Parse $ \b -> p (f b)
  rmap = (<$>)

-- | Run a 'Parse' and return the result or a structured error tree.
runParse :: Error -> Parse from to -> from -> Either ErrorTree to
runParse errMsg parser t =
  runParse' parser t
    & first (mergeParseErrorContextIntoTree >>> nestedMultiError errMsg)
    & validationToEither

-- | Internal parse runner (prefer 'useParse' because this leaks implementation details).
runParse' :: Parse from to -> from -> Validation ParseError to
runParse' (Parse f) from = f from

-- | Add a name to the current parser’s context
name :: Text -> Parse from to -> Parse from to
name n inner = Parse $ \from ->
  runParse' inner from
    & first (\ParseError {..} -> ParseError {ctx = n : ctx, ..})

-- | Pass the parse input as plain argument, for example for putting it into the error message.
--
-- NB: if you just need to continue with parsing, use '>>>' for combining parsers.
withFrom :: (from -> Parse from to) -> Parse from to
withFrom f = Parse $ \from -> runParse' (f from) from

-- | Create parser from the given function, while pushing the given Text as context element for nested parsers.
mkParsePushContext :: Text -> (from -> Either ErrorTree to) -> Parse from to
mkParsePushContext toPush f = Parse $ \from -> case f from of
  Right to -> Success to
  Left err -> Failure $ ParseError {ctx = [toPush], trees = singleton err}

-- | Create parser from the given function, while not pushing any context element for nested parsers.
mkParseNoContext :: (from -> Either ErrorTree to) -> Parse from to
mkParseNoContext f = Parse $ \from -> case f from of
  Right to -> Success to
  Left err -> Failure $ ParseError {ctx = [], trees = singleton err}

-- | Accept only exactly the given value
exactly :: (Eq from) => (from -> Text) -> from -> Parse from from
exactly errDisplay from = mkParseNoContext $ \from' ->
  if from == from'
    then Right from'
    else Left [fmt|Field has to be exactly {errDisplay from}, was: {errDisplay from'}|]

-- | Make a parser to parse the whole list
multiple :: Parse a1 a2 -> Parse [a1] [a2]
multiple inner = dimap nonEmpty (Prelude.maybe [] toList) (maybe $ multipleNE inner)

-- | Make a parser to parse the whole non-empty list
multipleNE :: Parse from to -> Parse (NonEmpty from) (NonEmpty to)
multipleNE inner = Parse $ \from ->
  from
    & zipIndex
    & traverse
      ( \(idx, f) ->
          f
            & runParse' inner
            & first (addParseErrrorContext [fmt|[{idx}]|])
      )

-- | Like '(>>>)', but returns the intermediate result alongside the final parse result.
andParse :: Parse to to2 -> Parse from to -> Parse from (to, to2)
andParse outer inner = Parse $ \from -> case runParse' inner from of
  Failure err -> Failure err
  Success to -> runParse' outer to & (second (to,))

-- | Lift a parser into an optional value
maybe :: Parse from to -> Parse (Maybe from) (Maybe to)
maybe inner = Parse $ \case
  Nothing -> Success Nothing
  Just a -> runParse' inner a <&> Just

-- | Assert that there is exactly one element in the list
exactlyOne :: Parse [from] from
exactlyOne = mkParseNoContext $ \case
  [] -> Left [fmt|Expected exactly 1 element, but got 0|]
  [one] -> Right one
  _more -> Left [fmt|Expected exactly 1 element, but got 2|]

-- | Assert that there is exactly zero or one element in the list
zeroOrOne :: Parse [from] (Maybe from)
zeroOrOne = mkParseNoContext $ \case
  [] -> Right Nothing
  [one] -> Right $ Just one
  _more -> Left [fmt|Expected 0 or 1 elements, but got 2|]

-- | Find the first element on which the sub-parser succeeds; if there was no match, return all error messages.
find :: Parse from to -> Parse [from] to
find inner = Parse $ \case
  IsEmpty -> Failure $ [fmt|Wanted to get the first sub-parser that succeeds, but there were no elements in the list|]
  IsNonEmpty ys -> runParse' (findNE' inner) ys

-- | Find the first element on which the sub-parser succeeds; if there was no match, return all error messages.
findNE' :: Parse from to -> Parse (NonEmpty from) to
findNE' inner = Parse $ \xs ->
  xs
    <&> (\x -> runParse' inner x)
    & traverse1
      ( \case
          Success a -> Left a
          Failure e -> Right e
      )
    & \case
      Left a -> Success a
      Right errs ->
        errs
          & zipIndex
          <&> (\(idx, errs') -> addParseErrrorContext [fmt|[{idx}]|] errs')
          & collectParseErrorContext [fmt|None of these sub-parsers succeeded|]
          & Failure

-- | Find all elements on which the sub-parser succeeds; if there was no match, return an empty list
findAll :: Parse from to -> Parse [from] [to]
findAll inner = Parse $ \xs ->
  xs
    <&> (\x -> runParse' inner x)
    & partitionValidations
    & \case
      (_miss, []) ->
        Success []
      (_miss, hits) -> Success hits

-- | Find the given element in the map, and parse it with the given parser.
mapLookup :: (Coercible Text key, Ord key) => key -> Parse from to -> Parse (Map key from) to
mapLookup key inner = do
  let keyT :: Text = coerce key
  name keyT $ Parse $ \m -> case Map.lookup (coerce key) m of
    Nothing -> Failure [fmt|Key "{keyT}" not found in map|]
    Just a -> runParse' inner a

-- | Find the given element in the map, and parse it with the given parser.
--
-- Use this instead of `rmap` to add the map key to the error context.
mapLookupMay :: (Coercible Text key, Ord key) => key -> Parse from to -> Parse (Map key from) (Maybe to)
mapLookupMay key inner = do
  let keyT :: Text = coerce key
  name keyT $ Parse $ \m -> case Map.lookup (coerce key) m of
    Nothing -> Success Nothing
    Just a -> runParse' (Just <$> inner) a

-- | convert a 'FieldParser' into a 'Parse'.
fieldParser :: FieldParser from to -> Parse from to
fieldParser fp = mkParseNoContext (\from -> from & Field.runFieldParser fp & first singleError)

zipIndex :: NonEmpty b -> NonEmpty (Natural, b)
zipIndex = zipNonEmpty (1 :| [2 :: Natural ..])

-- | Parse a literal value at compile time. This is used with Template Haskell, like so:
--
-- > $$("2023-07-27" & literal hyphenatedDay) :: Time.Day
--
-- You need the double @$$@!
--
-- ATTN: This needs an instance of the 'TH.Lift' class for the output type.
-- Many library types don’t yet implement this class, so we have to provide the instances ourselves.
-- See NOTE: Lift for library types
literal :: forall from to. (TH.Lift to) => Parse from to -> from -> TH.Code TH.Q to
literal parser s = do
  case runParse "Literal parse failed" parser s of
    Right a -> [||a||]
    Left err -> TH.liftCode (err & prettyErrorTree & textToString & fail)
