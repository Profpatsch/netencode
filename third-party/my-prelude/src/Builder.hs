module Builder
  ( TextBuilder (..),
    BytesBuilder (..),
    buildText,
    buildTextLazy,
    buildBytes,
    buildBytesLazy,
    textT,
    stringT,
    textLazyT,
    bytesB,
    bytesLazyB,
    utf8B,
    utf8LazyB,
    utf8LenientT,
    utf8LenientLazyT,
    intDecimalT,
    intDecimalB,
    int64DecimalT,
    int64DecimalB,
    integerDecimalT,
    integerDecimalB,
    naturalDecimalT,
    naturalDecimalB,
    doubleDecimalT,
    doubleDecimalB,
    scientificDecimalT,
    scientificDecimalB,
    nominalDiffTimeSecondsT,
    intersperseT,
    intersperseB,
    padPrefixB,
    padPrefixT,
  )
where

import Data.ByteString.Builder qualified as Bytes
import Data.ByteString.Builder.Scientific qualified as Scientific.Bytes
import Data.ByteString.Lazy qualified as ByteStringL
import Data.ByteString.Lazy qualified as Bytes.Lazy
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Int (Int64)
import Data.String
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Text
import Data.Text.Lazy.Builder.Int qualified as Text
import Data.Text.Lazy.Builder.RealFloat qualified as Text
import Data.Text.Lazy.Builder.Scientific qualified as Scientific.Text
import Data.Time (NominalDiffTime)
import MyPrelude

newtype TextBuilder a = TextBuilder {unTextBuilder :: a -> Text.Builder}
  deriving newtype (Semigroup, Monoid)

instance IsString (TextBuilder a) where
  fromString s = TextBuilder $ \_ -> s & fromString

instance Contravariant TextBuilder where
  contramap f (TextBuilder g) = TextBuilder $ g . f

instance Divisible TextBuilder where
  divide :: (a -> (b, c)) -> TextBuilder b -> TextBuilder c -> TextBuilder a
  divide f (TextBuilder bb) (TextBuilder bc) =
    TextBuilder $ \a -> let (b, c) = f a in bb b <> bc c
  conquer = TextBuilder $ \_ -> mempty

-- | Convert a 'TextBuilder' to a strict 'Text' by applying it to a value.
buildText :: TextBuilder a -> a -> Text
buildText (TextBuilder f) a = f a & Text.toLazyText & toStrict

-- | Convert a 'TextBuilder' to a lazy 'Text' by applying it to a value.
buildTextLazy :: TextBuilder a -> a -> Text.Lazy.Text
buildTextLazy (TextBuilder f) a = f a & Text.toLazyText

newtype BytesBuilder a = BytesBuilder {unBytesBuilder :: a -> Bytes.Builder}

instance IsString (BytesBuilder a) where
  fromString s = BytesBuilder $ \_ -> s & fromString

instance Contravariant BytesBuilder where
  contramap f (BytesBuilder g) = BytesBuilder $ g . f

instance Divisible BytesBuilder where
  divide f (BytesBuilder bb) (BytesBuilder bc) =
    BytesBuilder $ \a -> let (b, c) = f a in bb b <> bc c
  conquer = BytesBuilder $ \_ -> mempty

-- | Convert a 'BytesBuilder' to a strict 'ByteString' by applying it to a value.
buildBytes :: BytesBuilder a -> a -> ByteString
buildBytes (BytesBuilder b) a = b a & Bytes.toLazyByteString & toStrictBytes

-- | Convert a 'BytesBuilder' to a lazy 'ByteString' by applying it to a value.
buildBytesLazy :: BytesBuilder a -> a -> Bytes.Lazy.ByteString
buildBytesLazy (BytesBuilder b) a = b a & Bytes.toLazyByteString

textT :: TextBuilder Text
textT = TextBuilder Text.fromText

stringT :: TextBuilder String
stringT = TextBuilder Text.fromString

textLazyT :: TextBuilder Text.Lazy.Text
textLazyT = TextBuilder Text.fromLazyText

bytesB :: BytesBuilder ByteString
bytesB = BytesBuilder Bytes.byteString

bytesLazyB :: BytesBuilder Bytes.Lazy.ByteString
bytesLazyB = BytesBuilder Bytes.lazyByteString

utf8LenientT :: TextBuilder ByteString
utf8LenientT = bytesToTextUtf8Lenient >$< textT

utf8LenientLazyT :: TextBuilder Bytes.Lazy.ByteString
utf8LenientLazyT = bytesToTextUtf8LenientLazy >$< textLazyT

utf8B :: BytesBuilder Text
utf8B = textToBytesUtf8 >$< bytesB

utf8LazyB :: BytesBuilder Text.Lazy.Text
utf8LazyB = textToBytesUtf8Lazy >$< bytesLazyB

intDecimalT :: TextBuilder Int
intDecimalT = TextBuilder Text.decimal

intDecimalB :: BytesBuilder Int
intDecimalB = BytesBuilder Bytes.intDec

int64DecimalT :: TextBuilder Int64
int64DecimalT = TextBuilder Text.decimal

int64DecimalB :: BytesBuilder Int64
int64DecimalB = BytesBuilder Bytes.int64Dec

integerDecimalT :: TextBuilder Integer
integerDecimalT = TextBuilder Text.decimal

integerDecimalB :: BytesBuilder Integer
integerDecimalB = BytesBuilder Bytes.integerDec

naturalDecimalT :: TextBuilder Natural
naturalDecimalT = TextBuilder Text.decimal

naturalDecimalB :: BytesBuilder Natural
naturalDecimalB = toInteger >$< integerDecimalB

doubleDecimalT :: TextBuilder Double
doubleDecimalT = TextBuilder Text.realFloat

doubleDecimalB :: BytesBuilder Double
doubleDecimalB = BytesBuilder Bytes.doubleDec

scientificDecimalT :: TextBuilder Scientific
scientificDecimalT = TextBuilder Scientific.Text.scientificBuilder

scientificDecimalB :: BytesBuilder Scientific
scientificDecimalB = BytesBuilder Scientific.Bytes.scientificBuilder

nominalDiffTimeSecondsT :: TextBuilder NominalDiffTime
nominalDiffTimeSecondsT = truncate @NominalDiffTime @Int >$< intDecimalT

-- TODO: can these be abstracted over Divisible & Semigroup? Or something?
intersperseT :: (forall b. TextBuilder b) -> TextBuilder a -> TextBuilder [a]
intersperseT sep a = ((),) >$< intersperseT' sep a

intersperseT' :: TextBuilder b -> TextBuilder a -> TextBuilder (b, [a])
intersperseT' (TextBuilder sep) (TextBuilder a) = TextBuilder $ \(b, as) -> mintersperse (sep b) (fmap a as)

intersperseB :: (forall b. BytesBuilder b) -> BytesBuilder a -> BytesBuilder [a]
intersperseB sep a = ((),) >$< intersperseB' sep a

intersperseB' :: BytesBuilder b -> BytesBuilder a -> BytesBuilder (b, [a])
intersperseB' (BytesBuilder sep) (BytesBuilder a) = BytesBuilder $ \(b, as) -> mintersperse (sep b) (fmap a as)

-- | Pad the given string
--
-- ATTN: has to build the string first to figure out the length.
padPrefixB :: Natural -> Word8 -> BytesBuilder a -> BytesBuilder a
padPrefixB targetLength w8 (BytesBuilder b) = BytesBuilder $ \a -> do
  let builder = b a
  -- TODO: assert fromIntegral does not overflow
  let tlInt = fromIntegral @Natural @Int64 targetLength
  let len = ByteStringL.length (Bytes.toLazyByteString builder)
  if len < tlInt
    then Bytes.lazyByteString (ByteStringL.replicate (tlInt - len) w8) <> builder
    else builder

-- | Pad the given string
--
-- ATTN: has to build the string first to figure out the length.
padPrefixT :: Natural -> Char -> TextBuilder a -> TextBuilder a
padPrefixT targetLength c (TextBuilder f) = TextBuilder $ \a -> do
  let builder = f a
  let tlInt = fromIntegral @Natural @Int64 targetLength
  let len = Text.Lazy.length (Text.toLazyText builder)
  if len < tlInt
    then Text.fromLazyText (Text.Lazy.replicate (tlInt - len) (Text.Lazy.singleton c)) <> builder
    else builder
