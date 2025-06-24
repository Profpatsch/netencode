{-# LANGUAGE AllowAmbiguousTypes #-}

module Divisive where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import GHC.Records (HasField (getField))
import Label
import MyLabel (LabelPrx (LabelPrx))

-- | Interpolate a field from any record that has a field with the given label.
-- This makes it possible to ergonomically build up records, something like a dual “do”-notation.
--
-- For example, if you have a contravariant TextBuilder that is also a semigroup, you can build up a record like this:
--
-- @
-- bar _ = field #foo textT
--   <> "foo"
--   <> (field #bar naturalDecimalT)
--   <> "x"
--   <> (field #baz naturalDecimalT)
-- @
--
-- and GHC will correctly infer the type signature as
--
-- @
-- bar
--   :: (HasField "foo" a Text,
--       HasField "bar" a Natural,
--       HasField "baz" a Natural)
--   => p -> TextBuilder a
-- @
field :: forall f l a b. (Contravariant f, HasField l a b) => LabelPrx l -> f b -> f a
field LabelPrx a = getField @l >$< a

-- | Combine two divisibles into a struct with any two labelled fields.
divide2 ::
  forall l1 l2 t1 t2 d r.
  (Divisible d, HasField l1 r t1, HasField l2 r t2) =>
  LabelPrx l1 ->
  d t1 ->
  LabelPrx l2 ->
  d t2 ->
  d r
{-# INLINE divide2 #-}
divide2 LabelPrx a LabelPrx b = adapt >$< a `divided` b
  where
    adapt r = (getField @l1 r, getField @l2 r)

-- | Combine two divisibles into a 'T2' with any two labelled fields.
dt2 ::
  forall l1 l2 t1 t2 d.
  (Divisible d) =>
  LabelPrx l1 ->
  d t1 ->
  LabelPrx l2 ->
  d t2 ->
  d (T2 l1 t1 l2 t2)
{-# INLINE dt2 #-}
dt2 LabelPrx a LabelPrx b = adapt >$< a `divided` b
  where
    adapt (T2 a' b') = (getField @l1 a', getField @l2 b')

-- | Combine three divisibles into a struct with any three labelled fields.
divide3 ::
  forall l1 l2 l3 t1 t2 t3 d r.
  (Divisible d, HasField l1 r t1, HasField l2 r t2, HasField l3 r t3) =>
  LabelPrx l1 ->
  d t1 ->
  LabelPrx l2 ->
  d t2 ->
  LabelPrx l3 ->
  d t3 ->
  d r
{-# INLINE divide3 #-}
divide3 LabelPrx a LabelPrx b LabelPrx c = adapt >$< a `divided` b `divided` c
  where
    adapt r = ((getField @l1 r, getField @l2 r), getField @l3 r)

-- | Combine three divisibles into a 'T3' with any three labelled fields.
dt3 ::
  forall l1 l2 l3 t1 t2 t3 d.
  (Divisible d) =>
  LabelPrx l1 ->
  d t1 ->
  LabelPrx l2 ->
  d t2 ->
  LabelPrx l3 ->
  d t3 ->
  d (T3 l1 t1 l2 t2 l3 t3)
{-# INLINE dt3 #-}
dt3 LabelPrx a LabelPrx b LabelPrx c = adapt >$< a `divided` b `divided` c
  where
    adapt (T3 a' b' c') = ((getField @l1 a', getField @l2 b'), getField @l3 c')
