{-# OPTIONS_GHC -Wno-orphans #-}

module MyLabel where

import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.Records (HasField (..))
import GHC.TypeLits (Symbol)
import Label
import MyPrelude
import Prelude hiding (span)

-- case-match on an e2 with a t2 that provides the relevant functions
caseE2 ::
  forall l1 t1 l2 t2 matcher r.
  ( HasField l1 matcher (t1 -> r),
    HasField l2 matcher (t2 -> r)
  ) =>
  matcher ->
  E2 l1 t1 l2 t2 ->
  r
{-# INLINE caseE2 #-}
caseE2 m e2 = do
  let f1 = getField @l1 m
  let f2 = getField @l2 m
  case e2 of
    E21 a -> f1 $ getField @l1 a
    E22 b -> f2 $ getField @l2 b

caseE3 ::
  forall l1 t1 l2 t2 l3 t3 matcher r.
  ( HasField l1 matcher (t1 -> r),
    HasField l2 matcher (t2 -> r),
    HasField l3 matcher (t3 -> r)
  ) =>
  matcher ->
  E3 l1 t1 l2 t2 l3 t3 ->
  r
{-# INLINE caseE3 #-}
caseE3 m e3 = do
  let f1 = getField @l1 m
  let f2 = getField @l2 m
  let f3 = getField @l3 m
  case e3 of
    E31 a -> f1 $ getField @l1 a
    E32 b -> f2 $ getField @l2 b
    E33 c -> f3 $ getField @l3 c

e21 :: forall l1 t1 l2 t2. LabelPrx l1 -> t1 -> E2 l1 t1 l2 t2
{-# INLINE e21 #-}
e21 LabelPrx a = E21 (label @l1 a)

e22 :: forall l1 t1 l2 t2. LabelPrx l2 -> t2 -> E2 l1 t1 l2 t2
{-# INLINE e22 #-}
e22 LabelPrx b = E22 (label @l2 b)

e31 :: forall l1 t1 l2 t2 l3 t3. LabelPrx l1 -> t1 -> E3 l1 t1 l2 t2 l3 t3
{-# INLINE e31 #-}
e31 LabelPrx a = E31 (label @l1 a)

e32 :: forall l1 t1 l2 t2 l3 t3. LabelPrx l2 -> t2 -> E3 l1 t1 l2 t2 l3 t3
{-# INLINE e32 #-}
e32 LabelPrx b = E32 (label @l2 b)

e33 :: forall l1 t1 l2 t2 l3 t3. LabelPrx l3 -> t3 -> E3 l1 t1 l2 t2 l3 t3
{-# INLINE e33 #-}
e33 LabelPrx c = E33 (label @l3 c)

t2 :: forall l1 t1 l2 t2. LabelPrx l1 -> t1 -> LabelPrx l2 -> t2 -> T2 l1 t1 l2 t2
{-# INLINE t2 #-}
t2 LabelPrx a LabelPrx b = T2 (label @l1 a) (label @l2 b)

t2A :: forall f l1 t1 l2 t2. (Applicative f) => LabelPrx l1 -> f t1 -> LabelPrx l2 -> f t2 -> f (T2 l1 t1 l2 t2)
{-# INLINE t2A #-}
t2A LabelPrx a LabelPrx b = T2 <$> (label @l1 <$> a) <*> (label @l2 <$> b)

t3 :: forall l1 t1 l2 t2 l3 t3. LabelPrx l1 -> t1 -> LabelPrx l2 -> t2 -> LabelPrx l3 -> t3 -> T3 l1 t1 l2 t2 l3 t3
{-# INLINE t3 #-}
t3 LabelPrx a LabelPrx b LabelPrx c = T3 (label @l1 a) (label @l2 b) (label @l3 c)

t3A :: forall f l1 t1 l2 t2 l3 t3. (Applicative f) => LabelPrx l1 -> f t1 -> LabelPrx l2 -> f t2 -> LabelPrx l3 -> f t3 -> f (T3 l1 t1 l2 t2 l3 t3)
{-# INLINE t3A #-}
t3A LabelPrx a LabelPrx b LabelPrx c = T3 <$> (label @l1 <$> a) <*> (label @l2 <$> b) <*> (label @l3 <$> c)

lbl :: forall l t. LabelPrx l -> t -> Label l t
{-# INLINE lbl #-}
lbl LabelPrx a = label @l a

data LabelPrx (l :: Symbol) = LabelPrx

instance (l ~ l') => IsLabel l (LabelPrx l') where
  fromLabel = LabelPrx

instance (t ~ t') => IsLabel l (t -> (Label l t')) where
  fromLabel = label @l
