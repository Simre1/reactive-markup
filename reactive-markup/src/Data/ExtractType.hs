{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.ExtractType (Extract, extract, Nest, nest, unNest) where

import Data.Data (Proxy (..))
import Data.Kind (Type)
import GHC.Generics

data Direction = L | R | D | F | N | X

type family FindType (f :: Type -> Type) b :: Bool where
  FindType (Rec0 b) b = True
  FindType (Rec0 (Nest a)) b = FindType (Rep a) b
  FindType (M1 d meta i) b = FindType i b
  FindType (l :*: r) b = Or (FindType l b) (FindType r b)
  FindType _ _ = False

type family Or a b where
  Or True _ = True
  Or _ b = b

type family DecideDirection (l :: Bool) (r :: Bool) :: Direction where
  DecideDirection True _ = 'L
  DecideDirection _ True = 'R
  DecideDirection _ _ = 'D

type family FindDirection (f :: Type -> Type) b :: Direction where
  FindDirection (l :*: r) b = DecideDirection (FindType l b) (FindType r b)
  FindDirection (Rec0 b) b = F
  FindDirection (Rec0 (M1 _ _ _ _)) b = 'D
  FindDirection (Rec0 a) b = 'N
  FindDirection (M1 _ _ _) _ = 'D
  FindDirection _ _ = 'X

class ExtractType (d :: Direction) (f :: Type -> Type) b where
  extractType' :: Proxy d -> f () -> b

instance (ExtractType (FindDirection i b) i b) => ExtractType 'D (M1 d meta i) b where
  extractType' _ (M1 x) = extractType' (Proxy @(FindDirection i b)) x
  {-# INLINE extractType' #-}

instance (ExtractType (FindDirection l b) l b) => ExtractType L (l :*: r) b where
  extractType' _ (l :*: _) = extractType' (Proxy @(FindDirection l b)) l
  {-# INLINE extractType' #-}

instance (ExtractType (FindDirection r b) r b) => ExtractType 'R (l :*: r) b where
  extractType' _ (_ :*: r) = extractType' (Proxy @(FindDirection r b)) r
  {-# INLINE extractType' #-}

instance ExtractType F (Rec0 b) b where
  extractType' _ (K1 b) = b
  {-# INLINE extractType' #-}

instance (ExtractType (FindDirection i b) i b) => ExtractType 'D (Rec0 (i ())) b where
  extractType' _ (K1 x) = extractType' (Proxy @(FindDirection i b)) x
  {-# INLINE extractType' #-}

instance (ExtractType (FindDirection (Rep a) b) (Rep a) b) => ExtractType 'N (Rec0 (Nest a)) b where
  extractType' _ (K1 (Nest a)) = extractType' (Proxy @(FindDirection (Rep a) b)) a
  {-# INLINE extractType' #-}

type Extract a b = (Generic a, ExtractType (FindDirection (Rep a) b) (Rep a) b)

-- | Generically extract some value of type `b` from `a`. If there is no `b`, this will not typecheck.
-- If there are multiple `b`s, it will return the first one in depth-first top-down order.
extract :: forall a b. (Generic a, Extract a b) => a -> b
extract a =
  extractType'
    (Proxy @(FindDirection (Rep a) b))
    (from a :: Rep a ())
{-# INLINE extract #-}

-- | `extract` will not recurse to inner types even if those types implement `Generic`. If you want to recursively extract,
-- use the `Nest` type on those you want `extract` to search through.
newtype Nest a = Nest (Rep a ())

nest :: (Generic a) => a -> Nest a
nest a = Nest $ from a
{-# INLINE nest #-}

unNest :: Generic a => Nest a -> a 
unNest (Nest a) = to a
{-# INLINE unNest #-}

