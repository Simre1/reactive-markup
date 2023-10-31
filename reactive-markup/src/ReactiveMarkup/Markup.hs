{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module ReactiveMarkup.Markup
  ( Element,
    makeElement,
    HasElement,
    HasElements,
    coerceMarkupEvent,
    ET.Nest,
    ET.nest,
    ET.unNest,
    Markup,
    runMarkup,
    Target,
    toMarkup,
    Empty (..),
    emptyMarkup,
    Combine (..),
    )
where

import Data.Coerce (Coercible)
import Data.ExtractType qualified as ET
import Data.Kind
import Unsafe.Coerce (unsafeCoerce)

newtype Markup (c :: (Type -> Type) -> Type) (e :: Type) = Markup (forall f. c f -> f e)

newtype Element (f :: Type -> Type) (element :: Type -> Type) = Element (forall x. element x -> f x)

makeElement :: (forall e. element e -> f e) -> Element f element
makeElement = Element

data SearchF a

type family HasElement c element where
  HasElement c element = ET.Extract (c SearchF) (Element SearchF element)

type family HasElements t (elements :: [Type -> Type]) :: Constraint where
  HasElements t '[] = ()
  HasElements t '[x] = HasElement t x
  HasElements t (x : xs) = (HasElement t x, HasElements t xs)

toMarkup :: forall element c e. (HasElement c element) => element e -> Markup c e
toMarkup element = Markup $ \context ->
  let (Element f) = ET.extract @(c SearchF) @(Element SearchF element) $ unsafeCoerce context
   in unsafeCoerce $ f element

type family Target a :: Type -> Type

runMarkup :: c f -> forall e. Markup c e -> f e
runMarkup c (Markup f) = f c

coerceMarkupEvent :: (Coercible e1 e2) => Markup c e1 -> Markup c e2
coerceMarkupEvent (Markup f) = Markup $ unsafeCoerce . f

data Combine c e = Combine (Markup c e) (Markup c e)

combine :: (HasElement c (Combine c)) => Markup c e -> Markup c e -> Markup c e
combine m1 m2 = toMarkup $ Combine m1 m2

data Empty e = Empty

emptyMarkup :: (HasElement c Empty) => Markup c e
emptyMarkup = toMarkup Empty

instance (HasElement c (Combine c)) => Semigroup (Markup c e) where
  (<>) = combine

instance (HasElement c (Combine c), HasElement c Empty) => Monoid (Markup c e) where
  mempty = emptyMarkup
