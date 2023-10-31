module ReactiveMarkup.Elements.Interactive where

import Data.Coerce (coerce)
import Data.Functor.Identity
import GHC.Generics
import ReactiveMarkup.Elements.Dynamic (Dynamic)
import ReactiveMarkup.Markup

data Button c e = Button
  { event :: Maybe e,
    content :: Markup c e
  }
  deriving (Generic)

button :: forall paraC c e. (HasElement c (Button paraC)) => Maybe e -> Markup paraC e -> Markup c e
button event content = toMarkup $ Button event content

data SettableState f c e = forall a.
  SettableState
  { makeMarkup :: Dynamic (f a) -> Markup c (Either e (f a)),
    initialState :: f a
  }

settableStateF :: (HasElement c (SettableState f c)) => f a -> (Dynamic (f a) -> Markup c (Either e (f a))) -> Markup c e
settableStateF initial make = toMarkup $ SettableState make initial

settableState :: forall a c e. (HasElement c (SettableState Identity c)) => a -> (Dynamic a -> Markup c (Either e a)) -> Markup c e
settableState initial make = toMarkup $ SettableState @Identity (coerceEvent . make . coerce) (coerce initial)
  where
    coerceEvent :: Markup c (Either e a) -> Markup c (Either e (Identity a))
    coerceEvent = coerceMarkupEvent

data DuplicateEvent c e where
  DuplicateEvent :: Markup c e -> DuplicateEvent c (Either e e)

duplicateEvent :: (HasElement c (DuplicateEvent c)) => Markup c e -> Markup c (Either e e)
duplicateEvent = toMarkup . DuplicateEvent

data LeftEvent c e where
  LeftEvent :: Markup c (Either e x) -> LeftEvent c e

leftEvent :: (HasElement c (LeftEvent c)) => Markup c (Either e x) -> Markup c e
leftEvent = toMarkup . LeftEvent

data RightEvent c e where
  RightEvent :: Markup c (Either x e) -> RightEvent c e

rightEvent :: (HasElement c (RightEvent c)) => Markup c (Either x e) -> Markup c e
rightEvent = toMarkup . RightEvent

data IgnoreEvent c e where
  IgnoreEvent :: Markup c x -> IgnoreEvent c e

ignoreEvent :: (HasElement c (IgnoreEvent c)) => Markup c x -> Markup c e
ignoreEvent = toMarkup . IgnoreEvent

data MapEvent c e where
  MapEvent :: (e1 -> e2) -> Markup c e1 -> MapEvent c e2

mapEvent :: (HasElement c (MapEvent c)) => (e1 -> e2) -> Markup c e1 -> Markup c e2
mapEvent f = toMarkup . MapEvent f
