module ReactiveMarkup.Elements.Dynamic where

import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics
import ReactiveMarkup.Markup

data Dynamic a = DynamicId Int | ConstDynamic a deriving (Show, Eq, Ord, Generic)

constDynamic :: a -> Dynamic a
constDynamic = ConstDynamic

data MapDynamic c e = forall a b.
  MapDynamic
  { dynamic :: Dynamic a,
    mapping :: a -> b,
    markup :: Dynamic b -> Markup c e
  }

mapDynamic :: (HasElement c (MapDynamic c)) => Dynamic a -> (a -> b) -> (Dynamic b -> Markup c e) -> Markup c e
mapDynamic dynamic mapping makeMarkup = toMarkup $ MapDynamic dynamic mapping makeMarkup

data SplitDynamic c e = forall a b.
  SplitDynamic
  { dynamic :: Dynamic (a, b),
    markup :: Dynamic a -> Dynamic b -> Markup c e
  }

splitDynamic :: (HasElement c (SplitDynamic c)) => Dynamic (a, b) -> (Dynamic a -> Dynamic b -> Markup c e) -> Markup c e
splitDynamic dynamic makeMarkup = toMarkup $ SplitDynamic dynamic makeMarkup

data GroupDynamic c e = forall a b.
  GroupDynamic
  { dynamicA :: Dynamic a,
    dynamicB :: Dynamic b,
    markup :: Dynamic (a, b) -> Markup c e
  }

groupDynamic :: (HasElement c (GroupDynamic c)) => Dynamic a -> Dynamic b -> (Dynamic (a, b) -> Markup c e) -> Markup c e
groupDynamic dynamicA dynamicB makeMarkup = toMarkup $ GroupDynamic dynamicA dynamicB makeMarkup

newtype FiniteState (a :: Type) = FiniteState Int deriving (Eq, Show, Generic, Ord)

makeFinite :: forall a. (Enum a, Bounded a) => a -> FiniteState a
makeFinite = FiniteState . (\x -> x - fromEnum (minBound :: a)) . fromEnum

fromFinite :: (Enum a) => FiniteState a -> a
fromFinite (FiniteState i) = toEnum i

data FiniteStateMarkup c e
  = forall a.
    FiniteStateMarkup
      [a]
      (a -> Markup c e)
      (Dynamic Int)

finiteStateMarkup :: (Enum a, Bounded a, HasElement c (FiniteStateMarkup c)) => Dynamic (FiniteState a) -> (a -> Markup c e) -> Markup c e
finiteStateMarkup dynamic makeMarkup =
  let states = [minBound .. maxBound]
   in toMarkup $ FiniteStateMarkup states makeMarkup (coerce dynamic)
