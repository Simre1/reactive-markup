module ReactiveMarkup.Elements.Theme where

import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (..))
import GHC.Generics (Generic)
import ReactiveMarkup.Elements.Dynamic
import ReactiveMarkup.Markup

data LightDarkTheme = LightTheme | DarkTheme deriving (Eq, Show, Generic, Enum, Bounded)

data GetLightDarkTheme f c e = GetLightDarkTheme
  { child :: Dynamic (f LightDarkTheme) -> Markup c (Either e (f LightDarkTheme)),
    initial :: f LightDarkTheme
  }

lightDarkThemeF :: (HasElement c (GetLightDarkTheme f c)) => f LightDarkTheme -> (Dynamic (f LightDarkTheme) -> Markup c (Either e (f LightDarkTheme))) -> Markup c e
lightDarkThemeF initial m = toMarkup $ GetLightDarkTheme m initial

lightDarkTheme :: forall c e. (HasElement c (GetLightDarkTheme Identity c)) => LightDarkTheme -> (Dynamic LightDarkTheme -> Markup c (Either e LightDarkTheme)) -> Markup c e
lightDarkTheme initial m = lightDarkThemeF (Identity initial) $ coerceMarkupEvent . m . coerce
