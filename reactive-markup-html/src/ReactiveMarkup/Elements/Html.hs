{-# LANGUAGE ApplicativeDo #-}

module ReactiveMarkup.Elements.Html where

import ReactiveMarkup.Markup
import Text.Blaze.Html5 qualified as H

data Html c e = Html
  { child :: Markup c e,
    html :: H.Html -> H.Html
  }

html :: HasElement c (Html c) => (H.Html -> H.Html) -> Markup c e -> Markup c e
html f m = toMarkup $ Html m f

justHtml :: HasElements c [Html c, Empty] => H.Html -> Markup c e
justHtml h = html (const h) emptyMarkup
