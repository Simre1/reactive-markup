{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
module ReactiveMarkup.Website where

import Control.Applicative (Const (getConst))
import Data.Foldable (forM_)
import Data.Maybe (fromMaybe)
import Data.Text
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as T (toStrict)
import GHC.Generics (Generic)
import ReactiveMarkup.Markup
import ReactiveMarkup.Runner.Html
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Blaze.Html qualified as H
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html5 qualified as H

data Page f c e = Page
  { name :: Text,
    route :: Maybe Text,
    header :: H.Html,
    markup :: f PageRoute -> Markup c e
  }
  deriving (Generic)

data Website f c e = Website
  { header :: H.Html,
    pages :: f (Page f c e)
  }

data PageRoute = PageRoute
  { name :: Text,
    route :: Text
  }
  deriving (Show, Eq, Ord, Generic)

generateStaticWebsite :: (Traversable f) => FilePath -> Website f StaticHtmlBlock e -> IO ()
generateStaticWebsite = generateWebsite getConst staticHtmlBlockContext

generateReactiveWebsite :: (Traversable f) => (e -> Text) -> FilePath -> Website f ReactiveHtmlBlock e -> IO ()
generateReactiveWebsite handle outPath website = do
  generateWebsite (runReactiveHtmlBuilder handle) reactiveHtmlBlockContext outPath 
      website {header = interactiveScript <> website.header}

generateWebsite :: forall f g e c. (Traversable f) => (g e -> H.Html) -> c g -> FilePath -> Website f c e -> IO ()
generateWebsite makeHtml context outPath website = do
  let pageRoutes = generatePageRoute <$> website.pages
      pages = generatePage pageRoutes <$> website.pages

  createDirectoryIfMissing True outPath

  forM_ pages $ \(route, html) ->
    T.writeFile (outPath </> unpack route.route) $ T.toStrict (renderHtml html)
  where
    generatePage :: f PageRoute -> Page f c e -> (PageRoute, H.Html)
    generatePage routes page =
      let pageHtml = makeHtml $ runMarkup context $ page.markup routes
          html = H.docTypeHtml $ H.html $ H.head (website.header <> page.header) <> H.body pageHtml
       in (generatePageRoute page, html)
    generatePageRoute :: Page f c e -> PageRoute
    generatePageRoute page =
      PageRoute
        { name = page.name,
          route = fromMaybe (replace " " "-" (toLower page.name)) page.route <> ".html"
        }
