{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}

module ReactiveMarkup.Runner.Html where

import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Coerce (coerce)
import Data.Functor.Const
import Data.String.Interpolate (__i)
import Data.Text (Text)
import GHC.Generics (Generic)
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Dynamic
import ReactiveMarkup.Elements.Html
import ReactiveMarkup.Elements.Interactive
import ReactiveMarkup.Elements.Paragraph
import ReactiveMarkup.Elements.Theme
import ReactiveMarkup.Markup
import Text.Blaze.Html qualified as H
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as H hiding (span)

import ReactiveMarkup.Quote.Markdown (MarkdownBlockParagraph)

type instance MarkdownBlockParagraph StaticHtmlBlock = StaticHtmlParagraph

type instance MarkdownBlockParagraph ReactiveHtmlBlock = ReactiveHtmlParagraph

runReactiveHtmlBuilder :: (e -> Text) -> ReactiveHtmlBuilder e -> H.Html
runReactiveHtmlBuilder handle (ReactiveHtmlBuilder rh) =
  evalState (runReaderT rh handle) 0

data StaticHtmlParagraph f = StaticHtmlParagraph
  { words :: Element f Words,
    bold :: Element f (Bold StaticHtmlParagraph),
    italic :: Element f (Italic StaticHtmlParagraph),
    code :: Element f Code,
    empty :: Element f Empty,
    combine :: Element f (Combine StaticHtmlParagraph),
    html :: Element f (Html StaticHtmlParagraph)
  }
  deriving (Generic)

data StaticHtmlBlock f = StaticHtmlBlock
  { codeBlock :: Element f CodeBlock,
    aParagraph :: Element f (Paragraph StaticHtmlParagraph),
    list :: Element f (List StaticHtmlBlock),
    heading :: Element f (Heading StaticHtmlParagraph),
    quote :: Element f (Quote StaticHtmlBlock),
    bulletList :: Element f (BulletList StaticHtmlBlock),
    orderedList :: Element f (OrderedList StaticHtmlBlock),
    empty :: Element f Empty,
    combine :: Element f (Combine StaticHtmlBlock),
    html :: Element f (Html StaticHtmlBlock),
    paragraph :: Nest (StaticHtmlParagraph f)
  }
  deriving (Generic)

cmap :: (a -> b) -> Const a x -> Const b x
cmap f (Const a) = Const (f a)

staticHtmlParagraphContext :: StaticHtmlParagraph (Const H.Html)
staticHtmlParagraphContext =
  StaticHtmlParagraph
    { words = makeElement $ \(Words txt) -> Const $ H.toHtml txt,
      bold = makeElement $ \(Bold m) -> cmap H.b $ runP m,
      italic = makeElement $ \(Italic m) -> cmap H.i $ runP m,
      code = makeElement $ \(Code _ codeText) -> Const $ H.code $ H.toHtml codeText,
      empty = makeElement $ \Empty -> mempty,
      combine = makeElement $ \(Combine a b) -> runP a <> runP b,
      html = makeElement $ \(Html child f) -> cmap f $ runP child
    }
  where
    runP = runMarkup staticHtmlParagraphContext

staticHtmlBlockContext :: StaticHtmlBlock (Const H.Html)
staticHtmlBlockContext =
  StaticHtmlBlock
    { codeBlock = makeElement $ \(CodeBlock _language codeText) -> Const $ H.code $ H.toHtml codeText,
      aParagraph = makeElement $ \(Paragraph m) -> runP m,
      list = makeElement $ \(List ms) -> mconcat $ runB <$> ms,
      heading = makeElement $ \(Heading level content) ->
        let tag = case level of
              0 -> H.h1
              1 -> H.h2
              2 -> H.h3
              3 -> H.h4
              4 -> H.h5
              _ -> H.h6
         in cmap tag $ runP content,
      quote = makeElement $ \(Quote m) -> cmap H.blockquote $ runB m,
      bulletList = makeElement $ \(BulletList items) -> cmap H.ul $ mconcat $ cmap H.li . runB <$> items,
      orderedList = makeElement $ \(OrderedList items) -> cmap H.ol $ mconcat $ cmap H.li . runB <$> items,
      empty = makeElement $ \Empty -> mempty,
      combine = makeElement $ \(Combine a b) -> runB a <> runB b,
      paragraph = nest staticHtmlParagraphContext,
      html = makeElement $ \(Html child f) -> cmap f $ runB child
    }
  where
    runB = runMarkup staticHtmlBlockContext
    runP = runMarkup staticHtmlParagraphContext

data ReactiveHtmlParagraph f = ReactiveHtmlParagraph
  { words :: Element f Words,
    bold :: Element f (Bold ReactiveHtmlParagraph),
    italic :: Element f (Italic ReactiveHtmlParagraph),
    code :: Element f Code,
    empty :: Element f Empty,
    combine :: Element f (Combine ReactiveHtmlParagraph),
    interactive :: Nest (InteractiveFunctionality ReactiveHtmlParagraph f),
    html :: Element f (Html ReactiveHtmlParagraph)
  }
  deriving (Generic)

data ReactiveHtmlBlock f = ReactiveHtmlBlock
  { codeBlock :: Element f CodeBlock,
    paragraph :: Element f (Paragraph ReactiveHtmlParagraph),
    list :: Element f (List ReactiveHtmlBlock),
    heading :: Element f (Heading ReactiveHtmlParagraph),
    quote :: Element f (Quote ReactiveHtmlBlock),
    bulletList :: Element f (BulletList ReactiveHtmlBlock),
    orderedList :: Element f (OrderedList ReactiveHtmlBlock),
    empty :: Element f Empty,
    combine :: Element f (Combine ReactiveHtmlBlock),
    html :: Element f (Html ReactiveHtmlBlock),
    interactive :: Nest (InteractiveFunctionality ReactiveHtmlBlock f),
    reactiveHtmlParagraph :: Nest (ReactiveHtmlParagraph f)
  }
  deriving (Generic)

data InteractiveFunctionality c f = InteractiveFunctionality
  { button :: Element f (Button ReactiveHtmlParagraph),
    finiteStateMarkup :: Element f (FiniteStateMarkup c),
    settableState :: Element f (SettableState FiniteState c),
    duplicateEvent :: Element f (DuplicateEvent c),
    rightEvent :: Element f (RightEvent c),
    leftEvent :: Element f (LeftEvent c),
    ignoreEvent :: Element f (IgnoreEvent c),
    lightDarkTheme :: Element f (GetLightDarkTheme FiniteState c)
  } deriving Generic

reactiveHtmlParagraphContext :: ReactiveHtmlParagraph ReactiveHtmlBuilder
reactiveHtmlParagraphContext =
  ReactiveHtmlParagraph
    { words = makeElement $ \(Words txt) -> pureHtml $ H.toHtml txt,
      bold = makeElement $ \(Bold m) -> rhMap H.b $ runP m,
      italic = makeElement $ \(Italic m) -> rhMap H.i $ runP m,
      code = makeElement $ \(Code _ codeText) -> pureHtml $ H.code $ H.toHtml codeText,
      empty = makeElement $ \Empty -> mempty,
      combine = makeElement $ \(Combine a b) -> runP a <> runP b,
      interactive = nest $ interactiveFunctionality reactiveHtmlParagraphContext,
      html = makeElement $ \(Html child f) -> rhMap f $ runP child
    }
  where
    runP = runMarkup reactiveHtmlParagraphContext

reactiveHtmlBlockContext :: ReactiveHtmlBlock ReactiveHtmlBuilder
reactiveHtmlBlockContext =
  ReactiveHtmlBlock
    { codeBlock = makeElement $ \(CodeBlock _language codeText) -> pureHtml $ H.code $ H.toHtml codeText,
      paragraph = makeElement $ \(Paragraph m) -> runP m,
      list = makeElement $ \(List ms) -> mconcat $ runB <$> ms,
      heading = makeElement $ \(Heading level content) ->
        let tag = case level of
              0 -> H.h1
              1 -> H.h2
              2 -> H.h3
              3 -> H.h4
              4 -> H.h5
              _ -> H.h6
         in rhMap tag $ runP content,
      quote = makeElement $ \(Quote m) -> rhMap H.blockquote $ runB m,
      bulletList = makeElement $ \(BulletList items) -> rhMap H.ul $ mconcat $ rhMap H.li . runB <$> items,
      orderedList = makeElement $ \(OrderedList items) -> rhMap H.ol $ mconcat $ rhMap H.li . runB <$> items,
      empty = makeElement $ \Empty -> mempty,
      combine = makeElement $ \(Combine a b) -> runB a <> runB b,
      reactiveHtmlParagraph = nest reactiveHtmlParagraphContext,
      interactive = nest $ interactiveFunctionality reactiveHtmlBlockContext,
      html = makeElement $ \(Html child f) -> rhMap f $ runB child
    }
  where
    runB = runMarkup reactiveHtmlBlockContext
    runP = runMarkup reactiveHtmlParagraphContext

newtype ReactiveHtmlBuilder e = ReactiveHtmlBuilder {reactiveHtml :: ReaderT (e -> Text) (State Int) H.Html}
  deriving (Generic)

instance Semigroup (ReactiveHtmlBuilder e) where
  (ReactiveHtmlBuilder r1) <> (ReactiveHtmlBuilder r2) = ReactiveHtmlBuilder $ (<>) <$> r1 <*> r2

instance Monoid (ReactiveHtmlBuilder e) where
  mempty = ReactiveHtmlBuilder $ lift $ lift mempty

pureHtml :: H.Html -> ReactiveHtmlBuilder e
pureHtml = ReactiveHtmlBuilder . pure

rhMap :: (H.Html -> H.Html) -> ReactiveHtmlBuilder e -> ReactiveHtmlBuilder e
rhMap f (ReactiveHtmlBuilder rhb) = ReactiveHtmlBuilder $ f <$> rhb

interactiveFunctionality :: forall c. c ReactiveHtmlBuilder -> InteractiveFunctionality c ReactiveHtmlBuilder
interactiveFunctionality context =
  InteractiveFunctionality
    { button = makeElement $ \(Button event content) -> ReactiveHtmlBuilder $ do
        htmlContent <- coerce $ runP content
        handle <- ask
        pure $
          H.button H.! xClick [__i|#{maybe "void(0);" handle event}|] $
            htmlContent,
      finiteStateMarkup = makeElement $ \(FiniteStateMarkup states make stateIndex) -> ReactiveHtmlBuilder $ do
        let statefulMarkups = coerce . runC . make <$> states
        htmls <- sequenceA statefulMarkups
        let switchableHtmls = makeSwitchableHtml stateIndex <$> zip [0 ..] htmls
        pure $ mconcat switchableHtmls,
      settableState = makeElement $ \(SettableState makeMarkup initialFiniteState) -> ReactiveHtmlBuilder $ do
        newDynId <- lift newId
        let dynamic = DynamicId newDynId
            FiniteState initialState = initialFiniteState
        childHtml <-
          typeChangingLocal
            ( \passThrough -> \case
                Left p -> passThrough p
                Right (FiniteState e) -> [__i|dyn#{newDynId} = #{e};|]
            )
            $ coerce
            $ runC
            $ makeMarkup dynamic
        pure $
          H.div H.! xData [__i|{dyn#{show newDynId}: #{initialState}}|] $
            childHtml,
      duplicateEvent = makeElement $ \(DuplicateEvent m) ->
        ReactiveHtmlBuilder $ typeChangingLocal (\h e -> h (Left e) <> h (Right e)) $ coerce $ runC m,
      rightEvent = makeElement $ \(RightEvent m) ->
        ReactiveHtmlBuilder $ typeChangingLocal (either jsDoNothing) (runC m).reactiveHtml,
      leftEvent = makeElement $ \(LeftEvent m) ->
        ReactiveHtmlBuilder $ typeChangingLocal (`either` jsDoNothing) (runC m).reactiveHtml,
      ignoreEvent = makeElement $ \(IgnoreEvent m) ->
        ReactiveHtmlBuilder $ typeChangingLocal (const jsDoNothing) (runC m).reactiveHtml,
      lightDarkTheme = makeElement $ \(GetLightDarkTheme makeMarkup (FiniteState initial)) -> ReactiveHtmlBuilder $ do
        newDynId <- lift newId
        let dynamic = DynamicId newDynId
        childHtml <-
          typeChangingLocal
            ( \passThrough -> \case
                Left p -> passThrough p
                Right (FiniteState lightOrDark) -> [__i|dyn#{newDynId} = #{lightOrDark};|]
            )
            $ coerce
            $ runC
            $ makeMarkup dynamic
        pure $
          H.div childHtml
            H.! xData [__i|{dyn#{show newDynId}: detectTheme(#{initial})}|]
            H.! xBindClass [__i|dyn#{show newDynId} === #{fromEnum LightTheme} ? "light-mode" : "dark-mode"|]
    }
  where
    runP = runMarkup reactiveHtmlParagraphContext
    runC :: Markup c e -> ReactiveHtmlBuilder e
    runC = runMarkup context
    makeSwitchableHtml :: Dynamic Int -> (Int, H.Html) -> H.Html
    makeSwitchableHtml stateIndex (index, content) =
      case stateIndex of
        DynamicId dynId ->
          H.div
            H.! xShow
              [__i|dyn#{dynId} == #{index}|]
            H.! H.class_ "fs"
            $ content
        ConstDynamic constIndex -> if index == constIndex then content else mempty

jsDoNothing :: e -> Text
jsDoNothing _ = [__i|void(0);|]

xData :: H.AttributeValue -> H.Attribute
xData = H.customAttribute "x-data"

xShow :: H.AttributeValue -> H.Attribute
xShow = H.customAttribute "x-show"

xClick :: H.AttributeValue -> H.Attribute
xClick = H.customAttribute "x-on:click"

xBindClass :: H.AttributeValue -> H.Attribute
xBindClass = H.customAttribute "x-bind:class"

newId :: State Int Int
newId = do
  x <- get
  put (succ x)
  pure x

typeChangingLocal :: (Monad m) => (r2 -> r1) -> ReaderT r1 m a -> ReaderT r2 m a
typeChangingLocal f m = do
  e <- ask
  lift $ runReaderT m (f e)

interactiveScript :: H.Html
interactiveScript = (H.script "" H.! H.src [__i|https://unpkg.com/alpinejs|] H.! H.defer "") <> H.script (H.toHtml detectThemeScript)

detectThemeScript :: Text
detectThemeScript =
  [__i|
  function detectTheme(defaultTheme){
    var theme = defaultTheme;

    //local storage is used to override OS theme settings
    if(localStorage.getItem("theme")){
        if(localStorage.getItem("theme") == "dark"){
            theme = #{fromEnum DarkTheme};
        } else {
            theme = #{fromEnum LightTheme}
        }
    } else if(!window.matchMedia) {
        //matchMedia method not supported
    } else if(window.matchMedia("(prefers-color-scheme: dark)").matches) {
        //OS theme setting detected as dark
        theme = #{fromEnum DarkTheme};
    } else if (window.matchMedia("(prefers-color-scheme: light)").matches) {
        theme = #{fromEnum LightTheme}
    }
    return theme;
  }
  |]
