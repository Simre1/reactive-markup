module ReactiveMarkup.Runner.Markdown where

import Control.Applicative
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Builder.Linear
import GHC.Generics
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Paragraph
import ReactiveMarkup.Markup
import Prelude hiding (words)

data Markdown

runMarkdownMarkup :: Markup MarkdownBlock e -> Text
runMarkdownMarkup m = runBuilder $ getConst $ runMarkup markdownBlockContext m

data MarkdownParagraph f = MarkdownParagraph
  { words :: Element f Words,
    bold :: Element f (Bold MarkdownParagraph),
    italic :: Element f (Italic MarkdownParagraph),
    code :: Element f Code,
    empty :: Element f Empty,
    combine :: Element f (Combine MarkdownParagraph)
  }
  deriving (Generic)

data MarkdownBlock f = MarkdownBlock
  { codeBlock :: Element f CodeBlock,
    paragraph :: Element f (Paragraph MarkdownParagraph),
    list :: Element f (List MarkdownBlock),
    heading :: Element f (Heading MarkdownParagraph),
    quote :: Element f (Quote MarkdownBlock),
    bulletList :: Element f (BulletList MarkdownBlock),
    orderedList :: Element f (OrderedList MarkdownBlock),
    empty :: Element f Empty,
    combine :: Element f (Combine MarkdownBlock),
    markdownParagraph :: Nest (MarkdownParagraph f)
  }
  deriving (Generic)

markdownParagraphContext :: MarkdownParagraph (Const Builder)
markdownParagraphContext =
  MarkdownParagraph
    { words = makeElement $ \(Words txt) -> Const $ fromText txt,
      bold = makeElement $ \(Bold m) -> surround "**" $ f m,
      italic = makeElement $ \(Italic m) -> surround "*" $ f m,
      code = makeElement $ \(Code _ codeText) -> Const $ fromText $ surround "`" codeText,
      empty = makeElement $ \Empty -> mempty,
      combine = makeElement $ \(Combine a b) -> f a <> f b
    }
  where
    f = runMarkup markdownParagraphContext

surround :: (Monoid a) => a -> a -> a
surround symbol m = mconcat [symbol, m, symbol]

markdownBlockContext :: MarkdownBlock (Const Builder)
markdownBlockContext =
  MarkdownBlock
    { codeBlock = makeElement $ \(CodeBlock language codeText) ->
        Const $
          "```" <> fromText (fromMaybe "" language) <> "\n" <> fromText codeText <> "\n```",
      paragraph = makeElement $ \(Paragraph m) -> runP m,
      list = makeElement $ \(List ms) -> mconcat . intersperse "\n" $ runB <$> ms,
      heading = makeElement $ \(Heading level content) ->
        foldMap (Const . fromChar) (replicate level '#') <> " " <> runP content,
      quote = makeElement $ \(Quote m) -> "> " <> runB m,
      bulletList = makeElement $ \(BulletList items) -> mconcat . intersperse "\n" $ fmap ("- " <>) $ runB <$> items,
      orderedList = makeElement $ \(OrderedList items) -> mconcat . intersperse "\n" $ fmap ("1. " <>) $ runB <$> items,
      empty = makeElement $ \Empty -> mempty,
      combine = makeElement $ \(Combine a b) -> runB a <> runB b,
      markdownParagraph = nest markdownParagraphContext
    }
  where
    runB = runMarkup markdownBlockContext
    runP = runMarkup markdownParagraphContext
