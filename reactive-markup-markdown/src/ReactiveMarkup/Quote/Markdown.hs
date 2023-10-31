{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module ReactiveMarkup.Quote.Markdown (mdm, parseMarkdown, mdpm, parseMarkdownParagraph, MarkdownBlockParagraph) where

import Commonmark as CM
import Commonmark.Inlines
import Data.Bifunctor (Bifunctor (..))
import Data.Either (fromRight)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Text as T (Text, lines, null, pack, stripStart, unlines, unpack)
import GHC.Generics
import Language.Haskell.TH as TH hiding (Type)
import Language.Haskell.TH.Quote
import ReactiveMarkup.Elements.Basic as RM
import ReactiveMarkup.Elements.Paragraph as RM
import ReactiveMarkup.Markup as RM
import ReactiveMarkup.Runner.Markdown


mdm :: QuasiQuoter
mdm = QuasiQuoter {quoteExp = parseMarkdownQuote}

mdpm :: QuasiQuoter
mdpm = QuasiQuoter {quoteExp = parseMarkdownParagraphQuote}

parseMarkdownQuote :: String -> Q Exp
parseMarkdownQuote tx = case parseMarkdown @MarkdownBlock (pack tx) of
  Left err -> fail $ unpack err
  Right _ -> [|fromRight (error "no markdown parse") $ parseMarkdown (pack tx)|]

parseMarkdownParagraphQuote :: String -> Q Exp
parseMarkdownParagraphQuote tx = case parseMarkdownParagraph @MarkdownParagraph (pack tx) of
  Left err -> fail $ unpack err
  Right _ -> [|fromRight (error "no markdown parse") $ parseMarkdownParagraph (pack tx)|]

parseMarkdown :: (BlockElements (MarkdownBlockParagraph c) c, InlineElements (MarkdownBlockParagraph c)) => Text -> Either Text (Markup c e)
parseMarkdown tx =
  bimap
    (pack . show)
    (\(MarkdownMarkup mm) -> mm)
    $ commonmark "QQ"
    $ T.unlines
    $ T.stripStart
      <$> T.lines tx

parseMarkdownParagraph :: (InlineElements paraC) => Text -> Either Text (Markup paraC e)
parseMarkdownParagraph tx =
  bimap
    (pack . show)
    (\(FakeMarkdownBlock (MarkdownMarkup mm)) -> mm)
    $ runIdentity
    $ commonmarkWith spec "QQ"
    $ T.unlines
    $ T.stripStart
      <$> T.lines tx
  where
    spec =
      SyntaxSpec
        { syntaxBlockSpecs = [],
          syntaxBracketedSpecs = defaultBracketedSpecs,
          syntaxFormattingSpecs = defaultFormattingSpecs,
          syntaxInlineParsers = [defaultInlineParser],
          syntaxFinalParsers = [],
          syntaxAttributeParsers = []
        }

type family MarkdownBlockParagraph (block :: (Type -> Type) -> Type) :: (Type -> Type) -> Type

type instance MarkdownBlockParagraph MarkdownBlock = MarkdownParagraph

newtype MarkdownMarkup c e = MarkdownMarkup (Markup c e)

instance (HasElement c (Combine c)) => Semigroup (MarkdownMarkup c e) where
  (MarkdownMarkup a) <> (MarkdownMarkup b) = MarkdownMarkup $ a <> b

instance (HasElement c Empty, HasElement c (Combine c)) => Monoid (MarkdownMarkup c e) where
  mempty = MarkdownMarkup mempty

instance Show (MarkdownMarkup c e) where
  show _ = "Not implemented"

instance Rangeable (MarkdownMarkup c e) where
  ranged _ = id

instance HasAttributes (MarkdownMarkup c e) where
  addAttributes _ = id

newtype FakeMarkdownBlock paraC e = FakeMarkdownBlock (MarkdownMarkup paraC e) deriving (Generic, HasAttributes, Rangeable, Show)

deriving instance (HasElement paraC (Combine paraC)) => Semigroup (FakeMarkdownBlock paraC e)

deriving instance (HasElements paraC [Combine paraC, Empty]) => Monoid (FakeMarkdownBlock paraC e)

instance (InlineElements paraC) => IsBlock (MarkdownMarkup paraC e) (FakeMarkdownBlock paraC e) where
  paragraph = FakeMarkdownBlock
  plain = FakeMarkdownBlock
  thematicBreak = undefined
  blockQuote = undefined
  codeBlock = undefined
  heading = undefined
  rawBlock = undefined
  referenceLinkDefinition = undefined
  list = undefined


instance (InlineElements paraC, BlockElements paraC c, MarkdownBlockParagraph c ~ paraC) => IsBlock (MarkdownMarkup paraC e) (MarkdownMarkup c e) where
  paragraph (MarkdownMarkup mm) = MarkdownMarkup $ RM.paragraph mm
  plain (MarkdownMarkup mm) = MarkdownMarkup $ RM.paragraph mm
  thematicBreak = mempty
  blockQuote (MarkdownMarkup mm) = MarkdownMarkup $ quote mm
  codeBlock format codeText =
    MarkdownMarkup $
      RM.codeBlock
        (if T.null format then Nothing else Just format)
        codeText
  heading level (MarkdownMarkup mm) = MarkdownMarkup $ RM.heading level mm
  rawBlock (Format format) codeText =
    MarkdownMarkup $
      RM.codeBlock
        (if T.null format then Nothing else Just format)
        codeText

  referenceLinkDefinition _ _ = MarkdownMarkup mempty
  list listType _listSpacing children = MarkdownMarkup $ case listType of
    CM.BulletList _ -> bulletList $ fmap (\(MarkdownMarkup m) -> m) children
    CM.OrderedList {} -> orderedList $ fmap (\(MarkdownMarkup m) -> m) children

instance (InlineElements c) => IsInline (MarkdownMarkup c e) where
  lineBreak = MarkdownMarkup mempty
  softBreak = MarkdownMarkup mempty
  str x = MarkdownMarkup $ text x
  entity _ = mempty
  escapedChar x = MarkdownMarkup $ text (pack [x])
  emph (MarkdownMarkup mm) = MarkdownMarkup $ italic mm
  strong (MarkdownMarkup mm) = MarkdownMarkup $ bold mm
  link _ _ = id
  image _src _title _alt = MarkdownMarkup mempty
  code codeText =
    MarkdownMarkup $
      RM.code
        Nothing
        codeText
  rawInline (Format format) codeText =
    MarkdownMarkup $
      RM.code
        (if T.null format then Nothing else Just format)
        codeText

type BlockElements paraC c = (HasElements c '[RM.Paragraph paraC, RM.Quote c, RM.CodeBlock, RM.Heading paraC, RM.BulletList c, RM.OrderedList c, Combine c, Empty])

type InlineElements c = (HasElements c '[RM.Words, RM.Italic c, RM.Bold c, RM.Code, RM.Empty, RM.Combine c])
