module ReactiveMarkup.Elements.Paragraph where

import Data.Text
import ReactiveMarkup.Markup
import Data.Kind (Type)

newtype Bold c e = Bold (Markup c e)

bold :: (HasElement c (Bold c)) => Markup c e -> Markup c e
bold = toMarkup . Bold

newtype Italic c e = Italic (Markup c e)

italic :: (HasElement c (Italic c)) => Markup c e -> Markup c e
italic = toMarkup . Italic

data Code e = Code (Maybe Text) Text

code :: (HasElement c Code) => Maybe Text -> Text -> Markup c e
code language codeText = toMarkup $ Code language codeText

data CodeBlock (e :: Type) = CodeBlock (Maybe Text) Text

codeBlock :: (HasElement c CodeBlock) => Maybe Text -> Text -> Markup c e
codeBlock language codeText = toMarkup $ CodeBlock language codeText

data Image e = Image {source :: Text, title :: Text}

image :: (HasElement c Image) => Text -> Text -> Markup c e
image source title = toMarkup $ Image source title

data Heading c e = Heading {level :: Int, heading :: Markup c e}

heading :: (HasElement c (Heading paraC)) => Int -> Markup paraC e -> Markup c e
heading lvl = toMarkup . Heading lvl

newtype Paragraph c e = Paragraph (Markup c e)

paragraph :: (HasElement c (Paragraph paraC)) => Markup paraC e -> Markup c e
paragraph = toMarkup . Paragraph

newtype Quote c e = Quote (Markup c e)

quote :: (HasElement c (Quote c)) => Markup c e -> Markup c e
quote = toMarkup . Quote

newtype BulletList c e = BulletList [Markup c e]

bulletList :: (HasElement c (BulletList c)) => [Markup c e] -> Markup c e
bulletList = toMarkup . BulletList

newtype OrderedList c e = OrderedList [Markup c e]

orderedList :: (HasElement c (OrderedList c)) => [Markup c e] -> Markup c e
orderedList = toMarkup . OrderedList
