module ReactiveMarkup.Elements.Basic where

import Data.Text (Text, pack)
import ReactiveMarkup.Markup

newtype Words e = Words Text

text :: (HasElement c Words) => Text -> Markup c e
text = toMarkup . Words

string :: (HasElement c Words) => String -> Markup c e
string = text . pack

newtype List c e = List [Markup c e]

list :: (HasElement c (List c)) => [Markup c e] -> Markup c e
list = toMarkup . List

listC :: (HasElement c (List ic)) => [Markup ic e] -> Markup c e
listC = toMarkup . List

data Label c e = Label Text (Markup c e)

label :: (HasElement c (Label c)) => Text -> Markup c e -> Markup c e
label lbl m = toMarkup $ Label lbl m
