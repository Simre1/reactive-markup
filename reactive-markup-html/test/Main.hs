{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Paragraph
import ReactiveMarkup.Markup
import ReactiveMarkup.Quote.Markdown (mdm)
import ReactiveMarkup.Runner.Html
import Test.Tasty
import Test.Tasty.HUnit
import Text.Blaze.Html.Renderer.Text (renderHtml)

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite =
  testGroup
    "Test suite"
    [ testRenderParagraph,
      testRenderHeading,
      testRenderList,
      testRenderCodeBlock,
      testRenderDocument
    ]

testRenderParagraph :: TestTree
testRenderParagraph = testCase "Render paragraph element" $ do
  let markup = paragraph $ txt <*> pure "Hello, World!"
  let expected = "<p>Hello, World!</p>"
  html <-
    either (fail . show) (pure . runReactiveHtmlNoEvent) (runMarkup staticHtmlRunner markup)
  assertEqual "Paragraph" expected (renderHtml html)

testRenderHeading :: TestTree
testRenderHeading = testCase "Render heading element" $ do
  let markup = heading (txt <*> pure "Hello, World!") <*> pure 1
  let expected = "<h1>Hello, World!</h1>"
  html <-
    either (fail . show) (pure . runReactiveHtmlNoEvent) (runMarkup staticHtmlRunner markup)
  assertEqual "Heading" expected (renderHtml html)

testRenderList :: TestTree
testRenderList = testCase "Render list element" $ do
  let markup = bulletList $ replicate 3 <$> (txt <*> pure "Item")
  let expected = "<ul><li>Item</li><li>Item</li><li>Item</li></ul>"
  html <-
    either (fail . show) (pure . runReactiveHtmlNoEvent) (runMarkup staticHtmlRunner markup)
  assertEqual "List" expected (renderHtml html)

testRenderCodeBlock :: TestTree
testRenderCodeBlock = testCase "Render code block element" $ do
  let markup = codeBlock <*> pure Nothing <*> pure "let x = 10"
  let expected = "<code>let x = 10</code>"
  html <-
    either (fail . show) (pure . runReactiveHtmlNoEvent) (runMarkup staticHtmlRunner markup)
  assertEqual "Code Block" expected (renderHtml html)

testRenderDocument :: TestTree
testRenderDocument = testCase "Render document" $ do
  let markup =
        [mdm| 
          # Some title

          Some paragraph

          > Some quote

          - List1
          - List2
          |]
  let expected = "<h1>Some title</h1><p>Some paragraph</p><blockquote><p>Some quote</p></blockquote><ul><li><p>List1</p></li><li><p>List2</p></li></ul>"
  html <-
    either (fail . show) (pure . runReactiveHtmlNoEvent) (runMarkup staticHtmlRunner markup)
  assertEqual "Document" expected (renderHtml html)


