{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.TextInput where

import qualified Data.Text as T
import ReactiveMarkup.Elements.Basic
import ReactiveMarkup.Elements.Input
import ReactiveMarkup.Elements.Options
import ReactiveMarkup.Markup

textInputExample :: Markup '[List '[]] '[Label '[Text], DynamicState, List '[], TextInput '[TextChange], DynamicMarkup] e
textInputExample =
 list noOps
      ( (label (text "Write some Text"))
      +: (dynamicState
            ""
            (\_ t -> (Just t, Nothing))
            (\dynText ->
                list noOps $
                    textInput (onTextChange id)
                    +: [dynamicMarkup dynText (\t -> label $ text t)]
            ))
      +: empty
      )

empty :: [Markup '[] '[] e]
empty = []