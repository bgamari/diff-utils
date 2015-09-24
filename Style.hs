{-# LANGUAGE OverloadedStrings #-}

module Style where
import Prelude hiding ((**), span)
import Clay
import Data.Text.Lazy (Text)

styleSheet :: Text
styleSheet = render $ do
    table ? do
      borderCollapse collapse

    pre ? do
      margin nil nil nil nil

    ".diff" ** td ? do
      padding nil nil nil nil

    tr # ".header" |> td ? do
      paddingTop (px 10)
      backgroundColor "#eee"

    ".filename" ? do
      fontWeight bold
      fontSizeCustom large

    tr # ".diff" ? do
      span ? do
        whiteSpace preWrap
        fontFamily [] [monospace]

      ".add" ? backgroundColor "#d0ffd0"
      ".del" ? backgroundColor "#ffd0d0"
      ".add" |> ".change" ? backgroundColor "#a0ffa0"
      ".del" |> ".change" ? backgroundColor "#ffa0a0"
