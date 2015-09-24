{-# LANGUAGE OverloadedStrings #-}

module Style where
import Prelude hiding ((**))
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

    ".addition" ? do
      backgroundColor "#d0ffd0"

    ".deletion" ? do
      backgroundColor "#ffd0d0"
