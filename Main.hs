{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import qualified Data.Text.IO as TIO
import Control.Applicative
import Lucid

import Diff2Html
import Chunk
import Diff
import Style

parseChunks :: FilePath -> IO [Chunk]
parseChunks fname = do
    Right r <- parseOnly (many diff) <$> TIO.readFile fname
    return (toChunks r)

chunksToHtml :: [Chunk] -> Html ()
chunksToHtml chunks =
    doctypehtml_ $ do
        head_ $ style_ [] styleSheet
        body_ $ chunksToTable chunks

main = return ()
