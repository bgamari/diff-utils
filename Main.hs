{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
import qualified Data.Text.IO as TIO
import Control.Applicative
import Lucid

import Diff2Html
import Chunk
import Diff

parseChunks :: FilePath -> IO [Chunk]
parseChunks fname = do
    Right r <- parseOnly (many diff) <$> TIO.readFile fname
    return (toChunks r)

main = return ()
