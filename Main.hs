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

diffToHtml :: FilePath -> IO (Html ())
diffToHtml fname = do
    Right r <- parseOnly (many diff) <$> TIO.readFile fname
    return $ doctypehtml_ $ do
      head_ $ style_ [] styleSheet
      body_ $ table_ [class_ "diff"]$ foldMap chunkToRows $ toChunks r

main = return ()
