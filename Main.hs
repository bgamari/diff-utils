{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text (parseOnly)
import Data.Maybe
import qualified Data.Text.IO as TIO
import Control.Applicative
import Options.Applicative
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

data Options = Options { patchFile :: FilePath
                       , outputFile :: Maybe FilePath
                       }

options :: Parser Options
options = Options <$> strArgument (help "Patch file")
                  <*> optional (option auto (help "HTML output"))

main :: IO ()
main = do
    opts <- execParser $ info (helper <*> options) mempty
    chunks <- parseChunks (patchFile opts)
    let out = fromMaybe (patchFile opts++".html") (outputFile opts)
    renderToFile out $ chunksToHtml chunks
