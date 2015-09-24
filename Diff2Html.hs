{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Diff2Html where

import Data.Monoid
import qualified Data.Text as T

import Chunk
import Diff
import Lucid
import Style

chunksToHtml :: [Chunk] -> Html ()
chunksToHtml chunks =
    doctypehtml_ $ do
        head_ $ style_ [] styleSheet
        body_ $ table_ [class_ "diff"]$ foldMap chunkToRows chunks

chunkToRows :: Chunk -> Html ()
chunkToRows (Chunk files pos body) = do
    tr_ [class_ "header"] $ mapM_ (td_ . span_ [class_ "filename"] . toHtml . fileName) files
    tr_ $ mapM_ showTd pos
    mapM_ chunkBodyToRows body
  where
    showTd :: Show a => a -> Html ()
    showTd = cell [] . show

chunkBodyToRows :: ChunkBody -> Html ()
chunkBodyToRows (CContext lines) = mapM_ go lines
  where
    go :: T.Text -> Html ()
    go l = tr_ $ cell [] l <> cell [] l
chunkBodyToRows (CDiff (Diff dels adds)) =
    mapM_ (\(del,add) -> tr_ $ cell delAttrs del <> cell addAttrs add) (fillIn dels adds)
  where
    addAttrs = [class_ "addition"]
    delAttrs = [class_ "deletion"]

    fillIn :: [T.Text] -> [T.Text] -> [(T.Text, T.Text)]
    fillIn []     []     = []
    fillIn (x:xs) (y:ys) = (x,       y)       : fillIn xs ys
    fillIn (x:xs) []     = (x,       T.empty) : fillIn xs []
    fillIn []     (y:ys) = (T.empty, y)       : fillIn [] ys

cell :: ToHtml a => [Attribute] -> a -> Html ()
cell attrs d = td_ attrs $ pre_ (toHtml d)
