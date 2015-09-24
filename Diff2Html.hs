{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Diff2Html where

import Data.Monoid

import Chunk
import Diff
import Lucid
import qualified Data.Text as T

chunkToRows :: Chunk -> Html ()
chunkToRows (Chunk files pos body) = do
    tr_ $ mapM_ (cell [] . fileName) files
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
    mapM_ (\(del,add) -> tr_ $ diffCell delAttrs del <> diffCell addAttrs add) (fillIn dels adds)
  where
    diffCell attrs l
      | T.null l  = cell [] (""::String)
      | otherwise = cell attrs l
    addAttrs = [class_ "addition"]
    delAttrs = [class_ "deletion"]

    fillIn :: [T.Text] -> [T.Text] -> [(T.Text, T.Text)]
    fillIn []     []     = []
    fillIn (x:xs) (y:ys) = (x,       y)       : fillIn xs ys
    fillIn (x:xs) []     = (x,       T.empty) : fillIn xs []
    fillIn []     (y:ys) = (T.empty, y)       : fillIn [] ys

cell :: ToHtml a => [Attribute] -> a -> Html ()
cell attrs d = td_ attrs $ pre_ (toHtml d)
