{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Diff2Html where

import Data.Foldable
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Algorithm.Patience as P
import Lucid

import Chunk
import Diff
import Style
import Utils (breakWith)

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
    go l = tr_ [class_ "diff"] $ cell [] l <> cell [] l
chunkBodyToRows (CDiff (Diff dels adds)) = mapM_ row' (fillIn dels adds)
  where
    row, row' :: (T.Text, T.Text) -> Html ()
    row' (del, add) = tr_ [class_ "diff"] $
        fold (td_ <$> Diff delAttrs addAttrs <*> diffLines del add)

    row (del, add) = tr_ [class_ "diff"] $ do
        cell delAttrs del
        cell addAttrs add
    addAttrs = [class_ "add"]
    delAttrs = [class_ "del"]

    fillIn :: [T.Text] -> [T.Text] -> [(T.Text, T.Text)]
    fillIn []     []     = []
    fillIn (x:xs) (y:ys) = (x,       y)       : fillIn xs ys
    fillIn (x:xs) []     = (x,       T.empty) : fillIn xs []
    fillIn []     (y:ys) = (T.empty, y)       : fillIn [] ys

cell :: ToHtml a => [Attribute] -> a -> Html ()
cell attrs d = td_ attrs $ pre_ (toHtml d)

data Pair a b = Pair !a !b

diffLines :: T.Text -> T.Text -> Diff (Html ())
diffLines a b = foldl' go (Diff mempty mempty) $ groupItems $ P.diff (T.unpack a) (T.unpack b)
  where
    go :: Diff (Html ()) -> P.Item String -> Diff (Html ())
    go (Diff l r) (P.Both l' r') = Diff (l <> span_ [] (toHtml l')) (r <> span_ [] (toHtml r'))
    go (Diff l r) (P.Old l')     = Diff (l <> span_ new (toHtml l')) r
    go (Diff l r) (P.New r')     = Diff l (r <> span_ new (toHtml r'))
    new = [class_ "change"]

    groupItems :: [P.Item a] -> [P.Item [a]]
    groupItems [] = []
    groupItems (P.Old a : rest) = P.Old (a:xs) : groupItems rest'
      where
        (xs, rest') = breakWith go rest
        go (P.Old x) = Just x
        go _         = Nothing
    groupItems (P.New a : rest) = P.New (a:xs) : groupItems rest'
      where
        (xs, rest') = breakWith go rest
        go (P.New x) = Just x
        go _         = Nothing
    groupItems (P.Both l r : rest) = P.Both (l:ls) (r:rs) : groupItems rest'
      where
        (ls, rs) = unzip xs
        (xs, rest') = breakWith go rest
        go (P.Both a b) = Just (a, b)
        go _            = Nothing
