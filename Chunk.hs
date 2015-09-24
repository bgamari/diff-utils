module Chunk where

import Data.Monoid
import Data.Maybe (fromJust)
import qualified Data.DList as DList
import Data.Foldable
import qualified Data.Text as T

import Diff hiding (line, body)

data ChunkBody = CContext [T.Text]
               | CDiff (Diff [T.Text])
               deriving (Show)

data Chunk = Chunk { chunkFiles :: Diff DiffFile
                   , chunkPos :: Diff Position
                   , chunkBody :: [ChunkBody]
                   }
           deriving (Show)

toChunks :: [DiffLine] -> [Chunk]
toChunks (Header files0 pos0 : rest0) =
    goChunk files0 pos0 mempty rest0
  where
    goChunk :: Diff DiffFile -> Diff Position -> DList.DList ChunkBody
            -> [DiffLine] -> [Chunk]
    goChunk files pos accum [] = [Chunk files pos (toList accum)]
    goChunk files pos accum (Position pos' : rest) =
        Chunk files pos (toList accum) : goChunk files pos' mempty rest

    goChunk files pos accum (Header files' pos' : rest) =
        Chunk files pos (toList accum) : goChunk files' pos' mempty rest

    goChunk files pos accum (Body (Context line) : rest) =
        goChunk files pos (accum `DList.snoc` body) rest'
      where
        body = CContext (line:ctxLines)
        (ctxLines, rest') = breakWith ctx rest
        ctx (Body (Context l)) = Just l
        ctx _                  = Nothing

    goChunk files pos accum (other : rest) =
        goChunk files pos (accum `DList.snoc` body) rest'
      where
        body = CDiff $ fold $ fromJust (f other) : bodyLines
        (bodyLines, rest') = breakWith f rest :: ([Diff [T.Text]], [DiffLine])
        f :: DiffLine -> Maybe (Diff [T.Text])
        f (Body (Deletion line)) = Just $ Diff [line] []
        f (Body (Addition line)) = Just $ Diff []     [line]
        f _                      = Nothing
toChunks _ = error "toChunks: Malformed diff"

breakWith :: (a -> Maybe b) -> [a] -> ([b], [a])
breakWith f = go mempty
  where
    go accum (x:xs)
      | Just y <- f x = go (accum `DList.snoc` y) xs
      | otherwise     = (toList accum, x:xs)
    go accum []       = (toList accum, [])
