{-# LANGUAGE BangPatterns #-}

import Diff
import qualified Data.Sequence as S
import Data.Machine
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL

import           Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.Lazy.Builder as TB
import Control.Applicative


mapChunks :: (Diff DiffFile -> Diff Position -> [DiffBody] -> [DiffBody])
           -> [DiffLine] -> [DiffLine]
mapChunks f = go (error "no initial header")
  where
    go :: Diff DiffFile -> [DiffLine] -> [DiffLine]
    go hdr []                     = []
    go _   (Header hdr : rest)    = Header hdr : go hdr rest
    go hdr (Position pos : diffs) = Position pos' : diffs' ++ go hdr rest
      where
        (chunk, rest) = break collectBodies diffs
          where collectBodies (Body {}) = False
                collectBodies _         = True
        diffs' = map Body $ f hdr pos $ map (\(Body body)->body) chunk
        pos' = pos -- FIXME
    go hdr (_:rest)               = go hdr rest


-- | @dropContext s e diffs@ drops 'Context' lines from @diffs@
-- such that each change is preceded by no more than @s@ lines of context and
-- succeeded by no more than @e@ lines
dropContext :: Int -> Int -> [DiffLine] -> [DiffLine]
dropContext s e diffs = concat $ run $ supply diffs (auto $ dropContext' s e)

dropContext' :: Int -> Int -> Mealy DiffLine [DiffLine]
dropContext' s e = go [] 0
  where
    go :: [DiffLine] -> Int -> Mealy DiffLine [DiffLine]
    go contextAccum !afterCtx  = Mealy $ \diff ->
        case diff of
          Header {}        -> ([diff], go [] 0)
          Position {}      -> ([diff], go [] 0)
          Body (Context {})
            | afterCtx > 0 -> ([diff], go [] (afterCtx-1))
            | otherwise    -> ([], go (diff:contextAccum) 0)
          _           -> (reverse $ diff:take s contextAccum, go [] e)

{-}
uniqueFilter :: Mealy Diff [Diff]
uniqueFilter = Mealy go
  where
    go :: Diff -> ([Diff], Mealy Diff [Diff])
    go _ = return []
-}

main = return ()
