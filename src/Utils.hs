module Utils where

import qualified Data.DList as DList

breakWith :: (a -> Maybe b) -> [a] -> ([b], [a])
breakWith f = go mempty
  where
    go accum (x:xs)
      | Just y <- f x = go (accum `DList.snoc` y) xs
      | otherwise     = (DList.toList accum, x:xs)
    go accum []       = (DList.toList accum, [])
