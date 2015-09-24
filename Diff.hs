{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}

module Diff where

import Data.Monoid
import Data.Foldable
import qualified Data.DList as DList
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import           Data.Attoparsec.Text
import Control.Applicative
import Control.Monad (void)

-- | This functor represents an abstract pair of things
-- associated with the deletion and addition sides of the diff
data Diff a = Diff { deletion, addition :: a }
            deriving (Show, Eq, Ord, Functor)

instance Applicative Diff where
    pure x = Diff x x
    Diff a b <*> Diff x y = Diff (a x) (b y)

instance Monoid a => Monoid (Diff a) where
    mempty = pure mempty
    Diff a b `mappend` Diff x y = Diff (a <> x) (b <> y)

instance Foldable Diff where
    foldMap f (Diff a b) = f a <> f b

instance Traversable Diff where
    traverse f (Diff a b) = Diff <$> f a <*> f b

data DiffFile = DiffFile {fileName, timestamp :: T.Text}
              deriving (Show, Eq)

data Position = Pos { startLine, extent :: !Int }
              deriving (Show, Eq)

-- | A line from a diff
data DiffLine
    = Header { diffFiles :: Diff DiffFile
             , position :: Diff Position
             }
    | Position { position :: Diff Position }
    | Body { body :: DiffBody }
    deriving (Show, Eq)

-- | A body line of a diff (either context, addition, or deletion)
data DiffBody
    = Context { line :: T.Text }
    | Deletion { line :: T.Text }
    | Addition { line :: T.Text }
    deriving (Show, Eq)

format :: DiffLine -> TB.Builder
format (Header (Diff rm add) pos) =
    "--- "<>hdr rm<>"\n+++ "<>hdr add<>format (Position pos)
  where
    hdr (DiffFile fn ts) = TB.fromText fn<>"\t"<>TB.fromText ts
format (Position (Diff rm add))   = "@@ -"<>pos rm<>" +"<>pos add<>" @@"
  where
    pos (Pos l s) = TB.decimal l<>","<>TB.decimal s
format (Body body)                = symbol<>TB.fromText (line body)
  where
    symbol = case body of
               Context {}  -> " "
               Deletion {} -> "-"
               Addition {} -> "+"

takeLine :: Parser T.Text
takeLine = takeTill (=='\n') <* char '\n'

diff :: Parser DiffLine
diff = header <|> (Position <$> position) <|> body
  where
    header = try $ do
        del <- string "---" *> skipSpace *> hdr
        add <- string "+++" *> skipSpace *> hdr
        pos <- position
        return $ Header (Diff del add) pos
      where
        hdr = do
            file <- T.pack <$> manyTill anyChar (void (char '\t') <|> endOfLine)
            timestamp <- option "" $ char '\t' *> takeLine
            return $ DiffFile file timestamp

    position = do
        string "@@"
        rm <- skipSpace *> char '-' *> pos
        add <- skipSpace *> char '+' *> pos
        rest <- skipSpace *> string "@@" *> takeLine
        return $ Diff rm add
      where
        pos = Pos <$> decimal <*> option 1 (char ',' *> decimal)

    body = Body <$> (context <|> removal <|> addition)
    context  = Context <$> (char ' ' *> takeLine)
    removal  = Deletion <$> (char '-' *> takeLine)
    addition = Addition <$> (char '+' *> takeLine)
