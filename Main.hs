{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Prelude2
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec hiding (space, spaces, Line, Stream)
import Text.Parsec.Text
import Data.Char
import Data.Either (either)

import Control.Applicative ((*>),(<*))

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import GHC.Generics (Generic)

main = do
   [smt, sm] <- mapM getTest ["sm-and-text", "sm"]
   parseTest parser smt


--
-- Parser
--

newtype AST = AST [Stream :| MacroBlock] deriving Show

data MacroBlock = MacroBlock LHS Text deriving (Show)
data Macro = Macro LHS Line deriving (Show)
data LHS = LHS Name FormalArgs deriving (Show)
type Name = Word
type FormalArgs = [Word]

data Stream = Stream F Text deriving (Show)
type Text = [Line :| Macro]
type Line = ([Word :| Spaces], EOL)
data Word = W String | Sy String deriving (Eq, Show, Generic)
instance Hashable Word
data Spaces = Sp String deriving (Show)
data EOL = EOL String deriving (Show)
type F = String

parser = AST <$> many (stream <:|> macroblock)

stream = Stream "stdout" <$> text

macroblock = MacroBlock
   <$> (many spaceP *> char '=' *> lhs <* eol)
   <*> many (line <:|> macro)
   <?> "macroblock"

text = many1 (line <:|> macro) <?> "text"

macro = Macro <$> (lhs <* char '=')
              <*> (many spaceP *> line)
              <?> "macro"

lhs = LHS <$> (many spaceP *> word <* many spaceP)
          <*> (sepEndBy word $ many spaceP)
          <?> "lhs"

line = (,) <$> many (word <:|> spaces) <*> eol <?> "line"

word = try (W <$> many1 alphaNum)
    <|>    Sy <$> (many1 . noneOf $ sp <> nl <> spc)
    <?> "word"

spaces = Sp <$> spacesP <?> "spaces"
eol = EOL <$> many1 (oneOf nl) <?> "eol"

spacesP = many1 spaceP 
spaceP = oneOf sp

sp = " \t"
nl = "\r\n"
spc = "=\\"




--
-- Helpers
--
getTest file = TIO.readFile ("tests/" <> file) :: IO T.Text
u = undefined


parseEither l r = try (Left <$> l) <|> (Right <$> r)
(<:|>) = parseEither 
infixl 5 <:|>

infixl 5 :|
type (:|) = Either
bimap f g e = case e of
   Left l -> Left $ f l
   Right r -> Right $ g r


{- https://www.gnu.org/software/m4/manual/m4.html#Changeword
   default is [_a-zA-Z][_a-zA-Z0-9]* -}


--
-- Evaluate
--

type Macros = HM.HashMap F ([Text] -> F)
type Output = [Line]
type M = WriterT Output (StateT Macros Identity)

-- newtype AST = AST [Stream :| MacroBlock] deriving Show
evaluate (AST (Left x : _)) = f x
   where
   -- data Stream = Stream F Text deriving (Show)
   -- type Text = [Line :| Macro]
   f (Stream _ value) = map (either evalLine defineMacro) value

evalLine line = do
   macroMap <- get
   -- JÄRG find if macro and then take args
   tell =<< (flip replace line <$> get)

defineMacro (Macro (LHS name formal) value) = modify (HM.insert name func)
   where
      func actual = let
            hmap = HM.fromList $ zip formal actual
         in replace hmap value

-- type Line = ([Word :| Spaces], EOL)
replace :: HM.HashMap Word Word -> Line -> Line
replace hmap (xs, eol) = (map f xs, eol)
   where 
      f (Left w) = Left $ maybe w id $ HM.lookup w hmap
      f r = r
{-

define :: Macro -> M ()
define _ = error "make symbol a separate type! (apart from Word)"

evaluate :: AST -> F
evaluate ast = u

   where 
      define x = modify
 -}       


