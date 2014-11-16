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

module Parser where

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

import Helpers


newtype AST = AST [Stream :| MacroBlock] deriving Show

data MacroBlock = MacroBlock LHS Text deriving (Show)
data Macro = Macro LHS Line deriving (Show)
data LHS = LHS { name :: Name, args :: FormalArgs } deriving (Show)
type Name = Word
type FormalArgs = [Word]

data Stream = Stream F Text deriving (Show)
type Text = [Line :| Macro]
type Raw = [Line]
type Line = ([Token], EOL)
type Token = Word :| Spaces
data Word = W String | Sy String deriving (Eq, Show, Generic)
instance Hashable Word
data Spaces = Sp String deriving (Show)
data EOL = EOL String deriving (Show)
type F = String

ast = AST <$> many (stream <:|> macroblock)

reparse :: Raw -> AST
reparse lines = AST [Left $ Stream u $ map Left lines]
   where x = x
      

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
    <|>    Sy <$> many1 symbol
    <?> "word"
    where
      symbol = satisfy $ \ c -> not (isAlphaNum c || c `elem` (sp <> nl <> spc))

spaces = Sp <$> spacesP <?> "spaces"
eol = EOL <$> many1 (oneOf nl) <?> "eol"

spacesP = many1 spaceP 
spaceP = oneOf sp

sp = " \t"
nl = "\r\n"
spc = "=\\"

