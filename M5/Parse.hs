{-# LANGUAGE NoMonomorphismRestriction #-}

module M5.Parse where

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


import M5.Helpers
import M5.Types


ast = AST <$> many (stream <:|> macroblock)

reparse :: Raw -> AST
reparse lines = AST [Left $ Stream u $ map Left lines]
   where x = x
      

stream = Stream
   <$> (string "=>" *> many spaceP *> word <* eol)
   <*> text
   <?> "stream"

macroblock = MacroBlock
   <$> (many spaceP *> char '=' *> lhs <* eol)
   <*> many (line <:|> macro)
   <?> "macroblock"

text = many1 (line <:|> macro) <?> "text"

macro = Macro
   <$> (lhs <* char '=')
   <*> (many spaceP *> line)
   <?> "macro"

lhs = LHS
   <$> (many spaceP *> word <* many spaceP)
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

