{-# LANGUAGE NoMonomorphismRestriction #-}

module M5.Parse where

import Prelude2
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec hiding (space, spaces, Line, Stream, token, (<|>))
import Text.Parsec.Text
import Data.Char
import Data.Either (either)

import Control.Applicative ((*>),(<*), pure)

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import GHC.Generics (Generic)


import M5.Helpers
import M5.Types


--
-- Parser with a config:
--

data ParserConf = ParserConf
   { pcDef      :: String
   , pcStream   :: String
   , pcEsc      :: String
   }
askDef = asks pcDef
askStream = asks (\c -> pcDef c <> pcStream c)

type ParserMonad = ParsecT T.Text ()     (Reader ParserConf) () 
--                 ParsecT stream stateu baseMonad           return


parseAst :: ParserConf -> T.Text -> Either ParseError AST
parseAst cfg text = cfgParse cfg ast text

cfgParse cfg parser text = runReader (runParserT parser () "<todo>" text) cfg


ast = AST <$> stdout <*> many (stream <:|> macroblock) <* eof
   where stdout = Stream (W "stdout") <$> (body <|> return [])

stream = Stream
   <$> ((asks pcStream >>= string) *> many spaceP *> word <* eol)
   <*> body
   <?> "stream"


macroblock = MacroBlock
   <$> (many spaceP *> (askDef >>= string) *> lhs <* eol)
   <*> many line
   <?> "macroblock"
macro = Macro
   <$> (lhs <* (askDef >>= string))
   <*> (many spaceP *> textline)
   <?> "macro"
lhs = LHS
   <$> (many spaceP *> word <* many spaceP)
   <*> (sepEndBy word $ many spaceP)
   <?> "lhs"


body = many1 line <?> "body"
line = textline <:|> macro
textline = full <|> partial <?> "textline"
   where full = (,) <$> many token  <*> eol
         partial = (,) <$> many1 token <*> (eof *> return (EOL ""))
token = word <:|> spaces <?> "token"
word = W  <$> many1 alphaNum
   <|> Sy <$> many1 symbol
   <?> "word"
   where
      symbol = do
         def <- askDef
         satisfy $ \ c -> not (isAlphaNum c || c `elem` (sp <> nl <> def))
spaces = Sp <$> many1 spaceP <?> "spaces"
eol = EOL <$> eolP <?> "eol"

spacesP = many spaceP 
spaceP = oneOf sp
eolP = f '\n' '\r' <|> f '\r' '\n'
   where f a b = (:) <$> char a <*> (string [b] <|> pure [])

sp = " \t"
nl = "\r\n"


pt p (s :: T.Text) = parseTest p s
