{-# LANGUAGE NoMonomorphismRestriction #-}

module M5.Parse where

import Prelude2
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec hiding (space, spaces, Line, Stream, token, (<|>))
import Text.Parsec.Text
import Data.Char
import Data.Either (either)

import Control.Applicative ((*>),(<*))

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import GHC.Generics (Generic)


import M5.Helpers
import M5.Types



{-
-- reparse :: Raw -> Fragment
reparse lins = u -- Fragment [Right $ Left $ Stream u $ map Left lins]
   where x = x

fragment = Fragment
   <$> body
   <*> many (stream <:|> macroblock)
   <?> "fragment"

-}

--
-- Parser with a config:
--

data ParserConf = ParserConf
   { pcDef      :: String
   , pcStream   :: String
   , pcEsc      :: String
   }

type ParserMonad = ParsecT T.Text () (Reader ParserConf) () 
                -- ParsecT s      u  m                   a

{- runParserT :: Stream s m t
      => ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)

   expandInput text = runM . E.expand <$> parse P.ast "<todo>" text
      where runM = runIdentity . flip evalStateT HM.empty . execWriterT
-}

myparse :: ParserConf -> T.Text -> Either ParseError AST
myparse cfg text = runReader (runParserT ast () "<todo>" text) cfg



ast = AST <$> stdout <*> many (stream <:|> macroblock)
   where stdout = Stream (W "stdout") <$> (body <|> return [])

stream = Stream
   <$> ((asks pcStream >>= string) *> many spaceP *> word <* eol)
   <*> body
   <?> "stream"

macroblock = MacroBlock
   <$> (many spaceP *> char '=' *> lhs <* eol)
   <*> many line
   <?> "macroblock"
macro = Macro
   <$> (lhs <* char '=')
   <*> (many spaceP *> textline)
   <?> "macro"
lhs = LHS
   <$> (many spaceP *> word <* many spaceP)
   <*> (sepEndBy word $ many spaceP)
   <?> "lhs"


body = many1 line <?> "body"

line = textline <:|> macro
textline' = (,) <$> many token <*> eol <?> "textline"
token = word <:|> spaces <?> "token"

textline = full <|> partial <?> "textline"
   where 
      full = (,) <$> many token  <*> eol
      partial = (,) <$> many1 token <*> (eof *> return (EOL ""))

word = W  <$> many1 alphaNum
   <|> Sy <$> many1 symbol
   <?> "word"
   where
      symbol = satisfy $ \ c -> not (isAlphaNum c || c `elem` (sp <> nl <> spc))



spaces = Sp <$> many1 spaceP <?> "spaces"
eol = EOL <$> eolP <?> "eol"

spacesP = many spaceP 
spaceP = oneOf sp
eolP = many1 (oneOf nl)

sp = " \t"
nl = "\r\n"
spc = "=\\"



pt p (s :: T.Text) = parseTest p s
