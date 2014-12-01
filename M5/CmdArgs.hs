{-# LANGUAGE StandaloneDeriving #-}
module M5.CmdArgs where

import Prelude ()
import Prelude2

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Lazy as HM
import Data.Either (either)

import Control.Applicative ((*>),(<*))
import Control.Monad (forM_)

import Text.Parsec

import qualified System.Console.CmdArgs as C
import           System.Console.CmdArgs ((&=))
import qualified System.Environment as E

import M5.Helpers
import M5.Types
import M5.Parse


data Args = Args { oo :: [String], ii :: [String] }
   deriving (C.Typeable, C.Data, Show)

myargs = Args
   { oo = C.def
   , ii = C.def &= C.args &= C.typFile
   }

{-
getArgs = do
   Args oo ii <- C.cmdArgs myargs
   let oo' = mapM parseOut oo
   return (oo', ii)
-}
cmdArgs' = C.cmdArgs myargs

-- | A list of sources, where source is either stdin or file
newtype In  = In [() :| FilePath] deriving (Show)

instance Default In where def = In [Left ()]

getConcatIns []  = TIO.getContents
getConcatIns ins = T.concat <$> mapM (read . parse) ins
   where
      read = either (const TIO.getContents) TIO.readFile
      parse str = case str of
         "-" -> Left () -- stdin
         _ -> Right str -- path ..


-- | A map from stream name to an out-stream (bash) or to file
newtype Out = Out [((Name, Sink) :| Sink)] deriving (Show, Monoid)
type Sink = () :| FilePath

instance Default Out where def = Out [( Right $ Left () )]

parseOuts [] = Right [( Right $ Left () )]
parseOuts xs = mapM parseOut xs
   where
      parseOut = parse outParser "<outParser>"
      outParser = ((,) <$> word <* gt <*> sink) <:|> sink
      gt = spacesP *> char '>'
      sink = spacesP *> sink'
      sink' = (Left  <$> (oneOf "-0" *> return ()) <* eof)
         <|> (Right <$> many anyChar)


