{-# LANGUAGE StandaloneDeriving #-}
module M5.CmdArgs where

import Prelude ()
import Prelude2

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Lazy as HM

import Control.Monad (forM_)

import qualified System.Console.CmdArgs as C
import           System.Console.CmdArgs ((&=))
import qualified System.Environment as E

import M5.Helpers
import M5.Types


-- | A list of sources, where source is either stdin or file
newtype In  = In [() :| FilePath] deriving (Show)
stdin = Left ()

instance Default In where def = In [Left ()]

path = Right

-- | A map from stream name to an out-stream (bash) or to file
newtype Out = Out [(Match, Dest)] deriving (Show, Monoid)
type Match = () :| Word
type Dest = Int :| FilePath

instance Default Out where def = Out [(matchAll, stdout)]
stdout = Left 0
matchAll = Left ()

data Args = Args { oo :: [String], ii :: [String] }
   deriving (C.Typeable, C.Data, Show)

myargs = Args
   { oo = C.def
   , ii = C.def &= C.args &= C.typFile
   }

getArgs = C.cmdArgs myargs

getConcatIns (In ins) = T.concat <$> mapM getIn ins
   where
      getIn (Left _) = TIO.getContents
      getIn (Right p) = TIO.readFile p

putOut (Left i) v = case i of 
   0 -> TIO.putStr v
   _ -> error "todo"
putOut (Right p) v = TIO.writeFile p v

po (Out outs) (Output hm) = mapM_ f outs
   where
      f (Left () {-match all-}, dest) = forM_ (HM.toList hm) $ \ (name, raw) -> let
            f = putOut dest
         in do
            f ("=> " <> w2t name <> "\n")
            f $ raw2text raw
      f (Right name, dest) = maybe (return ()) (putOut dest . raw2text) (HM.lookup name hm)

