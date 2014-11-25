{-# LANGUAGE StandaloneDeriving #-}
module M5.CmdArgs where

import Prelude ()
import Prelude2

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import qualified Data.HashMap.Lazy as HM

import qualified System.Console.CmdArgs as C
import qualified System.Environment as E

import M5.Helpers
import M5.Types


-- | A list of sources, where source is either stdin or file
newtype In  = In [() :| FilePath]

instance Default In where def = In [Left ()]

path = Right

-- | A map from stream name to an out-stream (bash) or to file
newtype Out = Out [(Match, Dest)]
type Match = () :| Word
type Dest = Int :| FilePath

instance Default Out where def = Out [(matchAll, stdout)]
stdout = Left 0
matchAll = Left ()

deriving instance C.Typeable Word
deriving instance C.Typeable In
deriving instance C.Typeable Out
deriving instance C.Data Word
deriving instance C.Data In
deriving instance C.Data Out

data Args = Args { input :: In, output :: Out } deriving (C.Typeable, C.Data)
args = u --

getArgs = f <$> E.getArgs
   where
      f xs = case xs of
         (src : dest : _) -> Args (In [path src]) (Out [(matchAll, path dest)])
         (src : _) -> Args (In [path src]) def
         _ -> Args def def


getConcatIns (In ins) = T.concat <$> mapM getIn ins
   where
      getIn (Left _) = TIO.getContents
      getIn (Right p) = TIO.readFile p

putOut (Left i) v = case i of 
   0 -> TIO.putStr v
   _ -> error "todo"
   -- 1 -> TIO.putStr v
putOut (Right p) v = TIO.writeFile p v

po (Out outs) hm = mapM_ f outs
   where
      f (Left () {-match all-}, dest) = u
      f (Right name, dest) = maybe (return ()) (putOut dest . raw2text) (HM.lookup name hm)

