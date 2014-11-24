{-# LANGUAGE StandaloneDeriving #-}
module M5.CmdArgs where


import qualified System.Console.CmdArgs as C
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import M5.Helpers
import M5.Types


-- | A list of sources, where source is either stdin or file
newtype In  = In [() :| FilePath]

instance Default In where def = In [Left ()]

-- | A map from stream name to an out-stream (bash) or to file
newtype Out = Out [(Word, Int :| FilePath)]

instance Default Out where def = Out [(W "stdout", stdout)]
stdout = Left 0

deriving instance C.Typeable Word
deriving instance C.Typeable In
deriving instance C.Typeable Out
deriving instance C.Data Word
deriving instance C.Data In
deriving instance C.Data Out

data Args = Args { input :: In, output :: Out } deriving (C.Typeable, C.Data)
args = u --

getIn (Left _) = TIO.getContents
getIn (Right p) = TIO.readFile p

putOut (Left i) v = case i of 
   0 -> TIO.putStr v
   _ -> error "todo"
   -- 1 -> TIO.putStr v
putOut (Right p) v = TIO.writeFile p v
