{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
module M5.Types where

import Prelude2
import Data.Hashable
import Data.Either (either)
import GHC.Generics (Generic)

import qualified Data.Text as T -- GHC.Generics (Generic)
import qualified Data.HashMap.Lazy as HM


import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Control.DeepSeq.Generics (NFData(..), genericRnf)

import M5.Helpers


--
-- Syntax
-- 

data AST = AST Stream [Stream :| MacroBlock] deriving Show
data MacroBlock = MacroBlock LHS Text deriving (Show)
data Macro = Macro LHS Line deriving (Show)
data LHS = LHS { name :: AbstractName, args :: FormalArgs } deriving (Show)
type FormalArgs = [Word]
data Stream = Stream StreamName Text deriving (Show)
data StreamName
   = Abstract Word
   | Path FilePath
   deriving (Eq, Show, Generic)
instance Hashable StreamName
type Text = [Line :| Macro]
type Raw = [Line]
type Line = ([Token], EOL)
type Token = Word :| Spaces
data Word = W String | Sy String deriving (Eq, Show, Generic)
instance Hashable Word
data Spaces = Sp String deriving (Show, Generic)
data EOL = EOL String | EOF deriving (Show, Generic)
type F = String

type AbstractName = Word


instance NFData EOL where rnf = genericRnf
instance NFData Spaces where rnf = genericRnf
instance NFData Word where rnf = genericRnf


data Body = Body Text [Token]
   deriving (Show)

data Fragment = Fragment
   { fragText :: Text
   , fragBlocks :: [Stream :| MacroBlock]
   } deriving Show


class Default a where
   def :: a

raw2text :: Raw -> T.Text
raw2text raw = T.concat $ map line2te raw
   where
      line2te (toks, EOL eol) = T.concat $ map tok2te toks <> [p eol]
      tok2te = either w2te s2te
      w2te (W str) = p str
      w2te (Sy sy) = p sy
      s2te (Sp str) = p str
      p = T.pack

w2t (W str) = pack str
w2t (Sy str) = pack str

--
-- Expand
--

-- | The output monad is a writer.thus unionWith (<>)
-- is what we want (rather than the default union, which keeps value of the
-- first key).
type M = WriterT (Collector StreamName) (StateT Macros Identity)

-- | Output streams are stored in a key-value map of stream name to stream
-- contents. The monoid instance for the writer is defined with 'unionWith (<>)'
-- as it concatenates the values.
newtype Collector a = Collector { fromCollector :: HM.HashMap a Raw }
   deriving Show

instance (Eq a, Hashable a) => Monoid (Collector a) where
   mempty = Collector HM.empty
   mappend (Collector a) (Collector b) = Collector (a `f` b)
      where f = HM.unionWith (<>)

-- | Macro definitions are kept in a map from macro name to macro definition.
type Macros = HM.HashMap AbstractName Def

-- | Definition of a macro are its formal aruments and its body. A future
-- optimization would be to have a body with pre-found holes (for speed).
type Def = (FormalArgs, Raw)
type ArgMap = HM.HashMap Word Raw

