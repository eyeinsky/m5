{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module M5.Types where

import Prelude2
import Data.Hashable
import GHC.Generics (Generic)

import qualified Data.Text as T -- GHC.Generics (Generic)



import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.HashMap.Lazy as HM



import M5.Helpers


--
-- Syntax
-- 

newtype AST = AST [Stream :| MacroBlock] deriving Show
data MacroBlock = MacroBlock LHS Text deriving (Show)
data Macro = Macro LHS Line deriving (Show)
data LHS = LHS { name :: Name, args :: FormalArgs } deriving (Show)
type Name = Word
type FormalArgs = [Word]
data Stream = Stream Word Text deriving (Show)
type Text = [Line :| Macro]
type Raw = [Line]
type Line = ([Token], EOL)
type Token = Word :| Spaces
data Word = W String | Sy String deriving (Eq, Show, Generic)
instance Hashable Word
data Spaces = Sp String deriving (Show)
data EOL = EOL String deriving (Show)
type F = String


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


--
-- Expand
--

type M = WriterT Output (StateT Macros Identity)

type Output = HM.HashMap Word Raw
instance Monoid (HM.HashMap Word Raw) where
   mempty = HM.fromList [(W "stdout", [([Left $ W ""], EOL "")])]
   mappend = HM.unionWith (<>)

type Macros = HM.HashMap Name Def
type Def = (FormalArgs, Raw)
type ArgMap = HM.HashMap Word Raw


