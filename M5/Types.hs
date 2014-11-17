{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module M5.Types where

import Prelude2
import Data.Hashable
import GHC.Generics (Generic)


import M5.Helpers


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
