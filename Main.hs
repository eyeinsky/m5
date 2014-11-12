{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Prelude2
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec hiding (space, spaces, Line, Stream)
import Text.Parsec.Text
import Data.Char
import Data.Either (either, lefts)

import Control.Applicative ((*>),(<*), pure)

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import GHC.Generics (Generic)

import Helpers
import Parser

main = do
   [smt, sm] <- mapM getTest ["sm-and-text", "sm"]
   parseTest parser smt

type Macros = HM.HashMap Name ([Word], [Line])
type Output = [Line]
type M = WriterT Output (StateT Macros Identity)
type ArgMap = HM.HashMap Word Word

evaluate :: AST -> M ()
evaluate (AST ast) = mapM_ (either eStream eMacroBlock) ast

eMacroBlock :: MacroBlock -> M ()
eMacroBlock (MacroBlock lhs text) = define lhs =<< eText text

eMacro :: Macro -> M ()
eMacro (Macro lhs line) = define lhs . pure =<< eLine line 

define :: LHS -> [Line] -> M ()
define lhs body = modify (HM.insert (name lhs) (args lhs, body))

eStream :: Stream -> M ()
eStream (Stream _ text) = output =<< eText text

eText :: Text -> M [Line]
eText text = lefts <$> mapM (bifmap eLine eMacro) text

eLine :: Line -> M Line
eLine line = do
   macros <- get
   let f (w'@ (Left w) : ws) = let 
            g (args, body) = u
         in maybe w' g (HM.lookup w macros)



   u
   where 
      zipw :: FormalArgs -> [Word :| Spaces] -> ArgMap -> (ArgMap, [Word :| Spaces])
      zipw formal@ (x : xs) actual@ (Left w : ys) assoc
         = zipw xs ys (HM.insert x w assoc)
      zipw formal@ (x : xs) actual@ (_ {-space-} : ys) assoc 
         = zipw formal ys assoc
      zipw _ leftover assoc
         = (assoc, leftover)
      
      apply :: Macros -> Line -> Line
      apply hm (ws, eol) = u -- bimap id ws

output :: [Line] -> M ()
output = tell
