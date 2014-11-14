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

type M = WriterT Output (StateT Macros Identity)

type Output = Raw

type Macros = HM.HashMap Name Def
type Def = (FormalArgs, Raw)
type ArgMap = HM.HashMap Word Raw


reparse :: Raw -> AST
reparse = u

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
eLine line@ (ws, eol)  = do
   macros <- get
   u
   where 
      {- I need to concatMap or similar here:
         * expand macros from a single line to raw (i.e, many lines),
         * then reparse raw to AST, and
         * then recurse in expansion until nothing can be expanded anymore.
      -}

      applier :: Macros -> Line -> Raw
      applier hm (w'@ (Left w) : ws, eol) = case HM.lookup w hm of
         Just ((args, body) :: Def)
            -> -- a matching macro! => will expand it
            u
         _  -> -- no match => leave word as is and prepend it to the output
            w `prepend` applier hm (ws, eol)

      helper :: Macros -> [Word :| Spaces] -> Def -> [Line]
      helper macros ws (args, body) = let
         (argMap, leftover) = zipArgs args ws HM.empty :: (ArgMap, [Word :| Spaces])
         in expand macros argMap =<< body

      expand :: Macros -> ArgMap -> Line -> [Line]
      expand macros argMap (w'@ (Left w) : ws, eol) = 
         maybe u
            (\(lines :: [Line]) -> applier macros =<< (lines<>[(ws, eol)]))
            (HM.lookup w argMap)
      -- expand argMap (s           : ws, eol) = u

output :: Raw -> M ()
output = tell



-- helpers

zipArgs :: FormalArgs -> [Word :| Spaces] -> ArgMap
        -> (ArgMap, [Word :| Spaces])
zipArgs formal@ (x : xs) actual@ (Left w : ys) assoc
   = zipArgs xs ys (HM.insert x u {-w-} assoc)
zipArgs formal@ (x : xs) actual@ (_ {-space-} : ys) assoc 
   = zipArgs formal ys assoc
zipArgs _ leftover assoc
   = (assoc, leftover)


prepend :: Word -> Raw -> Raw
prepend w ((ws, eol) : rest) = (Left w : ws, eol) : rest
prepend w raw = error "prepend: empty raw"
