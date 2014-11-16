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
import Data.List (concat)
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
   txt <- TIO.getContents
   case parse ast "<stdin>" txt of
      Left err -> print err
      Right res -> let 
            raw = evaluate HM.empty res
            text = raw2text raw
         in do
            -- TIO.putStr text
            print res
            print "-----"
            print raw
            print "-----"
            TIO.putStr text

test = do
   [smt, sm] <- mapM getTest ["sm-and-text", "sm"]
   parseTest ast smt

type M = WriterT Output (StateT Macros Identity)

type Output = Raw

type Macros = HM.HashMap Name Def
type Def = (FormalArgs, Raw)
type ArgMap = HM.HashMap Word Raw


evaluate :: Macros -> AST -> Raw
evaluate macros (AST ast) = f $ es $ execWriterT $ mapM_ (either eStream eMacroBlock) ast
   where es = flip evalStateT macros
         f = runIdentity

eMacroBlock :: MacroBlock -> M ()
eMacroBlock (MacroBlock lhs text) = define lhs =<< eText text

eMacro :: Macro -> M ()
eMacro (Macro lhs line) = define lhs =<< eLine' line

define :: LHS -> Raw -> M ()
define lhs body = modify (HM.insert (name lhs) (args lhs, body))

eStream :: Stream -> M ()
eStream (Stream _ text) = output =<< eText text

eText :: Text -> M Raw
eText text = concat . lefts <$> mapM (bifmap eLine' eMacro) text

-- | Evaluate the Line into Raw, and return it instead of 
--   writing to output -- the caller can decide what
--   to do with it.
eLine' :: Line -> M Raw
eLine' line = eLine line <$> get

eLine :: Line -> Macros -> Raw
eLine line@ (wss@ (ws : rest), eol) macros =
   case either (flip HM.lookup macros) (const Nothing) ws of
      Just (args, body) -> let
            (argMap, leftovers) = zipArgs args rest HM.empty :: (ArgMap, [Token])
            result = expand argMap body <> [(leftovers, eol)]
         in evaluate macros $ reparse result
      _  -> let rest' = eLine (rest, eol) macros
         in prepend ws rest'
eLine line _ = [line]


expand :: ArgMap -> Raw -> Raw
expand argMap body = do
   (tokens, eol) <- body
   token <- tokens
   either expandWord expandSpace token
   where 
      expandWord w = maybe ([([Left w],EOL "")]) (id) (HM.lookup w argMap)
      expandSpace s = [([Right s],EOL "")]

     
output :: Raw -> M ()
output = tell



-- helpers

zipArgs :: FormalArgs -> [Token] -> ArgMap
        -> (ArgMap, [Token])
zipArgs formal@ (x : xs) actual@ (Left w : ys) assoc
   = zipArgs xs ys (HM.insert x (w2raw w) assoc)
zipArgs formal@ (x : xs) actual@ (_ {-space-} : ys) assoc 
   = zipArgs formal ys assoc
zipArgs _ leftover assoc
   = (assoc, leftover)


prepend :: Token -> Raw -> Raw
prepend w ((ws, eol) : rest) = (w : ws, eol) : rest
prepend w raw = error "prepend: empty raw"

w2raw :: Word -> Raw
w2raw w = [([Left w],EOL "")]

--
-- Output
--

raw2text :: Raw -> T.Text
raw2text raw = T.concat $ map line2te raw
   where
      line2te (toks, EOL eol) = T.concat $ map tok2te toks <> [p eol]

      tok2te = either w2te s2te

      w2te (W str) = p str
      w2te (Sy sy) = p sy
      s2te (Sp str) = p str
      p = T.pack

