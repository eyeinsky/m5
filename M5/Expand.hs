{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module M5.Expand where

import Prelude ()
import Prelude2

import qualified Data.HashMap.Lazy as HM
import qualified Data.Text as T
import           Data.Either (either, lefts)
import           Data.List (concat)
-- import           Data.Monoid

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import M5.Helpers
import M5.Types
import M5.Parse


expand :: AST -> M ()
expand (AST ast) = mapM_ (either eStream eMacroBlock) ast

eMacroBlock :: MacroBlock -> M ()
eMacroBlock (MacroBlock lhs text) = define lhs =<< eText text

eMacro :: Macro -> M ()
eMacro (Macro lhs line) = define lhs =<< eLine line

define :: LHS -> Raw -> M ()
define lhs body = modify (HM.insert (name lhs) (args lhs, body))

eStream :: Stream -> M ()
eStream (Stream name text) = output name =<< eText text
   where
      output :: Word -> Raw -> M ()
      output name text = tell $ Output $ HM.singleton name text

eText :: Text -> M Raw
eText text = concat . lefts <$> mapM (bifmap eLine eMacro) text

-- | Evaluate the Line into Raw, and return it instead of 
--   writing to output -- the caller can decide what
--   to do with it.
eLine :: Line -> M Raw
eLine line@ (wss@ (ws : rest), eol) = do
   macros <- get
   case either (flip HM.lookup macros) (const Nothing) ws of
      Just (args, body) -> let
            (argMap, leftovers) = zipArgs args rest HM.empty :: (ArgMap, [Token])
            result = expandArgs argMap body <> [(leftovers, eol)] :: Raw
            -- frag = reparse result :: Fragment
         in return result 
      _  -> prepend ws <$> eLine (rest, eol)
eLine line = return [line]


expandArgs :: ArgMap -> Raw -> Raw
expandArgs argMap body = do
   (tokens, eol) <- body
   token <- tokens
   either expandWord expandSpace token
   where 
      expandWord w = maybe ([([Left w],EOL "")]) (id) (HM.lookup w argMap)
      expandSpace s = [([Right s],EOL "")]

     


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


