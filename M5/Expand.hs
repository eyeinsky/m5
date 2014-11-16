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

type M = WriterT Output (StateT Macros Identity)


--
type Output = Raw

output :: Raw -> M ()
output = tell

type Output2 = HM.HashMap Word Raw
output2 name raw = tell $ HM.fromList [(name, raw)]

instance Monoid (HM.HashMap Word Raw) where
   mempty = HM.fromList [(W "stdout", [([Left $ W ""], EOL "")])]
   mappend = HM.unionWith (<>)

type Macros = HM.HashMap Name Def
type Def = (FormalArgs, Raw)
type ArgMap = HM.HashMap Word Raw


expand :: Macros -> AST -> Output
expand macros (AST ast) = f $ mapM_ (either eStream eMacroBlock) ast
   where f = runIdentity . flip evalStateT macros . execWriterT

eMacroBlock :: MacroBlock -> M ()
eMacroBlock (MacroBlock lhs text) = define lhs =<< eText text

eMacro :: Macro -> M ()
eMacro (Macro lhs line) = define lhs =<< eLine' line

define :: LHS -> Raw -> M ()
define lhs body = modify (HM.insert (name lhs) (args lhs, body))

eStream :: Stream -> M ()
eStream (Stream name text) = output =<< eText text

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
            result = expandArgs argMap body <> [(leftovers, eol)]
         in expand macros $ reparse result
      _  -> let rest' = eLine (rest, eol) macros
         in prepend ws rest'
eLine line _ = [line]


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

raw2text :: Raw -> T.Text
raw2text raw = T.concat $ map line2te raw
   where
      line2te (toks, EOL eol) = T.concat $ map tok2te toks <> [p eol]
      tok2te = either w2te s2te
      w2te (W str) = p str
      w2te (Sy sy) = p sy
      s2te (Sp str) = p str
      p = T.pack

