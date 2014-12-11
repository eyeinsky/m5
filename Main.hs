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
import Text.Parsec hiding (space, spaces, Line, Stream, (<|>))
import Text.Parsec.Text
import Data.Char
import Data.List (concat)
import Data.Either (either, lefts, rights)
import qualified Data.HashMap.Lazy as HM

import Control.Applicative ((*>),(<*), pure)
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Either

import qualified System.Directory as Dir
import qualified System.Console.CmdArgs as C
import           System.Console.CmdArgs ((&=))
import qualified System.Environment as E
import           System.IO (stderr)

import M5.Helpers
import M5.Types
import M5.Parse
import qualified M5.Expand as E
-- import qualified M5.CmdArgs as C


main = runMain $ do
   args <- lift $ C.cmdArgs myargs
   -- @ (Args dbg ovr strOuts strIns)
   let sw :: (Show a) => T.Text -> a -> EitherT T.Text IO ()
       sw = mkDbg (debug args)

   sw "Args from commandline: " args

   outDirs <- f "out directions" $ parseSinks cfg (oo args)
   sw "Calculated out streams: " (oo args)

   inText <- lift $ getConcatIns (ii args)
   sw "Input: " inText
   
   coll <- f "input contents" $ expandInput inText
   mapM_ (sw "Outstream: ") $ HM.toList $ fromCollector coll
   
   putOuts (overwrite args) coll outDirs
   return ()

   where
      f x = either (left . (("Parse error in " <> x) <>) . tshow) return

-- * Main

type MainM = EitherT T.Text IO

runMain :: MainM () -> IO ()
runMain m = either err ignore =<< runEitherT m
   where
      err txt = TIO.putStrLn ("ERROR: "<> txt)
      ignore = const (return ())


-- ** Debuging

mkDbg :: (Show a) => Bool -> T.Text -> a -> MainM ()
mkDbg dbg intro showable = if dbg
   then lift $ TIO.putStrLn (intro <> tshow showable)
   else return ()



-- * Act on inputs and outputs

expandInput text = runM . E.expand <$> parseAst cfg text
   where runM = runIdentity . flip evalStateT HM.empty . execWriterT


putOuts :: Bool -> Collector -> Out -> MainM ()
putOuts ovr (Collector hm) outDirs = if ovr then do_ else checkDo
   where
      checkDo :: MainM ()
      checkDo = bool do_ error . or =<< mapM check outDirs

      do_ :: MainM ()
      do_ = mapM_ (putOut hm) outDirs

      check :: OutPair -> MainM Bool
      check = maybe (return False) (lift . Dir.doesFileExist) . getSinkFilePath
      error :: MainM a
      error = left "some files exist"


putOut :: CollectorR -> OutPair -> MainM () 
putOut hm dir = case dir of 
   Left (name, sink) -> maybe
      (left $ "Collector stream '"<>w2t name<>"' not defined")
      (lift . sinkToIO sink)
      (HM.lookup name hm)
   Right sink -> case HM.toList hm of
      [(_, value)] -> lift $ sinkToIO sink value
      [] -> left "No streams defined in a 'catch all to single sink' output"
      _  -> left "More than one stream defined in a 'catch all to single sink'"
   where
      sinkToIO :: OutStream -> Raw -> IO () 
      sinkToIO sink raw = let 
            text = raw2text raw
         in case sink of
            StdOut -> TIO.putStr text
            StdErr -> TIO.hPutStr stderr text
            OutFile path -> TIO.writeFile path text



-- * Argument parsing

data Args = Args
   { debug     :: Bool  -- ^ Debug
   , overwrite :: Bool  -- ^ Allow owerwriting of files, default is no overwriting

   , baseDir :: String  -- ^ Base directory for output files
   , oo     :: [String] -- ^ Output streams: files, stdout and stderr
   
   , ii     :: [String] -- ^ Input streams: files and stdin

   -- TODO, implement these:
   -- , define :: U -- ^ Define macros on the commandline
   } deriving (C.Typeable, C.Data, Show)


myargs = Args
   { debug     = False              
               &= C.help "print debug info to stdout"
   , overwrite = False &= C.name "f"
               &= C.help "owerwrite existing files"
   , baseDir   = "." &= C.help "base dir"
   , oo        = C.def             
               &= C.help "map streams to files or stdout"
   , ii        = C.def &= C.args &= C.typFile
   } &= C.program "m5"
     &= C.summary "m5 v0.1"

cfg = ParserConf "=" "=>" "\\"



-- * Stream direction

-- | A list of sources, where source is either stdin or file
data Input
   = StdIn
   | File FilePath
   deriving (Show)
instance Default [Input] where def = [StdIn]


-- | Parses input specifiers, and gets their contents as a single text.
getConcatIns []  = TIO.getContents
getConcatIns ins = T.concat <$> mapM (read . parse) ins
   where
      read input = case input of StdIn -> TIO.getContents; File path -> TIO.readFile path
      {- TODO: write a better parser -}
      parse "-" = StdIn
      parse path = File path


-- | A map from stream name to an out-stream (bash) or to file. If
-- only one stream was defined in input, then having a sink is
-- enough -- everythint is put there.
type Out = [OutPair]
type OutPair = (Name, OutStream) :| OutStream
data OutStream
   = StdOut
   | StdErr
   | OutFile FilePath
   deriving (Show)

getSinkFilePath :: OutPair -> Maybe FilePath
getSinkFilePath (Left (name, OutFile path)) = Just path
getSinkFilePath (Right (OutFile path)) = Just path
getSinkFilePath _ = Nothing

-- | Standard out is the default output map.
instance Default Out where def = [( Right StdOut )]

-- | Parses command line mapping arguments to output mappings.

{- TODO: ParserConf not used in parsing commandline, using it 
         only to reuse the content parsers. Should I get rid of
         this dependency? -}

parseSinks :: ParserConf -> [String] -> Either ParseError Out
parseSinks cfg [] = Right def
parseSinks cfg xs = mapM parseOut xs
   where
      parseOut = myparse cfg outParser
      outParser = ((,) <$> word <* gt <*> sink) <:|> sink
      gt = spacesP *> char '>'
      sink = spacesP *> sink'
      sink' = (oneOf "-0" *> return StdOut <* eof)
          <|> (char '2'   *> return StdErr <* eof)
          <|> (OutFile <$> many anyChar <* eof)

