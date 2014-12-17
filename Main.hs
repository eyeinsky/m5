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
import Data.Word (Word64)
import Data.List (concat)
import Data.Either (either, lefts, rights)
import qualified Data.HashMap.Lazy as HM

import Control.Applicative ((*>),(<*), pure)
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Either
import qualified Control.DeepSeq.Generics as DeepSeq

import qualified System.Directory as Dir
import qualified System.Environment as E
import           System.IO (stderr)
import           System.Random (randomIO)

import qualified Options.Applicative as O

import M5.Helpers
import M5.Types
import M5.Parse
import qualified M5.Expand as E



-- * Main
{- TODO:
   * the <<< >>> delimiting debugger output is hacky..
   -}
main = runMain $ do
   args <- lift $ O.execParser optp
   -- args <- u

   let sw :: (Show a) => (T.Text -> T.Text) -> a -> EitherT T.Text IO ()
       sw = mkDbg (debug args)
       mBase = baseDir args
       outDirs = maybe id addBase mBase $ oo args
       inDirs = let i = ii args in if null i then def else i

   sw ("Arguments: " <>) args
   sw ("Calculated in streams: " <>) inDirs
   sw ("Calculated out streams: " <>) outDirs

   inText <- readInputs inDirs
   rand <- tshow <$> lift (randomIO :: IO Word64)
   sw (\x -> "Input was:\n<<< " <> rand <> "\n"
          <> x <> ">>> " <> rand)
      inText
   
   coll <- f "input contents" $ expandInput inText
   mapM_ (sw ("Outstream: "<>)) $ HM.toList $ fromCollector coll
   
   maybe (return ()) (lift . Dir.createDirectoryIfMissing True) mBase -- create directory
   putOuts (pretend args) (overwrite args) coll outDirs
   return ()

   where
      f x = either (left . (("Parse error in " <> x) <>) . tshow) return


type MainM = EitherT T.Text IO

runMain :: MainM () -> IO ()
runMain m = either err ignore =<< runEitherT m
   where
      err txt = TIO.putStrLn ("ERROR: "<> txt)
      ignore = const (return ())

-- ** Hardcoded parser configuration 

cfg = ParserConf "=" "=>" "\\"


-- ** Debuging

mkDbg :: (Show a) => Bool -> (T.Text -> T.Text) -> a -> MainM ()
mkDbg dbg f showable = if dbg
   then lift $ TIO.putStrLn (f $ tshow showable)
   else return ()


-- * Act on inputs and outputs

expandInput text = runM . E.expand <$> parseAst cfg text
   where runM = runIdentity . flip evalStateT HM.empty . execWriterT


putOuts :: Bool -> Bool -> Collector -> OutSpecs -> MainM ()
putOuts pretend ovr (Collector hm) outDirs = do
   if ovr then do_ else checkDo
   when pretend $ lift $ TIO.putStrLn "Pretended to do everything but writing to outputs.."
   where
      outputter :: Outputter
      outputter = if pretend
         then (\ os raw -> DeepSeq.deepseq raw (return ()))
         else sinkToIO

      checkDo :: MainM ()
      checkDo = bool do_ error . or =<< mapM check outDirs

      do_ :: MainM ()
      do_ = mapM_ (putOut outputter hm) outDirs

      check :: OutSpec -> MainM Bool
      check = maybe (return False) (lift . Dir.doesFileExist) . getSinkFilePath
      error :: MainM a
      error = left "some files exist"

type Outputter = OutStream -> Raw -> IO ()

putOut :: Outputter -> CollectorR -> OutSpec -> MainM () 
putOut outputter hm dir = case dir of 
   Left (name, sink) -> maybe
      (left $ "Collector stream '"<>w2t name<>"' not defined")
      (lift . outputter sink)
      (HM.lookup name hm)
   Right sink -> case HM.toList hm of
      [(_, value)] -> lift $ outputter sink value
      [] -> left "No streams defined in a 'catch all to single sink' output"
      _  -> left "More than one stream defined in a 'catch all to single sink'"

sinkToIO :: OutStream -> Raw -> IO () 
sinkToIO sink raw = let 
      text = raw2text raw
   in case sink of
      StdOut -> TIO.putStr text
      StdErr -> TIO.hPutStr stderr text
      OutFile path -> TIO.writeFile path text



-- * Argument parsing

data Args = Args
   { debug     :: Bool        -- ^ Debug
   , overwrite :: Bool        -- ^ Allow owerwriting of files, default is no overwriting
   , pretend   :: Bool
   , baseDir   :: Maybe String  -- ^ Base directory for output files
   , oo        :: OutSpecs      -- ^ Output streams: files, stdout and stderr
   , ii        :: InSpecs       -- ^ Input streams: files and stdin

   -- TODO, implement these:
   -- , define :: U -- ^ Define macros on the commandline
   } deriving (Show)


optp :: O.ParserInfo Args
optp = O.info (O.helper <*> parser) desc
   where
      desc = O.fullDesc
         <> O.header "m5 -- a macro processor"
         <> O.progDesc ".. description .."
      parser = pure Args
         <*> sw 'd' "debug" "print debug info to stdout"
         <*> sw 'f' "overwrite" "Overwrite existing files"
         <*> sw 'p' "pretend" "Do everything short of writing/creating any outputs"
         <*> (O.optional
            $ str' 'b' "basedir" "Base directory for output files"
            $ O.metavar "DIR")
         <*> (O.many
                  $ either (error "parse oo fail") id
                  . parseOutSpec cfg
                  <$> str 'o' "oo" "Map output streams")
         <*> (O.many
               $ either (error "parse ii fail") id
               . parseInSpec
               <$> (O.argument O.str
                      $ O.metavar "input files or stream"
                     <> O.help "Base directory for output files" ))
      sw short long help = sw' short long help mempty
      str short long help = str' short long help mempty
      sw' short long help more = O.switch $
         O.short short <> O.long long <> O.help help <> more
      str' short long help more = O.strOption $ 
         O.short short <> O.long long <> O.help help <> more


-- | Parses command line mapping arguments to output mappings.
{- TODO:
   * ParserConf not used in parsing commandline, using it 
     only to reuse the content parsers. Should I get rid of
     this dependency?
   -}
parseOutSpec :: ParserConf -> String -> Either ParseError OutSpec
parseOutSpec cfg xs = cfgParse cfg outParser xs
   where
      outParser = ((,) <$> word <* gt <*> sink) <:|> sink
      gt = spacesP *> char '>'
      sink = spacesP *> sink'
      sink' = (oneOf "-0" *> return StdOut <* eof)
          <|> ( char '2'  *> return StdErr <* eof)
          <|> (OutFile <$> many anyChar <* eof)


-- * Stream direction

-- | A list of sources, where source is either stdin or file
type InSpecs = [Input]
data Input
   = StdIn
   | File FilePath
   deriving (Show)
instance Default InSpecs where def = [StdIn]


-- | Parses input specifiers, and gets their contents as a single text.
readInputs []  = left "Empty input list"
readInputs ins = T.concat <$> mapM readInp ins
   where readInp input = liftIO $ case input of
            StdIn -> TIO.getContents
            File path -> TIO.readFile path


parseInSpec :: String -> Either ParseError Input
parseInSpec str = return $ case str of
   "-" -> StdIn
   fp  -> File fp

-- | A map from stream name to an out-stream (bash) or to file. If
-- only one stream was defined in input, then having a sink is
-- enough -- everythint is put there.
type OutSpecs = [OutSpec]
type OutSpec = (Name, OutStream) :| OutStream
data OutStream
   = StdOut
   | StdErr
   | OutFile FilePath
   deriving (Show)

getSinkFilePath :: OutSpec -> Maybe FilePath
getSinkFilePath (Left (name, OutFile path)) = Just path
getSinkFilePath (Right (OutFile path)) = Just path
getSinkFilePath _ = Nothing

addBase :: FilePath -> OutSpecs -> OutSpecs
addBase base li = map f li
   where f :: OutSpec -> OutSpec
         f (Left (n, o)) = Left  (n, ab o)
         f (Right o)     = Right (ab o)
         ab :: OutStream -> OutStream
         ab (OutFile fp) = OutFile $ base <> "/" <> fp
         ab x = x

-- | Standard out is the default output map.
instance Default OutSpecs where def = [( Right StdOut )]


{- TODO
   * Generate warning when baseDir is something, but no file outputs
     have been defined.
   -}
