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
import Data.Either (either, lefts, rights, partitionEithers)
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

main = do
   x <- O.execParser optp
   case x of 
      Left args -> runMain $ do
   
         let sw :: (Show a) => (T.Text -> T.Text) -> a -> EitherT T.Text IO ()
             sw = mkDbg (debug args)
             mBase = baseDir args
             outSpecs = maybe id addBase mBase $ oo args :: OutSpecs
             inDirs = let i = ii args in if null i then def else i

         outActSpec <- prepareOuts (overwrite args) (pretend args) outSpecs

         sw ("Arguments: " <>) args
         sw ("Calculated in streams: " <>) inDirs
         sw ("Calculated out streams: " <>) outSpecs

         inText <- readInputs inDirs
         rand <- tshow <$> lift (randomIO :: IO Word64)
         sw (\x -> "Input was:\n<<< " <> rand <> "\n"
                <> x <> ">>> " <> rand)
            inText
         
         result <- f "input contents" $ expandInput inText
         mapM_ (sw ("Outstream: "<>)) $ HM.toList $ fromCollector result
         
         maybe (return ()) (lift . Dir.createDirectoryIfMissing True) mBase -- create directory

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


-- 

combine :: CollectorR -> [OutActionSpec] -> MainM ()
combine hm li = let
      lifted = ([],) <$> hm :: HM.HashMap Word ([OutActionSpec],Raw)
      (nameds, anons) = partitionEithers li
   in if not (null anons)
      then u
      else u
   {-
   Left (name, sink) -> maybe
      (left $ "Collector stream '"<>w2t name<>"' not defined")
      (lift . outputter sink)
      (HM.lookup name hm)
   Right sink -> case HM.toList hm of
      [(_, value)] -> lift $ outputter sink value
      [] -> left "No streams defined in a 'catch all to single sink' output"
      _  -> left "More than one stream defined in a 'catch all to single sink'"
      -}
   where
      x=x


-- ** Prepare output streams

-- | Evaluate OutSpecs to OutActionSpec. If overwrite is False
-- then return early with the list of files that do.
prepareOuts :: Bool -> Bool -> [OutSpec] -> MainM [OutActionSpec]
prepareOuts ovr pret specs = do
   (existing, rest) <- partitionEithers <$> (lift $ mapM f specs)
   if null existing
      then return rest
      else left $ "Files exist: " <> tshow existing
   where 
      g = specToAction ovr pret
      f :: OutSpec -> IO (Either FilePath OutActionSpec)
      f (Left (name, streamDef)) = do 
         e <- g streamDef
         return $ case e of
            Left fp -> Left fp
            Right act -> Right (Left (name, act))
      f (Right streamDef) = do
         e <- g streamDef
         return $ case e of
            Left fp -> Left fp
            Right act -> Right (Right act)

-- | Converts an output destination to either a Left if
-- overwrite is on and file exists. Otherwise returns a
-- function that outputs the 'Raw' to its destination.
specToAction
   :: Bool                       -- ^ whether to overwrite
   -> Bool                       -- ^ whether to pretend
   -> OutStream                  -- ^ outstream, i.e sink
   -> IO (FilePath :| OutAction) -- ^ Either file exists or write action
specToAction ovr pretend sink = case sink of
   StdOut -> ret $ TIO.putStr . raw2text
   StdErr -> ret $ TIO.hPutStr stderr . raw2text
   OutFile path -> let write = handlePretend (TIO.writeFile path . raw2text)
      in if ovr
         then rr write
         else bool (Left path) (Right write) <$> Dir.doesFileExist path
   where
      rr = return . Right 
      ret act = return $ Right $ handlePretend act
      handlePretend doReally raw = if pretend
         then DeepSeq.deepseq raw (return ())
         else doReally raw
      




-- * Argument parsing


type Args = MainArgs :| MetaArgs
data MainArgs = MainArgs
      { debug     :: Bool        -- ^ Debug
      , debugOpts :: Maybe String
      , overwrite :: Bool        -- ^ Allow owerwriting of files, default is no overwriting
      , pretend   :: Bool
      , baseDir   :: Maybe String  -- ^ Base directory for output files
      , oo        :: OutSpecs      -- ^ Output streams: files, stdout and stderr
      , ii        :: InSpecs       -- ^ Input streams: files and stdin

      {- TODO:
         implement define :: U -- ^ Define macros on the commandline
      -}
      }
   deriving (Show)

data MetaArgs = MetaArgs


optp :: O.ParserInfo Args
optp = O.info (O.helper <*> (Left <$> optParser)) optProgDesc

optProgDesc = O.fullDesc
   <> O.header "m5 -- a macro processor"
   <> O.progDesc ".. description .."
optParser = pure MainArgs
   <*> sw 'd' "debug" "print debug info to stdout"
   <*> (O.optional $ str 'x' "debugopts" "Specify debug behavior")
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
   where
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
type OutSpecs  = [OutSpec]
type OutSpec   = MkOut OutStream
type OutActionSpec = MkOut OutAction

type OutAction = Raw -> IO ()

type MkOut a = (Name, a) :| a
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

{-
xxx = O.subparser
    ( O.command "meta" (O.info optParser optProgDesc)
   <> O.command "" (O.info optParser optProgDesc)
   )
-}


