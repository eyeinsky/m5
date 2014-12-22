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
import Data.Word (Word32)
import Numeric (showHex)
import Data.List (concat)
import Data.Either (either, lefts, rights, partitionEithers)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet      as HS

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

import Debug.Trace

import M5.Helpers
import M5.Types
import M5.Parse
import qualified M5.Expand as E



-- * Main

main = do
   args <- O.execParser optp
   case args of 
      Left mainArgs -> runMain $ do
   
         let rep :: forall a. Report a => a -> MainM ()
             rep a = mkRep (debug mainArgs) a
             mBase = baseDir mainArgs
             outSpecs = maybe id addBase mBase $ oo mainArgs :: [OutAs OutStream]
             inDirs = let i = ii mainArgs in if null i then def else i

         -- Check for existing files, if no overwriting allowed
         when (overwrite mainArgs) $ do
            b <- or <$> mapM (lift . checkFileExists) outSpecs
            when b $ left "Some files exist, quitting.."

         -- Read and evaluate inputs
         inText <- readInputs inDirs
         let parseResult = expandInput inText
         collector <- f "input contents" parseResult

         rep mainArgs >> rep inDirs >> rep outSpecs >>  rep inText
         rep =<< collectorToOutputText collector

         maybe (return ()) (lift . Dir.createDirectoryIfMissing True) mBase -- create directory

         let (tbl, unused) = combine collector outSpecs
         if pretend mainArgs
            then DeepSeq.deepseq (map (snd . snd) tbl) (return ())
            else mapM_ (lift . doRow) tbl

         rep tbl

         return ()

         where
            f :: T.Text -> Either ParseError a -> MainM a
            f intro (Left err) = left $ "Parse error in " <> intro <> tshow err
            f _     (Right res) = return res



type MainM = EitherT T.Text IO

runMain :: MainM () -> IO ()
runMain m = either err ignore =<< runEitherT m
   where
      err txt = TIO.putStrLn ("ERROR: "<> txt)
      ignore = const (return ())

-- ** Hardcoded parser configuration 

cfg = ParserConf "=" "=>" "\\"


-- * Debuging

mkRep dbg = if dbg then rep else const $ return ()
   where
      rep :: Report a => a -> MainM () 
      rep = lift . TIO.putStrLn . report

-- | The reporting (debugging) class, used when the debug flag is set
-- on the command line. 
class Report a where
   report :: a -> T.Text

dIndent = " - "
unl = T.intercalate "\n"
unc = T.intercalate ", "
instance Report [InSpec] where
   report li = "Inputs in order: " <> unc (map f li)
      where f StdIn = "stdin"
            f (File p) = "file '" <> T.pack p <> "'"
instance Report [OutAs OutStream] where
   report li = "Outputs in order: " <> unc (map h li)
      where f StdOut = "stdout"
            f StdErr = "stderr"
            f (OutFile p) = "file '" <> T.pack p <> "'"
            h = either ((\(a,b) -> a <>" > "<>b) . (w2t *** f)) ((">" <>) . f)
instance Report TextInstance where
   report (TextInstance rand txt) =
         "<<<" <> r <> "\n" <> txt <> "\n>>>" <> r
      where r = "["<> (T.pack $ showHex rand "]")
instance Report InputText where
   report (InputText ti) = "Input was:\n" <> report ti
instance Report MainArgs where
   report a = "Arguments: " <> tshow a

instance Report Collector where
   report (Collector m) = T.intercalate "\n" $ map f (HM.toList m) 
      where f (name, raw) = "=> " <> w2t name <> "\n" <> raw2text raw
instance Report OutputText where
   report (OutputText ti) = "Output was:\n" <> report ti

newtype NoSourceStream = NSS [Name]
instance Report NoSourceStream where
   report (NSS li) = "No source to output mappings: " <> T.intercalate ", " (w2t <$> li)

instance Report Tbl where
   report li = "Output table:\n" <> unl (map row li)
      where row (name, (outStream, raw)) = " - " <> w2t name <> ": " <> f outStream
            f StdOut = "stdout"
            f StdErr = "stderr"
            f (OutFile p) = "file '" <> pack p <> "'"


-- * Act on inputs and outputs

expandInput (InputText (TextInstance _ text)) = runM . E.expand <$> parseAst cfg text
   where runM = runIdentity . flip evalStateT HM.empty . execWriterT

-- | Output a row in the output table
doRow (name, (st, raw)) = f raw
   where f = streamToFunc st . raw2text

-- | The output table.
type Tbl = [(Name, (OutStream, Raw))]

-- | Combine collector with output specifications to a single output,
-- table plus unused streams.
combine :: Collector -> [OutAs OutStream] -> (Tbl, HM.HashMap Name Raw)
combine (Collector hm) li = let
      (nameds, anons) = partitionEithers li
      lookup = flip HM.lookup hm
   in if null anons
      then let  
         f (name, spec) tup@ (tbl', used') = maybe' (lookup name) tup
            (\raw -> ((name, (spec, raw)) : tbl', HS.insert name used'))
         tbl :: Tbl
         used :: HS.HashSet Name
         (tbl, used) = foldr f ([], HS.empty) nameds
         unused = foldr HM.delete hm $ HS.toList used
         in (tbl, unused)
      else error "anon def"

-- | Convert output specification to an IO function.
streamToFunc :: OutStream -> (T.Text -> IO ())
streamToFunc st = case st of
   StdOut -> TIO.putStr
   StdErr -> TIO.hPutStr stderr
   OutFile path -> TIO.writeFile path

-- | Check if the file specified already exists
checkFileExists :: OutAs OutStream -> IO Bool
checkFileExists = maybe (return False) Dir.doesFileExist . getSinkFilePath 



-- * Argument parsing


type Args = MainArgs :| MetaArgs
data MainArgs = MainArgs
      { debug     :: Bool        -- ^ Debug
      , debugOpts :: Maybe String
      , overwrite :: Bool        -- ^ Allow owerwriting of files, default is no overwriting
      , pretend   :: Bool
      , baseDir   :: Maybe String  -- ^ Base directory for output files
      , oo        :: [OutAs OutStream]      -- ^ Output streams: files, stdout and stderr
      , ii        :: [InSpec]       -- ^ Input streams: files and stdin

      {- TODO:
         implement define :: U -- ^ Define macros on the commandline
      -}
      }
   deriving (Show)

data MetaArgs = MetaArgs deriving (Show)


optp :: O.ParserInfo Args
optp = O.info (O.helper <*> (Left <$> optParser)) optProgDesc

optProgDesc = O.fullDesc
   <> O.header "m5 -- a macro processor"
   <> O.progDesc ".. description .."
optParser = pure MainArgs
   <*> sw 'd' "debug" "print debug info to stdout"
   <*> (O.optional $ str 'x' "debugopts" "Specify debug behavior")
   <*> sw 'f' "overwrite" "Overwrite existing files"
   <*> sw 'p' "pretend" "Do everything short of touching any outputs"
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
parseOutSpec :: ParserConf -> String -> Either ParseError (OutAs OutStream)
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
data InSpec
   = StdIn
   | File FilePath
   deriving (Show)
instance Default [InSpec] where def = [StdIn]


-- | Parses input specifiers, and gets their contents as a single text.

newtype OutputText = OutputText { fromOutputText :: TextInstance }
collectorToOutputText col = OutputText <$> mkTI (report col)

mkTI txt = TextInstance <$> lift randomIO <*> pure txt

newtype InputText = InputText { fromInputText :: TextInstance }
data TextInstance = TextInstance { stRand :: Word32, stText :: T.Text }
readInputs []  = left "Empty input list"
readInputs ins = InputText <$> (mkTI =<< (T.concat <$> mapM readInp ins))
   where readInp input = liftIO $ case input of
            StdIn -> TIO.getContents
            File path -> TIO.readFile path

parseInSpec :: String -> Either ParseError InSpec
parseInSpec str = return $ case str of
   "-" -> StdIn
   fp  -> File fp
{- ^ TODO: implement proper parsers. -}

type OutAction = Raw -> IO ()

type OutAs a = (Name, a) :| a
data OutStream
   = StdOut
   | StdErr
   | OutFile FilePath
   deriving (Show)



getSinkFilePath :: OutAs OutStream -> Maybe FilePath
getSinkFilePath (Left (name, OutFile path)) = Just path
getSinkFilePath (Right (OutFile path)) = Just path
getSinkFilePath _ = Nothing

addBase :: FilePath -> [OutAs OutStream] -> [OutAs OutStream]
addBase base li = map f li
   where f :: OutAs OutStream -> OutAs OutStream
         f (Left (n, o)) = Left  (n, ab o)
         f (Right o)     = Right (ab o)
         ab :: OutStream -> OutStream
         ab (OutFile fp) = OutFile $ base <> "/" <> fp
         ab x = x

-- | Standard out is the default output map.
instance Default [OutAs OutStream] where def = [( Right StdOut )]

{-
xxx = O.subparser
    ( O.command "meta" (O.info optParser optProgDesc)
   <> O.command "" (O.info optParser optProgDesc)
   )
-}
