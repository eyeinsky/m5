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
import Data.Either (either, lefts, rights)

import Control.Applicative ((*>),(<*), pure)
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Either

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashMap.Lazy as HM

import M5.Helpers
import M5.Types
import qualified M5.Parse as P
import qualified M5.Expand as E
import qualified M5.CmdArgs as C


main = either TIO.putStr (\_ -> return ()) =<< (runEitherT $ do
   args@ (C.Args strOuts strIns) <- lift C.cmdArgs'
   sw "Args from commandline: " args
   outDirs <- f "out directions" $ C.parseOuts strOuts 
   sw "Calculated out streams: " outDirs
   inText <- lift $ C.getConcatIns strIns
   sw "Input: " inText
   Output outHM <- f "input contents" $ expandInput inText
   mapM_  (sw "Outstream: ") $ HM.toList outHM
   mapM_ (putOut outHM) outDirs
   return ()

   )
   {-
   -}
   where f x = either (left . (("Parse error in " <> x) <>) . tshow) return
         sw intro = lift . TIO.putStrLn . (intro <>) . tshow

expandInput text = runM . E.expand <$> parse P.ast "<todo>" text
   where runM = runIdentity . flip evalStateT HM.empty . execWriterT


putOut hm dir = case dir of 
   Left (name, sink) -> maybe
      (left "Output stream '' not defined")
      (lift . sinkToIO sink)
      (HM.lookup name hm)
   Right sink -> case HM.toList hm of
      [(_, value)] -> lift $ sinkToIO sink value
      [] -> left "No streams defined in a 'catch all to single sink' output"
      _  -> left "More than one stream defined in a 'catch all to single sink'"
   where
      sinkToIO sink raw = let 
            text = raw2text raw
         in case sink of
            Left _ -> TIO.putStr text
            Right path -> TIO.writeFile path text
