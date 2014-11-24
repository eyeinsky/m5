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

import qualified System.Console.CmdArgs as C

import M5.Helpers
import qualified M5.Parse as P
import M5.Types
import M5.Expand
import M5.CmdArgs

main = go =<< C.cmdArgs u

go (Args (In ins) (Out outs)) = do
   src <- T.concat <$> mapM getIn ins
   case parse P.ast "<todo>" src of
      Left err -> print err
      Right res -> let hm = runM $ expand res
         {- TODO: 
            * need to convert outs to HM
            * then unionWith it with the 'hm' that runM returned
            * change the Args type to two texts and parse them to HMs?
         -- (wrong:) in mapM_ (\(x, y) -> putOut (Right x) u) (HM.toList hm)
         -}
   u
{-
   txt <- TIO.getContents
   case parse ast "<stdin>" txt of
      Left err -> print err
      Right res -> let 
            raw = u -- expand HM.empty res
            text = raw2text raw
         in do
            print res
            print "-----"
            print raw
            print "-----"
            TIO.putStr text

-}
