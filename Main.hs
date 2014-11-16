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
import M5.Helpers
import M5.Parse
import M5.Types
import M5.Expand

main = do
   txt <- TIO.getContents
   case parse ast "<stdin>" txt of
      Left err -> print err
      Right res -> let 
            raw = expand HM.empty res
            text = raw2text raw
         in do
            print res
            print "-----"
            print raw
            print "-----"
            TIO.putStr text

