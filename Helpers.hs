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
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Helpers where


import Prelude2
import qualified Data.Text.IO as TIO
import qualified Data.Text    as T  

import Text.Parsec

--
-- Helpers
--
getTest file = TIO.readFile ("tests/" <> file) :: IO T.Text
u = undefined


parseEither l r = try (Left <$> l) <|> (Right <$> r)
(<:|>) = parseEither 
infixl 5 <:|>

infixl 5 :|
type (:|) = Either
bimap f g e = case e of
   Left l -> Left $ f l
   Right r -> Right $ g r
bifmap f g e = case e of
   Left l -> Left <$> f l
   Right r -> Right <$> g r

data U
