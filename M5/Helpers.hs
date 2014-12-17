{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
module M5.Helpers where

import Prelude2
import qualified Data.Text.IO as TIO
import qualified Data.Text    as T  

import Control.Monad
import Text.Parsec hiding ((<|>))
import qualified Text.Parsec as P

import Data.Typeable
import Data.Data
--
-- Helpers
--
getTest file = TIO.readFile ("tests/" <> file) :: IO T.Text


parseEither l r = (Left <$> l) <|> (Right <$> r)
(<:|>) = parseEither 
infixl 5 <:|>

l <|> r = try l P.<|> r
infixr 1 <|>

infixl 5 :|
type (:|) = Either
bimap f g e = case e of
   Left l -> Left $ f l
   Right r -> Right $ g r
bifmap f g e = case e of
   Left l -> Left <$> f l
   Right r -> Right <$> g r

data U = U deriving (Typeable, Data, Show)
u = undefined

pack = T.pack
tshow = pack . show
