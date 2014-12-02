{-# LANGUAGE TypeOperators #-}
module M5.Helpers where

import Prelude2
import qualified Data.Text.IO as TIO
import qualified Data.Text    as T  

import Text.Parsec hiding ((<|>))
import qualified Text.Parsec as P

--
-- Helpers
--
getTest file = TIO.readFile ("tests/" <> file) :: IO T.Text
u = undefined


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

data U

pack = T.pack
tshow = pack . show
