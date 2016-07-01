module Data.Makefile.Internal where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (void)
import           Data.Attoparsec.ByteString
import           Data.Word                        (Word8)

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString                  as B

type Target = B.ByteString
type Dependency = B.ByteString
type Command = B.ByteString
type Rule = (Target, [Dependency], [Command])
data Entry = Rule Rule
           | Assignment B.ByteString B.ByteString deriving (Show)
type Makefile = [Entry]
