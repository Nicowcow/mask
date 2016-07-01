{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Makefile.Internal where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (void)
import           Data.Attoparsec.ByteString
import           Data.Word                        (Word8)
import Data.String (IsString)

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString                  as B

newtype Target = Target B.ByteString deriving (Show, Eq, IsString)
newtype Dependency = Dependency B.ByteString deriving (Show, Eq, IsString)
newtype Command = Command B.ByteString deriving (Show, Eq, IsString)
data Entry = Rule Target [Dependency] [Command]
           | Assignment B.ByteString B.ByteString deriving (Show, Eq)
data Makefile = Makefile { entries :: [Entry] } deriving (Show, Eq)
