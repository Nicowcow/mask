{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|

Module      : Data.Makefile
Copyright   : (c) 2016 Nicolas Mattia
License     : MIT
Maintainer  : Nicolas Mattia <nicolas@nmattia.com>
Stability   : experimental

This module defines the different types used when working with a Makefile.


@
# File: Makefile

hello = world

foo: bar
  baz
@

@
Makefile {
  entries =
    [ Assignment "hello " " world"
    , Rule (Target "foo") [Dependency "bar"] [Command "baz"] ]
    })
@

-}

module Data.Makefile where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (void)
import           Data.Attoparsec.ByteString
import           Data.String                      (IsString)
import           Data.Word                        (Word8)

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString                  as B




-- | Makefile target (@foo@ in the example above)
newtype Target = Target B.ByteString deriving (Show, Eq, IsString)

-- | Target dependency (@bar@ in the example above)
newtype Dependency = Dependency B.ByteString deriving (Show, Eq, IsString)

-- | Command (@baz@ in the example above)
newtype Command = Command B.ByteString deriving (Show, Eq, IsString)

-- | A makefile entry, either a rule @(target: dep1 dep1; commands)@ or a
-- variable assignment (@hello = world@ or @hello := world@)
data Entry = Rule Target [Dependency] [Command]
           | Assignment B.ByteString B.ByteString deriving (Show, Eq)

-- | A Makefile object, a list of makefile entries
data Makefile = Makefile { entries :: [Entry] } deriving (Show, Eq)
