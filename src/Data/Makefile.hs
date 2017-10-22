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
    [ Assignment RecursiveAssign "hello" "world"
    , Rule (Target "foo") [Dependency "bar"] [Command "baz"]
    ]
  }
@

-}

module Data.Makefile where

import           Data.String                      (IsString)

import qualified Data.Text as T


-- | A Makefile object, a list of makefile entries
data Makefile = Makefile { entries :: [Entry] } deriving (Show, Read, Eq)

-- | A makefile entry, either a rule @(target: dep1 dep1; commands)@ or a
-- variable assignment (@hello = world@ or @hello := world@)
data Entry = Rule Target [Dependency] [Command]
           | Assignment AssignmentType T.Text T.Text
           | OtherLine T.Text
           -- ^ Catch all value for comments, empty lines and lines that failed
           -- to parse.
           deriving (Show, Read, Eq)

data AssignmentType
  = RecursiveAssign
    -- ^ foo = bar
  | SimpleAssign
    -- ^ foo := bar
  | SimplePosixAssign
    -- ^ foo ::= bar
  | ConditionalAssign
    -- ^ foo ?= bar
  | ShellAssign
    -- ^ foo != bar
  | AppendAssign
    -- ^ foo += bar
  deriving (Show, Read, Eq, Enum, Bounded)

-- | Makefile target (@foo@ in the example above)
newtype Target = Target T.Text deriving (Show, Read, Eq, IsString)

-- | Target dependency (@bar@ in the example above)
newtype Dependency = Dependency T.Text deriving (Show, Read, Eq, IsString)

-- | Command (@baz@ in the example above)
newtype Command = Command T.Text deriving (Show, Read, Eq, IsString)
