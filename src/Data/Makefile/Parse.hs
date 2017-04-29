{-# LANGUAGE OverloadedStrings #-}

module Data.Makefile.Parse
  ( I.parseMakefile
  , I.parseAsMakefile
  , I.parseMakefileContents
  , I.makefile
  , I.entry
  , I.assignment
  , I.variableName
  , I.assignmentType
  , I.rule
  , I.command
  , I.target
  , I.dependency
  , I.comment
  , I.toEscapedLineEnd
  ) where

import qualified Data.Makefile.Parse.Internal as I
