{-# LANGUAGE OverloadedStrings #-}

module Data.Makefile.Parse
  ( I.parseMakefile
  , I.parseAsMakefile
  , I.makefile
  , I.entry
  , I.assignment
  , I.rule
  , I.command
  , I.target
  , I.dependency
  , I.lazyVar
  , I.immVar
  , I.comment) where

import qualified Data.Makefile.Parse.Internal as I
