{-|

Module      : Data.Makefile
Copyright   : (c) 2016 Nicolas Mattia
License     : MIT
Maintainer  : Nicolas Mattia <nicolas@nmattia.com>
Stability   : experimental

-}
module Data.Makefile
    ( M.Target(..)
    , M.Dependency(..)
    , M.Command(..)
    , M.Entry (..)
    , M.Makefile (..) ) where

import qualified Data.Makefile.Internal as M
