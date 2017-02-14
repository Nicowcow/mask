{-# LANGUAGE OverloadedStrings #-}

module Data.Makefile.Render.Internal where
import           Data.Makefile
import           Data.Monoid
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BL

writeMakefile :: FilePath -> Makefile -> IO ()
writeMakefile f m = do
  let s = encodeMakefile m
  BL.writeFile f s

encodeMakefile :: Makefile -> B.ByteString
encodeMakefile = toLazyByteString . renderMakefile

renderMakefile :: Makefile -> Builder
renderMakefile (Makefile es ) = mconcat [renderEntry e <> charUtf8 '\n' | e <- es]

renderEntry :: Entry -> Builder
renderEntry (Assignment key value ) = byteString key <> charUtf8 '=' <> byteString value
renderEntry (Rule (Target t) ds cmds) =
  byteString t <> charUtf8 ':' <>
  mconcat [charUtf8 ' ' <> renderDep d | d <- ds] <>
  charUtf8 '\n' <>
  mconcat [renderCmd cmd <> charUtf8 '\n' | cmd <- cmds]

renderDep :: Dependency -> Builder
renderDep (Dependency dep ) = byteString dep

renderCmd :: Command -> Builder
renderCmd (Command cmd ) = charUtf8 '\t' <> byteString cmd
