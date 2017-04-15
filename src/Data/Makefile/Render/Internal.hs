{-# LANGUAGE OverloadedStrings #-}

module Data.Makefile.Render.Internal where
import           Data.Makefile
import           Data.Monoid
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Lazy.Builder

writeMakefile :: FilePath -> Makefile -> IO ()
writeMakefile f m = do
  let s = encodeMakefile m
  TL.writeFile f s

encodeMakefile :: Makefile -> TL.Text
encodeMakefile = toLazyText . renderMakefile

renderMakefile :: Makefile -> Builder
renderMakefile (Makefile es ) = mconcat [renderEntry e <> singleton '\n' | e <- es]

renderEntry :: Entry -> Builder
renderEntry (Assignment RecursiveAssign key value ) =
  fromText key <> singleton '=' <> fromText value
renderEntry (Assignment SimpleAssign key value ) =
  fromText key <> fromText ":=" <> fromText value
renderEntry (Assignment SimplePosixAssign key value ) =
  fromText key <> fromText "::=" <> fromText value
renderEntry (Assignment ConditionalAssign key value ) =
  fromText key <> fromText "?=" <> fromText value
renderEntry (Assignment ShellAssign key value ) =
  fromText key <> fromText "!=" <> fromText value
renderEntry (Assignment AppendAssign key value ) =
  fromText key <> fromText "+=" <> fromText value
renderEntry (Rule (Target t) ds cmds) =
  fromText t <> singleton ':' <>
  mconcat [singleton ' ' <> renderDep d | d <- ds] <>
  singleton '\n' <>
  mconcat [renderCmd cmd <> singleton '\n' | cmd <- cmds]

renderDep :: Dependency -> Builder
renderDep (Dependency dep ) = fromText dep

renderCmd :: Command -> Builder
renderCmd (Command cmd ) = singleton '\t' <> fromText cmd
