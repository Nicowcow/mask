{-# LANGUAGE OverloadedStrings #-}

module Data.Makefile.Parse where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (void)
import           Data.Attoparsec.ByteString
import           Data.Makefile.Internal
import           Data.Word                        (Word8)

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString                  as B

parseMakefile :: IO (Either String Makefile)
parseMakefile = Atto.parseOnly makefile <$> B.readFile "Makefile"

parseAsMakefile :: FilePath -> IO (Either String Makefile)
parseAsMakefile f = Atto.parseOnly makefile <$> B.readFile f

assignment :: Parser Entry
assignment = do v1 <- desc1 <|> desc2
                v2 <- toLineEnd1
                return $ Assignment v1 v2

desc1 :: Parser B.ByteString
desc1 = do v1 <- Atto.takeWhile1 (`notElem` ['=', '\n', '#'])
           Atto.char8 '='
           return v1

desc2 :: Parser B.ByteString
desc2 = do v1 <- Atto.takeWhile1 (`notElem` [':', '\n', '#'])
           Atto.string ":="
           return v1

emptyLine :: Parser ()
emptyLine = do Atto.takeWhile (`elem` ['\t', ' '])
               many' comment
               Atto.char8 '\n'
               return ()

comment :: Parser B.ByteString
comment = do Atto.char8 '#'
             Atto.takeWhile (/= '\n')

nextLine :: Parser ()
nextLine = do Atto.takeWhile (/= '\n')
              void $ Atto.char8 '\n'

block :: Parser Entry
block = do  many' emptyLine
            assignment <|> rule

makefile :: Parser Makefile
makefile = many' block

isSpaceChar :: Char -> Bool
isSpaceChar c = c == ' '

target :: Parser Target
target = do t <- Atto.takeWhile (/= ':')
            Atto.char8 ':'
            return t

toLineEnd1 :: Parser B.ByteString
toLineEnd1 = Atto.takeWhile1 (`notElem` ['\n', '#'])

dependency :: Parser Dependency
dependency = do Atto.takeWhile isSpaceChar
                Atto.takeWhile1 (`notElem` [' ', '\n', '#'])

command :: Parser Command
command = do many' emptyLine
             Atto.char8 '\t'
             c <- toLineEnd1
             nextLine
             return c

rule :: Parser Entry
rule = do t <- target
          ds <- many' dependency
          nextLine
          cs <- many' command
          return $ Rule (t, ds, cs)
