{-# LANGUAGE OverloadedStrings #-}

module Data.Makefile.Parse.Internal where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (void)
import           Data.Attoparsec.ByteString
import           Data.Makefile.Internal
import           Data.Word                        (Word8)

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString                  as B

-- $setup
-- >>> :set -XOverloadedStrings

-- | Parse makefile.
--
-- Tries to open and parse a file name @Makefile@ in the current directory.
parseMakefile :: IO (Either String Makefile)
parseMakefile = Atto.parseOnly makefile <$> B.readFile "Makefile"

-- | Parse the specified file as a makefile.
parseAsMakefile :: FilePath -> IO (Either String Makefile)
parseAsMakefile f = Atto.parseOnly makefile <$> B.readFile f

--------------------------------------------------------------------------------
-- Parsers


-- | Parser for a makefile
makefile :: Parser Makefile
makefile = Makefile <$> many' entry

-- | Parser for a makefile entry (either a rule or a variable assignment)
entry :: Parser Entry
entry = do
  many' emptyLine
  assignment <|> rule

-- | Parser of variable assignment
assignment :: Parser Entry
assignment = do v1 <- lazyVar <|> immVar
                v2 <- toLineEnd1
                return $ Assignment v1 v2

-- | Parser for an entire rule
rule :: Parser Entry
rule = do t <- target
          ds <- many' dependency
          nextLine
          cs <- many' command
          return $ Rule t ds cs

-- | Parser for a command
command :: Parser Command
command = do many' emptyLine
             Atto.char8 '\t'
             c <- Command <$> toLineEnd1
             nextLine
             return c

-- | Parser for a (rule) target
target :: Parser Target
target = do t <- Target <$> Atto.takeWhile (/= ':')
            Atto.char8 ':'
            return t

-- | Parser for a (rule) dependency
dependency :: Parser Dependency
dependency = do Atto.takeWhile isSpaceChar
                Dependency <$> Atto.takeWhile1 (`notElem` [' ', '\n', '#'])

-- | Parser for variable name in declaration (lazy set, @var = x@)
--
-- >>> Atto.parseOnly lazyVar "CFLAGS=-c -Wall"
-- Right "CFLAGS"
lazyVar :: Parser B.ByteString
lazyVar = do
  v1 <- Atto.takeWhile1 (`notElem` ['=', '\n', '#'])
  Atto.char8 '='
  return v1

-- | Parser for variable name in declaration (immediate set, @var := x@)
--
-- >>> Atto.parseOnly immVar "CFLAGS:=-c -Wall"
-- Right "CFLAGS"
immVar :: Parser B.ByteString
immVar = do
  v1 <- Atto.takeWhile1 (`notElem` [':', '\n', '#'])
  Atto.string ":="
  return v1

-- | Parser for a comment (the comment starts with the hashtag)
--
-- >>> Atto.parseOnly comment "# I AM A COMMENT"
-- Right " I AM A COMMENT"
comment :: Parser B.ByteString
comment = do Atto.char8 '#'
             Atto.takeWhile (/= '\n')

-- | Consume a newline character (@'\n'@)
nextLine :: Parser ()
nextLine = do Atto.takeWhile (/= '\n')
              void $ Atto.char8 '\n'

-- | Consume an empty line (potentially containing spaces and/or tabs).
--
-- >>> Atto.parseOnly emptyLine "\t\t   \t   \t\n"
-- Right ()
emptyLine :: Parser ()
emptyLine = do Atto.takeWhile (`elem` ['\t', ' '])
               many' comment
               Atto.char8 '\n'
               return ()

isSpaceChar :: Char -> Bool
isSpaceChar c = c == ' '

toLineEnd1 :: Parser B.ByteString
toLineEnd1 = Atto.takeWhile1 (`notElem` ['\n', '#'])

