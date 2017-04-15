{-# LANGUAGE OverloadedStrings #-}

module Data.Makefile.Parse.Internal where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.Text
import           Data.Makefile

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- $setup
-- >>> :set -XOverloadedStrings

-- | Parse makefile.
--
-- Tries to open and parse a file name @Makefile@ in the current directory.
parseMakefile :: IO (Either String Makefile)
parseMakefile = Atto.parseOnly makefile <$> T.readFile "Makefile"

-- | Parse the specified file as a makefile.
parseAsMakefile :: FilePath -> IO (Either String Makefile)
parseAsMakefile f = Atto.parseOnly makefile <$> T.readFile f

parseMakefileContents :: T.Text -> Either String Makefile
parseMakefileContents = Atto.parseOnly makefile

--------------------------------------------------------------------------------
-- Parsers


-- | Parser for a makefile
makefile :: Parser Makefile
makefile = Makefile <$> many' entry

-- | Parser for a makefile entry (either a rule or a variable assignment)
entry :: Parser Entry
entry = many' emptyLine *> (assignment <|> rule)

-- | Parser of variable assignment (see 'Assignment'). Note that leading and
-- trailing whitespaces will be stripped both from the variable name and
-- assigned value.
--
-- >>> Atto.parseOnly assignment "foo = bar "
-- Right (Assignment "foo" "bar")
assignment :: Parser Entry
assignment =
  Assignment
    <$> stripped (lazyVar <|> immVar)
    <*> stripped toLineEnd

-- | Parser for an entire rule
rule :: Parser Entry
rule =
  Rule
    <$> target
    <*> ((many' dependency <* nextLine) <|> pure [])
    <*> many' command

-- | Parser for a command
command :: Parser Command
command = Command <$> (many' emptyLine *> Atto.char '\t'
                                       *> toLineEnd
                                       <* nextLine)

-- | Parser for a (rule) target
target :: Parser Target
target = Target <$> stripped (Atto.takeWhile (/= ':') <* Atto.char ':')

-- | Parser for a (rule) dependency
dependency :: Parser Dependency
dependency = Dependency <$> (Atto.takeWhile isSpaceChar
                         *> Atto.takeWhile1 (`notElem` [' ', '\n', '#']))

-- | Parser for variable name in declaration (lazy set, @var = x@)
--
-- >>> Atto.parseOnly lazyVar "CFLAGS=-c -Wall"
-- Right "CFLAGS"
lazyVar :: Parser T.Text
lazyVar = Atto.takeWhile1 (`notElem` ['=', '\n', '#']) <* Atto.char '='

-- | Parser for variable name in declaration (immediate set, @var := x@)
--
-- >>> Atto.parseOnly immVar "CFLAGS:=-c -Wall"
-- Right "CFLAGS"
immVar :: Parser T.Text
immVar = Atto.takeWhile1 (`notElem` [':', '\n', '#']) <* Atto.string ":="

-- | Parser for a comment (the comment starts with the hashtag)
--
-- >>> Atto.parseOnly comment "# I AM A COMMENT"
-- Right " I AM A COMMENT"
comment :: Parser T.Text
comment = Atto.char '#' *> Atto.takeWhile (/= '\n')

-- | Consume a newline character (@'\n'@)
nextLine :: Parser ()
nextLine = Atto.takeWhile (/= '\n') *> Atto.char '\n' *> pure ()

-- | Consume an empty line (potentially containing spaces and/or tabs).
--
-- >>> Atto.parseOnly emptyLine "\t\t   \t   \t\n"
-- Right ()
emptyLine :: Parser ()
emptyLine = Atto.takeWhile (`elem` ['\t', ' ']) *>
            many' comment *>
            Atto.char '\n' *>
            pure ()

isSpaceChar :: Char -> Bool
isSpaceChar c = c == ' '

toLineEnd :: Parser T.Text
toLineEnd = Atto.takeWhile (`notElem` ['\n', '#'])

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

stripped :: Parser T.Text -> Parser T.Text
stripped = fmap T.strip
