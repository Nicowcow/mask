{-# LANGUAGE OverloadedStrings #-}

module Data.Makefile.Parse.Internal where

import Control.Monad
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
    <*> toEscapedLineEnd -- toEscapedLineEnd strips

-- | Parser for an entire rule
rule :: Parser Entry
rule =
  Rule
    <$> target
    <*> many' dependency
    <*> many' (many' emptyLine *> command)

-- | Parser for a command
command :: Parser Command
command = Command <$> (Atto.char '\t' *> toEscapedLineEnd)

-- | Parser for a (rule) target
target :: Parser Target
target = Target <$> stripped (Atto.takeWhile (/= ':') <* Atto.char ':')

-- | Parser for a (rule) dependency
dependency :: Parser Dependency
dependency = Dependency <$> (sameLine <|> newLine)
  where
    sameLine =
      Atto.takeWhile (== ' ')
        *> Atto.takeWhile1 (`notElem` [' ', '\n', '#', '\\'])
    newLine =
      Atto.takeWhile (== ' ')
        *> Atto.char '\\'
        *> Atto.char '\n'
        *> (sameLine <|> newLine)

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

toLineEnd :: Parser T.Text
toLineEnd = Atto.takeWhile (`notElem` ['\n', '#'])

-- | Get the contents until the end of the (potentially multi) line. Multiple
-- lines are separated by a @\\@ char and individual lines will be stripped and
-- spaces will be interspersed.
--
-- >>> Atto.parseOnly toEscapedLineEnd "foo bar \\\n baz"
-- Right "foo bar baz"
--
-- >>> Atto.parseOnly toEscapedLineEnd "foo \t\\\n bar \\\n baz \\\n \t"
-- Right "foo bar baz"
toEscapedLineEnd :: Parser T.Text
toEscapedLineEnd = (T.unwords . filter (not . T.null)) <$> go
  where
    go = do
      l <- toLineEnd <* (void (Atto.char '\n') <|> pure ())
      case T.stripSuffix "\\" l of
        Nothing -> return [T.strip l]
        Just l' -> (T.strip l':) <$> go

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

stripped :: Parser T.Text -> Parser T.Text
stripped = fmap T.strip
