module Data.Makefile
    ( target
    , dependency
    , command
    , rule
    , parseMakefile ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Attoparsec.ByteString
import Data.Word (Word8)

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString as B

type Target = B.ByteString
type Dependency = B.ByteString
type Command = B.ByteString
type Rule = (Target, [Dependency], [Command])
data Entry =  Rule Rule | Assignment B.ByteString B.ByteString deriving (Show)
type Makefile = [Entry]

parseMakefile :: IO (Either String Makefile)
parseMakefile = Atto.parseOnly makefile <$> B.readFile "Makefile"

assignment :: Parser Entry
assignment = do v1 <- Atto.takeWhile1 (/= '=')
                Atto.char8 '='
                v2 <- toLineEnd1
                return $ Assignment v1 v2

emptyLine :: Parser ()
emptyLine = do Atto.takeWhile (`elem` ['\t', ' '])
               Atto.char8 '\n'
               return ()

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
command = do Atto.char8 '\t'
             c <- toLineEnd1
             nextLine
             return c

rule :: Parser Entry
rule = do t <- target
          ds <- many' dependency
          nextLine
          cs <- many' command
          return $ Rule (t, ds, cs)
