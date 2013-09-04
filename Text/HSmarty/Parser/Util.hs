{-# LANGUAGE OverloadedStrings #-}
module Text.HSmarty.Parser.Util where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char
import Numeric (readHex)
import Prelude hiding (takeWhile)
import qualified Data.Text as T

eolP :: Parser T.Text
eolP =
    "\n" <$ (string "\r\n" <|> string "\n" <|> string "\r") <|>
    "" <$ endOfInput

boolP :: Parser Bool
boolP =
    const True <$> string "true" <|>
    const False <$> string "false"

stringP :: Parser T.Text
stringP = (quotedString '"' <|> quotedString '\'') <?> "stringP"

identP :: (Char -> Bool) -> (Char -> Bool) -> Parser T.Text
identP first rest =
    (T.cons <$> satisfy first <*> takeWhile rest) <?> "identP"

stripSpace = between optSpace_ optSpace_

space_ = skipWhile1 isSpace
optSpace_ = skipWhile isSpace

between :: Parser a -> Parser b -> Parser c -> Parser c
between left right main = left *> main <* right

skipWhile1 pred = (() <$ takeWhile1 pred) <?> "skipWhile1"

quotedString :: Char -> Parser T.Text
quotedString c = T.pack <$> between (char c) (char c) (many innerChar)
    where innerChar = char '\\' *> (escapeSeq <|> unicodeSeq)
                  <|> satisfy (`notElem` [c,'\\'])

escapeSeq :: Parser Char
escapeSeq = choice (zipWith decode "bnfrt\\\"'" "\b\n\f\r\t\\\"'")
    where decode c r = r <$ char c

unicodeSeq :: Parser Char
unicodeSeq = char 'u' *> (intToChar <$> decodeHexUnsafe <$> count 4 hexDigit)
    where intToChar = toEnum . fromIntegral

decodeHexUnsafe :: String -> Integer
decodeHexUnsafe hex = (head $ map fst $ readHex hex)

hexDigitUpper = satisfy (inClass "0-9A-F")
hexDigit = satisfy (inClass "0-9a-fA-F")

braced :: Parser l -> Parser r -> Parser a -> Parser a
braced l r =
    between (l *> optSpace_) (optSpace_ *> r)

listLike :: Parser l -> Parser r -> Parser s -> Parser a -> Parser [a]
listLike l r sep inner = braced l r (sepBy inner (stripSpace sep))

tupleP :: Parser p -> Parser [p]
tupleP = listLike (char '(') (char ')') (char ',')
