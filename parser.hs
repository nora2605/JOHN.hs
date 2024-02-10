module JOHN where

import Control.Applicative
import Data.Char
import Numeric
import Data.Maybe
import Data.Tuple

main :: IO () 
main = undefined

data JohnValue
  = JohnAbyss
  | JohnBool Bool
  | JohnInt Integer
  | JohnFloat Double
  | JohnString String
  | JohnChar Char
  | JohnRange Integer Integer
  | JohnRangeStep Integer Integer Integer
  | JohnVersion [Integer]
  | JohnIndex Bool Integer
  | JohnArray [JohnValue]
  | JohnObject [(String, JohnValue)]
  -- JohnTuple
  deriving (Show, Eq)

newtype Parser t = Parser { runParser :: String -> Maybe (String, t) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

spanP :: (Char -> Bool) -> Parser String
spanP = many . parseIf

spanP1 :: (Char -> Bool) -> Parser String
spanP1 = some . parseIf

parseIf :: (Char -> Bool) -> Parser Char
parseIf f = Parser $ \input ->
  case input of
    x:xs | f x -> Just (xs, x)
    _ -> Nothing

charP :: Char -> Parser Char
charP c = Parser f
  where
    f (x:xs)
      | x == c = Just (xs, c)
      | otherwise = Nothing
    f [] = Nothing

consume :: String -> Parser String
consume = sequenceA . map charP

johnSep :: Parser Char
johnSep = ((ws *> (charP ',' <|> charP ';') <* ws) <|> (charP ' ' <* ws))

johnPair :: Parser (String, JohnValue)
johnPair = (\key _ value -> (key, value)) <$> fieldName <*> ((johnSep <|> (ws *> charP ':')) *> ws) <*> johnValue
  where fieldName = (++) <$> (spanP (\c -> isAlpha c || c == '_')) <*> (spanP (\c -> isAlphaNum c || c == '_'))

ws :: Parser String
ws = spanP isSpace

hexChar :: Parser Char
hexChar = (\a -> f (readHex a)) <$> sequenceA (replicate 4 (parseIf isHexDigit))
  where f (x:xs) = chr (fst x)
        f [] = undefined

escapeChar :: Parser Char
escapeChar =  ('"' <$ consume "\\\"") <|>
              ('\\' <$ consume "\\\\") <|>
              ('/' <$ consume "\\/") <|>
              ('\b' <$ consume "\\b") <|>
              ('\f' <$ consume "\\f") <|>
              ('\n' <$ consume "\\n") <|>
              ('\r' <$ consume "\\r") <|>
              ('\t' <$ consume "\\t") <|>
              (consume "\\u" *> hexChar)

normalChar :: Parser Char
normalChar = parseIf ((&&) <$> (/= '"') <*> (/= '\\'))

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> (many (sep *> element) <* (many sep)) <|> pure []

sepByNotNull :: Parser a -> Parser b -> Parser [b]
sepByNotNull sep element = (:) <$> element <*> (many (sep *> element) <* (many sep))

johnAbyss :: Parser JohnValue
johnAbyss = (\_ -> JohnAbyss) <$> (consume "#" <|> consume "abyss")

johnBool :: Parser JohnValue
johnBool = f <$> (consume "true" <|> consume "false")
  where f "true"  = JohnBool True
        f "false" = JohnBool False
        f _       = undefined -- This should never happen :0

johnInt :: Parser JohnValue
johnInt = JohnInt <$> intP

johnFloat :: Parser JohnValue
johnFloat = JohnFloat <$> (read <$> ((\a b c d -> concat [a, b, c, d]) <$> consume "-" <*> spanP1 isDigit <*> consume "." <*> spanP1 isDigit))

intP :: Parser Integer
intP = read <$> ((:) <$> charP '-' <*> (spanP1 isDigit) <|> spanP1 isDigit)

johnString :: Parser JohnValue
johnString = JohnString <$> (charP '"' *> many (normalChar <|> escapeChar) <* charP '"')

-- no safety checks for the strict one character limit
johnChar :: Parser JohnValue
johnChar = JohnChar <$> (charP '\'' *> (normalChar <|> escapeChar) <* charP '\'')

johnVersion :: Parser JohnValue
johnVersion = JohnVersion <$> (charP 'v' *> (sepByNotNull (charP '.') intP))

johnRange :: Parser JohnValue
johnRange = (\a _ b -> JohnRange a b) <$> intP <*> (consume ".." *> ws) <*> intP

johnRangeStep :: Parser JohnValue
johnRangeStep = (\a _ b _ c -> JohnRangeStep a b c) <$> intP <*> (consume ".." *> ws) <*> intP <*> (consume ".." *> ws) <*> intP

-- ^number or *number
johnIndex :: Parser JohnValue
johnIndex = JohnIndex <$> (consume "^" *> pure True <|> consume "*" *> pure False) <*> intP

johnArray :: Parser JohnValue
johnArray = JohnArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where elements = sepBy johnSep johnValue

-- Tuple not feasable in Haskell :/
johnTuple :: Parser JohnValue
johnTuple = JohnArray <$> (charP '(' *> ws *> elements <* ws <* charP ')')
  where elements = sepBy johnSep johnValue

johnObject :: Parser JohnValue
johnObject = JohnObject <$> (charP '{' *> ws *> sepBy johnSep johnPair <* ws <* charP '}')

johnTLObject :: Parser JohnValue
johnTLObject = JohnObject <$> (ws *> sepByNotNull johnSep johnPair <* ws)

johnValue :: Parser JohnValue
johnValue = ws *> johnBool <|> johnAbyss <|> johnIndex <|> johnFloat <|> johnRange <|> johnRangeStep <|> johnInt <|> johnString <|> johnChar <|> johnArray <|> johnObject <|> johnTuple <|> johnVersion

parseJohn :: String -> Maybe JohnValue
parseJohn input = snd <$> runParser (ws *> (johnValue <|> johnTLObject)) input