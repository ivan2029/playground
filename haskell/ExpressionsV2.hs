{--
  Task described on https://www.hackerrank.com/challenges/expressions-v2

  No parsing library used! Would be much simpler with Parsec (or similar)
--}

module Main where

import Data.Char 
import Data.List

{--
 Expression ::= Term [+-] Expression
              | Term

 Term       ::= Factor [*/] Term
              | Factor

 Factor     ::= Number
              | [+-] Factor
              | '(' Expression ')'
--}

type ParseResult a = Either String (a, String)

showPR :: Show a => ParseResult a -> String
showPR (Left err) = "Parse failure: " ++ err
showPR (Right (res, rest)) = "Parse success, result is " ++ show res ++
                             ", unconsumed input: [" ++ rest ++ "]" 

number :: String -> ParseResult Integer
number [] = Left "EOI reached"
number input =
  let (digits, rest) = span isDigit input
  in case digits of
    [] -> Left $ "digit expected, got: " ++ take 10 input
    cs -> Right (read cs, rest)
                   
literal :: String -> String -> ParseResult String
literal lit input =
  case match' lit input of
    (True, rest) -> Right (lit, rest)
    _ -> Left $ "`" ++ lit ++ "` expected, got: " ++ take 10 input
  where
    match' [] cs = (True, cs)
    match' (x:xs) (c:cs) | x == c    = match' xs cs
                         | otherwise = (False, "") 

sign :: Char -> Integer
sign '+' = 1
sign '-' = -1
sing _ = error "unexpected sign glyph"

factor :: String -> ParseResult Integer
factor [] = Left "EOI reached"
factor input@(c:cs) | isDigit c = number input
                    | c == '+' || c == '-' = do
                        (num, rest_2) <- factor cs
                        return ((sign c)*num, rest_2)
                    | c == '(' = do
                        (res, rest_2) <- expr cs
                        (_, rest_3) <- literal ")" rest_2
                        return (res, rest_3)
                    | otherwise = Left $
                                  "expected digit, `+`, `-`, or `(`, got: " ++
                                  take 10 input

term :: String -> ParseResult Integer
term input = do
  (a, rest) <- factor input
  term' a rest
  where
    term' a [] = return (a, [])
    term' a rest@(c:cs) | c == '*' || c == '/' = do
                            (b, rest_1) <- term cs
                            let op = if c == '*' then (*) else div'
                            return (op a b, rest_1)
                        | otherwise = return (a, rest)
    
  

expr :: String -> ParseResult Integer
expr input = do
  (a, rest) <- term input
  expr' a rest
  where
    expr' a [] = return (a, [])
    expr' a rest@(c:cs) | c == '+' || c == '-'  = do
                            (b, rest_1) <- expr cs
                            let op = if c == '+' then (+) else (-)
                            return (op a b, rest_1)
                        | otherwise = return (a, rest)
  

magicNum :: Integer
magicNum = 7 + 10^9

correct :: Integer -> Integer
correct x = x `mod` magicNum

div' :: Integer -> Integer -> Integer
div' a b = a * (modPow b (magicNum - 2) magicNum)

modPow :: Integer -> Integer -> Integer -> Integer
modPow b e m = powm b e m 1

-- taken from rosetta code... 
powm b 0 m r = r
powm b e m r | e `mod` 2 == 1 =
                 powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r

eval :: String -> Either String Integer
eval input = fmap convert $ expr input
  where
    convert (x, _) = (x + magicNum) `mod` magicNum

--
-- TEST
--
--printPR :: Show a => ParseResult a -> IO ()
--printPR = putStrLn . showPR

main :: IO ()
main = do
  line <- getLine
  let input = filter (not . isSpace) line
  case eval input of
    Left err -> putStrLn err
    Right res -> putStrLn $ show $ res


