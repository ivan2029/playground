import Data.Char (isDigit, isSpace)
import Text.Printf (printf)

data Token = TokNum Int
           | TokBinOp BinOp
           | TokLpar
           | TokRpar  
           deriving(Show)

data BinOp = OpPow | OpMul | OpDiv | OpPlus | OpMinus deriving(Show, Eq, Ord)

data Expr = ENum Int
          | EBinOp BinOp Expr Expr
          deriving(Show)

prettyPrint :: Expr -> String
prettyPrint (ENum num) = show num
prettyPrint (EBinOp op lhs rhs) =
  let op' = toChar op
      lhs' = prettyPrint lhs
      rhs' = prettyPrint rhs
  in printf "(%s %s %s)" lhs' op' rhs'
  where
    toChar OpPow   = "^"
    toChar OpMul   = "*"
    toChar OpDiv   = "/"
    toChar OpPlus  = "+"
    toChar OpMinus = "-"

isSpec :: Char -> Bool
isSpec c = elem c "^*/+-()"

toSpec :: Char -> Token
toSpec '^' = TokBinOp OpPow
toSpec '*' = TokBinOp OpMul
toSpec '/' = TokBinOp OpDiv
toSpec '+' = TokBinOp OpPlus
toSpec '-' = TokBinOp OpMinus
toSpec '(' = TokLpar
toSpec ')' = TokRpar
toSpec c   = error (printf "invalid special character %c" c)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c:cs) | isSpec c  = toSpec c : tokenize cs
                | isDigit c = readNum (c:cs)
                | isSpace c = tokenize cs
                | otherwise = error (printf "unexpected next char: %s" (take 10 (c:cs)))
  where
    readNum ls = let (num, rest) = span isDigit ls
                 in TokNum (read num) : tokenize rest

shuntingYard :: [BinOp] -> [Expr] -> [([BinOp],[Expr])] -> [Token] -> Expr
shuntingYard [] [expr] [] [] = expr
shuntingYard ops exs sub [] = case (ops, exs) of
  (o:os, rhs:lhs:rest) -> shuntingYard os (EBinOp o lhs rhs : rest) sub []
  _ -> error (printf "invalid state: %s %s" (show ops) (show exs))
shuntingYard ops exs sub (TokNum num : toks) = let exs' = ENum num : exs
                                               in shuntingYard ops exs' sub toks
shuntingYard ops exs sub (TokBinOp op : toks) = case (ops, exs) of
  ([], exs) -> shuntingYard [op] exs sub toks
  (o:os, rhs:lhs:rest) | o > op -> shuntingYard (op:ops) exs sub toks
                       | otherwise -> shuntingYard (op:os) (EBinOp o lhs rhs : rest) sub toks
  _ -> error (printf "invalid state %s %s" (show ops) (show exs))
shuntingYard ops exs sub (TokLpar : toks) = shuntingYard [] [] ((ops, exs):sub) toks
shuntingYard ops exs sub (TokRpar : toks) = case sub of
  ((ops', exs'):sub') -> let expr = shuntingYard ops exs [] []
                         in shuntingYard ops' (expr:exs') sub' toks
  _ -> error (printf "invalid state %s %s" (show ops) (show exs))


parse :: String -> Expr
parse = shuntingYard [] [] [] . tokenize

{--
  test
--}

main :: IO ()
main = putStrLn $ prettyPrint $ parse "32 + 2*3 + 4*(1 + (4 + 3)*2)"
