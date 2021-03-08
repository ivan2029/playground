{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM_)

--
--
--
infixl 9 |>
(|>) = flip (.)

--
--
--
occurrenceCounts :: T.Text -> [(T.Text, Int)]
occurrenceCounts
  =  T.map filterChar
  |> T.toLower
  |> T.words
  |> foldr insertWord M.empty
  |> M.toList

filterChar :: Char -> Char
filterChar c
  | C.isAlpha c = c
  | otherwise   = ' '

insertWord :: T.Text -> M.Map T.Text Int -> M.Map T.Text Int
insertWord word map = M.insertWith (+) word 1 map 

--
--
--
example = "The quick brown fox jumps over the lazy dog!"

-- main = print $ occurrenceCounts example

main = forM_ (occurrenceCounts example) $ \(word, count) -> do
  let countTxt = T.pack $ show count
  let justifiedWord = T.justifyRight 10 ' ' word
  let msg = justifiedWord `T.append` " -> " `T.append` countTxt
  TIO.putStrLn msg
