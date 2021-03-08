
import qualified Data.Map as M

{--

find longes subsequence of 2 sequences

abazdc bacbad  -> abad
aggtab gxtxayb -> gtab
aaaa   aa      -> aa

--}

{-- not memoized!!! -}
longestSubseq :: Eq a => [a] -> [a] -> [a]
longestSubseq _  [] = []
longestSubseq [] _  = []
longestSubseq (x:xs) (y:ys) 
  | x == y    = x : longestSubseq xs ys 
  | otherwise = 
    let s1 = longestSubseq xs     (y:ys)
        s2 = longestSubseq (x:xs) ys    
    in if length s1 > length s2 then s1 else s2
    
    
{-- --}
longestSubseqMemoized ::  (Ord a, Eq a) => [a] -> [a] -> [a]
longestSubseqMemoized xs ys = 
  let (res, _) = impl M.empty xs ys
  in res
  where
    --
    implCall map xs ys =
      case M.lookup (xs, ys) map of
        Just x  -> (x, map)
        Nothing -> impl map xs ys
    --
    impl map []  _ = ([], map)
    impl map _  [] = ([], map)
    impl map (x:xs) (y:ys) 
      | x == y    = 
        let (xs1, map1) = implCall map xs ys
        in (x:xs1, map1)
      | otherwise =
        let (s1, map1) = implCall map xs     (y:ys)
            (s2, map2) = implCall map (x:xs) ys
        in 
          if length s1 > length s2 
          then (s1, map1) 
          else (s2, map2)

{-- --}    
test :: (String -> String -> String) -> IO ()
test solve = do
  putStrLn "expected: abad, gtab, aa"
  putStrLn "results:"
  putStrLn $ solve "abazdc" "bacbad"
  putStrLn $ solve "aggtab" "gxtxayb"
  putStrLn $ solve "aaaa"   "aa"

main = do
  test longestSubseq
  putStrLn "-----------------------------"
  test longestSubseqMemoized