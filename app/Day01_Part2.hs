
module Main where

import qualified Data.Set as Set
import Debug.Trace

main :: IO ()
main = interact mySolution

toInt :: String -> Int
toInt = read . filter (/= '+')

-- |
-- >>> let (n:ns) = cycle [3,3,4,(-2),(-4)] in findRepeat n ns Set.empty
-- 10

findRepeat :: Int -> [Int] -> Set.Set Int -> Int
findRepeat l (n:ns) freqs
  | Set.member next freqs = next 
  | otherwise = findRepeat next ns (Set.insert next freqs)
  where next = n + l

mySolution :: String -> String
mySolution inputStr = show $ findRepeat n ns Set.empty
  where (n:ns) = cycle $ fmap toInt $ lines inputStr

testIt :: String -> IO ()
testIt filename = do s <- readFile filename
                     putStrLn $ mySolution s
