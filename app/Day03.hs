{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Char
import Data.Either
import Debug.Trace
import Text.ParserCombinators.ReadP
import qualified Data.Map as M
import qualified Data.Set as S


-- Types

type ClaimNo = String
type Grid = M.Map (Int, Int) (Int, S.Set ClaimNo)

data GridSection =
  GridSection { _claimNo :: ClaimNo
              , _xPos :: Int
              , _yPos :: Int
              , _width :: Int
              , _height :: Int
              }
  deriving (Show, Eq)

makeLenses ''GridSection


-- main + ghci helper

main :: IO ()
main = interact $ countClaimOverlaps

testIt :: (String -> String) -> String -> IO ()
testIt f filename = do s <- readFile filename
                       putStrLn $ f s

-- Parsing

parseClaim :: ReadP GridSection
parseClaim = do
  _ <- char '#'
  cn <- many1 $ satisfy isDigit
  skipSpaces
  _ <- char '@'
  skipSpaces
  x <- many1 $ satisfy isDigit
  _ <- char ','
  y <- many1 $ satisfy isDigit
  _ <- char ':'
  skipSpaces
  w <- many1 $ satisfy isDigit
  _ <- char 'x'
  h <- many1 $ satisfy isDigit
  -- if we don't have this it doesn't read all the numbers for height,
  -- not sure why that is...
  _ <- eof 
  return $ GridSection cn (read x) (read y) (read w) (read h)

claimsToGridSections :: String -> [GridSection]
claimsToGridSections = fmap ((fst . head) . readP_to_S parseClaim) . lines


-- This ties together reading in the claims and converting them to
-- GridSections followed by smooshing them all together into a single
-- Grid, and is used by both Part 1 and Part 2

readClaims :: String -> Grid
readClaims claims = mapGridSections claims' M.empty
  where claims' = claimsToGridSections claims


-- Calculating the total overlaps from the final calculated Grid

countClaimOverlaps :: String -> String
countClaimOverlaps inputStr = show $ countOverlaps $ readClaims inputStr

countOverlaps :: Grid -> Int
countOverlaps = M.foldr (\(c, _) m -> if (c > 1) then succ m else m) 0


-- Calculating the non-overlapping claim using set semantics

claimsWith1 :: Grid -> S.Set ClaimNo
claimsWith1 = M.foldr (\(c, cms) with1 ->
                         if (c == 1)
                         then S.union cms with1
                         else with1) S.empty

claimsWith2orMore :: Grid -> S.Set ClaimNo
claimsWith2orMore = M.foldr (\(c, cms) twoPlus
                             -> if (c > 1)
                                then S.union cms twoPlus
                                else twoPlus) S.empty

findIsolatedGridSegments :: String -> [ClaimNo]
findIsolatedGridSegments inputStr = S.toList $ S.difference cms1 cms2plus
  where claims = readClaims inputStr
        cms1 = claimsWith1 claims
        cms2plus = claimsWith2orMore claims


-- From here below is all the logic for mapping GridSections into the
-- single Grid which we can then use to calculate both the >1 overlaps
-- (Part 1) as well as find the claim which doesn't overlap any other
-- claims (Part 2).

mapToGrid :: GridSection -> Grid -> Grid
mapToGrid gs grid
  | gs ^. height == 0 = grid
  | otherwise = mapToGrid (gs & yPos %~ succ & height %~ pred) (addRow gs grid)

addRow :: GridSection -> Grid -> Grid
addRow gs grid
  | gs ^. width == 0 = grid
  | otherwise = let key = (gs ^. xPos, gs ^. yPos)
                    val = (1, S.singleton (gs ^. claimNo))
                    insertClaim (_, cms) (cnt, cms') = (succ cnt, S.union cms cms')
                    grid' = M.insertWith insertClaim key val grid
                in addRow (gs & xPos %~ succ & width %~ pred) grid'
      
mapGridSections :: [GridSection] -> Grid -> Grid
mapGridSections gss grid = foldr mapToGrid grid gss


-- Examples

ex1 :: [GridSection]
ex1 = [ GridSection "1" 0 0 2 2 
      , GridSection "2" 2 2 2 2 
      ]

-- |
-- >>> countOverlaps $ mapGridSections ex1 M.empty
-- 0

ex2 :: [GridSection]
ex2 = [ GridSection "1" 0 0 3 3
      , GridSection "2" 1 1 3 3
      ]

-- |
-- >>> countOverlaps $ mapGridSections ex2 M.empty
-- 4

ex3 :: [GridSection]
ex3 = [ GridSection "1" 2 0 2 2
      , GridSection "2" 0 1 4 2
      ]

-- |
-- >>> countOverlaps $ mapGridSections ex3 M.empty
-- 2 
--

ex4 :: String
ex4 = "#1 @ 0,0: 5x5\n#2 @ 1,1: 5x5\n#3 @ 2,2: 5x5\n#4 @ 3,3: 5x5\n#5 @ 4,4: 5x5\n#6 @ 0,10: 5x5"

-- |
-- >>> countOverlaps $ readClaims ex4
-- 37
--

-- |
-- >>> findIsolatedGridSegments $ ex4
-- ["6"]
--