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
type FabricGrid = M.Map (Int, Int) (Int, S.Set ClaimNo)

data Claim =
  Claim { _claimNo :: ClaimNo
        , _xPos :: Int
        , _yPos :: Int
        , _width :: Int
        , _height :: Int
        }
  deriving (Show, Eq)

makeLenses ''Claim


-- main + ghci helper

main :: IO ()
main = interact $ (show . countClaimOverlaps . readClaims)
-- main = interact $ (show . findIsolatedFabricGridSegments . readClaims)

testIt :: (String -> String) -> String -> IO ()
testIt f filename = do s <- readFile filename
                       putStrLn $ f s

-- Parsing

parseClaim :: ReadP Claim
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
  return $ Claim cn (read x) (read y) (read w) (read h)

parseClaims :: String -> [Claim]
parseClaims = fmap ((fst . head) . readP_to_S parseClaim) . lines


-- This ties together reading in the claims and converting them to
-- Claims followed by smooshing them all together into a single
-- FabricGrid, and is used by both Part 1 and Part 2

readClaims :: String -> FabricGrid
readClaims claimsStr = mapClaims claims' M.empty
  where claims' = parseClaims claimsStr


-- Calculating the total overlaps from the final calculated FabricGrid

countClaimOverlaps :: FabricGrid -> Int
countClaimOverlaps = M.foldr (\(c, _) m -> if (c > 1) then succ m else m) 0


-- Calculating the non-overlapping claim using set semantics

filterClaims :: (Int -> Bool) -> FabricGrid -> S.Set ClaimNo
filterClaims pf = M.foldr (\(c, cms) pass ->
                               if (pf c)
                               then S.union cms pass
                               else pass) S.empty

findIsolatedFabricGridSegments :: FabricGrid -> [ClaimNo]
findIsolatedFabricGridSegments claims = S.toList $ S.difference cms1 cms2plus
  where cms1 = filterClaims (== 1) claims
        cms2plus = filterClaims (> 1) claims


-- From here below is all the logic for mapping Claims into the
-- single FabricGrid which we can then use to calculate both the >1 overlaps
-- (Part 1) as well as find the claim which doesn't overlap any other
-- claims (Part 2).

mapToFabricGrid :: Claim -> FabricGrid -> FabricGrid
mapToFabricGrid gs grid
  | gs ^. height == 0 = grid
  | otherwise = mapToFabricGrid (gs & yPos %~ succ & height %~ pred) (addRow gs grid)

addRow :: Claim -> FabricGrid -> FabricGrid
addRow gs grid
  | gs ^. width == 0 = grid
  | otherwise = let key = (gs ^. xPos, gs ^. yPos)
                    val = (1, S.singleton (gs ^. claimNo))
                    insertClaim (_, cms) (cnt, cms') = (succ cnt, S.union cms cms')
                    grid' = M.insertWith insertClaim key val grid
                in addRow (gs & xPos %~ succ & width %~ pred) grid'
      
mapClaims :: [Claim] -> FabricGrid -> FabricGrid
mapClaims gss grid = foldr mapToFabricGrid grid gss


-- Examples

ex1 :: [Claim]
ex1 = [ Claim "1" 0 0 2 2
      , Claim "2" 2 2 2 2
      ]

-- |
-- >>> countClaimOverlaps $ mapClaims ex1 M.empty
-- 0

ex2 :: [Claim]
ex2 = [ Claim "1" 0 0 3 3
      , Claim "2" 1 1 3 3
      ]

-- |
-- >>> countClaimOverlaps $ mapClaims ex2 M.empty
-- 4

ex3 :: [Claim]
ex3 = [ Claim "1" 2 0 2 2
      , Claim "2" 0 1 4 2
      ]

-- |
-- >>> countClaimOverlaps $ mapClaims ex3 M.empty
-- 2 
--

ex4 :: String
ex4 = "#1 @ 0,0: 5x5\n#2 @ 1,1: 5x5\n#3 @ 2,2: 5x5\n#4 @ 3,3: 5x5\n#5 @ 4,4: 5x5\n#6 @ 0,10: 5x5"

-- |
-- >>> countClaimOverlaps $ readClaims ex4
-- 37
--

-- |
-- >>> findIsolatedFabricGridSegments $ readClaims ex4
-- ["6"]
--
