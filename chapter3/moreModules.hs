import Data.List

module Chapter3.DataTypes (Range()) where

permutationsStartingWith :: Char -> String -> [String]
permutationsStartingWith letter = filter (\l -> head l == letter) . permutations

data Range = Range Integer Integer deriving Show

-- smart constructor
range :: Integer -> Integer -> Range
range a b = if a <= b then Range a b else error "a must be <= b"

data RangeObs = R Integer Integer deriving Show

r :: Range -> RangeObs
r (Range a b) = R a b
