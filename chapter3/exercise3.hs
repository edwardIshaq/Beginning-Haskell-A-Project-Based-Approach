{-# LANGUAGE LambdaCase #-}

import Chapter2.SimpleFunctions
import Data.List (find)

product1 :: (Num a) => [a] -> a
product1 [] = 1
product1 (x:[]) = x
product1 (x:xs) = x * (product1 xs)

product2 :: (Num a) => [a] -> a
product2 xs = foldl (*) 1 xs

all1 :: [Bool] -> Bool
all1 [] = True
all1 (x:xs) = x && (all1 xs)

all2 :: [Bool] -> Bool
all2 xs = foldr (&&) True xs

skipUntillGov :: [ClientR] -> [ClientR]
skipUntillGov = dropWhile (\case { GovOrgR {} -> False; _ -> True})

john = PersonR { firstName = "John", lastName = "Smith", gender = Male}
johnClient = IndividualR {person = john, ads = False}
nato = GovOrgR { clientRName = "NATO"}
hyundai = CompanyR { clientRName = "Hyundai", companyID = 12321, person = PersonR {firstName="wierd", lastName="guy", gender = Male}, duty= "reception"}

clientsExample = [johnClient, nato, hyundai]

isIndividual :: ClientR -> Bool
isIndividual (IndividualR {}) = True
isIndividual _               = False

checkIndividualAnalytics :: [ClientR] -> (Bool, Bool)
checkIndividualAnalytics cs = ( any isIndividual cs, not $ all isIndividual cs)

-- find:: (a -> Bool) -> t a -> Maybe a
elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = let res = find (\y -> y == x ) xs in
  case res of
     Just _ -> True
     _      -> False



    --  data Client i = GovOrg  { clientId :: i , clientName :: String }
    --                | Company { clientId :: i , clientName :: String
    --                           , person :: Person, duty :: String }
    --                | Individual { clientId :: i , person :: Person }x
    --                deriving Show
    --
    --  data Person = Person { firstName :: String, lastName  :: String }
    --                deriving Show
