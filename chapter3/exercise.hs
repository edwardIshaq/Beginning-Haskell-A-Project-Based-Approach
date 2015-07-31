{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

import Chapter2.SimpleFunctions

swapTriple :: (a,b,c) -> (b,c,a)
swapTriple (x,y,z) = (y,z,x)

duplicate :: a -> (a,a)
duplicate x = (x,x)

nothing :: t -> Maybe a
nothing _ = Nothing

-- index :: [a] -> [(Num,a)]
index :: Num t => [a] -> [(t, a)]
index []     = []
index [x]    = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
                                      in  (n+1,x):indexed

maybeA :: [t] -> Char
maybeA [] = 'a'


sayHello :: [String] -> [String]
-- sayHello names = map (\name -> case name of
--                                  "Alejandro" -> "Hello, writer"
--                                  _           -> "Welcome, " ++ name
--                      ) names

-- Using LambdaCase
sayHello names = map (\case "Alejandro" -> "Hello, writer"
                            name        -> "Welcome, " ++ name
                    ) names

{-
Using the function filter as the basis for your solution, write the following functions:
filterOnes, which returns only the elements equal to the constant 1.
filterANumber, which returns only the elements equal to some number which is given via a parameter.
filterNot, which performs the reverse duty of filter: it returns only those elements of the list which do not fulfill the condition.
filterGovOrgs, which takes a list of Clients (as defined before) and returns only those which are government organizations.
Write it using both an auxiliary function isGovOrg and a \case expression.
-}
filterOnes :: (Num a, Eq a) => [a] -> [a]
filterOnes xs = filter (\x -> x == 1) xs

filterANumber :: (Num a, Eq a) => a -> [a] -> [a]
filterANumber n xs = filter (\x -> x == n) xs

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f xs = filter (not . f) xs


filterGovOrgs :: [ClientR] -> [ClientR]
filterGovOrgs xs = filter (\case
  GovOrgR {..} -> True
  otherwise  -> False) xs

isGovOrg :: ClientR -> Bool
isGovOrg GovOrgR {..} = True
isGovOrg _            = False

john = PersonR { firstName = "John", lastName = "Smith", gender = Male}
johnClient = IndividualR {person = john, ads = False}
nato = GovOrgR { clientRName = "NATO"}
hyundai = CompanyR { clientRName = "Hyundai", companyID = 12321, person = PersonR {firstName="wierd", lastName="guy", gender = Male}, duty= "reception"}

companies = [johnClient, nato, hyundai]
