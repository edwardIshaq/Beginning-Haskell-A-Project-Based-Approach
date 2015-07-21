firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 = if null lst1  {- check emptyness -}
                then lst2      -- base case
                else (head lst1) : (tail lst1 +++ lst2)
reverse2 :: [a] -> [a]
reverse2 list = if null list
                then []
                else reverse2 (tail list) +++ [head list]

maxmin list = let h = head list
              in if null (tail list)
                 then (h, h)
                 else ( if h > t_max then h else t_max, if h < t_min then h else t_min )
                      where t = maxmin (tail list)
                            t_max = fst t
                            t_min = snd t

-- V1
{-
data Client = GovOrg     String
            | Company    String Integer String String
            | Individual String String Bool
            deriving(Show)
-}

data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving(Show)

data Gender = Male | Female | Unknown deriving(Show)
data Person = Person String String Gender deriving(Show)
data TimeMachine = TimeMachine String Integer String Bool Bool Float deriving(Show, Read)


clientName :: Client -> String
clientName client = case client of
  GovOrg name -> name
  Company name _ _ _-> name
  Individual (Person fname lname _) ads -> fname ++ " " ++ lname

genderStats :: [Client] ->
