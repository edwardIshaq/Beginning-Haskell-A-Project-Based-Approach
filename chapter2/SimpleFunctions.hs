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
{--
Time machines are defined by its
manufacturer, its model (which is an integer),
its name,
whether they can travel to the past
and to the future
and a price (which can be represented as a floating-point number).
--}
data TimeMachine = TimeMachine String Integer String Bool Bool Float deriving(Show, Read)


eddie = Person "Edward" "Ashak" Male
shadi = Person "Shadi" "Shalabi" Male
jamie = Person "Jamie" "Mandell" Female

fbi = GovOrg "FBI"
zuno  = Company "Zuno" 2973289 eddie "Future Interactions Strategist"
booz  = Company "Booz Allen" 12039 jamie "Consoltant"
dood  = Individual shadi False

companies = [fbi, zuno, booz, dood]

tm1 = TimeMachine "Apple" 32 "iTime" True False 100
tm2 = TimeMachine "Apple" 35 "iTime 2" True False 200
tm3 = TimeMachine "Apple" 39 "iTime 3" True False 300

machines = [tm1, tm2, tm3]

clientName :: Client -> String
clientName client = case client of
  GovOrg name -> name
  Company name _ _ _-> name
  Individual (Person fname lname _) ads -> fname ++ " " ++ lname

-- Exercise 2-5
clientGender :: Client -> Gender
clientGender (GovOrg _) = Unknown
clientGender (Company _ _ (Person _ _ gender) _) = gender
clientGender (Individual (Person _ _ gender) _ ) = gender

{-For statistical purposes, write a function that returns the number of clients of each gender.
You may need to define an auxiliary data type to hold the results of this function.-}
genderState :: [Client] -> (Int, Int, Int) -- (Male, Female, Unknown)
genderState [] = (0, 0, 0)
genderState xs = (males, females, unknowns)
  where genders = map clientGender xs
        males = length $ [x | x@(Male) <- genders]
        females = length $ [x | x@(Female) <- genders]
        unknowns = length $ [x | x@(Unknown) <- genders]

{-Every year a time comes when time machines are sold with a big discount to encourage potential buyers.
Write a function that given a list of time machines, decreases their price by some percentage.
Use the TimeMachine data type you defined in the previous set of exercises.-}
timeMachineDiscount2 :: [TimeMachine] -> Float -> [TimeMachine]
timeMachineDiscount2 [] _ = []
timeMachineDiscount2 (x@(TimeMachine comp model name past future p):xs) discount = (TimeMachine comp model name past future (discount * p)) : (timeMachineDiscount2 xs discount)

discount ::  Float -> TimeMachine -> TimeMachine
discount rate (TimeMachine comp model name past future p)  = TimeMachine comp model name past future (rate * p)

timeMachineDiscount :: [TimeMachine] -> Float -> [TimeMachine]
timeMachineDiscount xs rate = map (discount rate) machines
