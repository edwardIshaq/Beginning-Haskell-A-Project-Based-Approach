{-# LANGUAGE RecordWildCards #-}
import qualified Data.Map as M
import qualified Data.Set as S

{-
insert, delete and adjust are all instances of a general function alter that subsumes all of them.
The first argument is a function of type Maybe a -> Maybe a:
  the input will be Nothing if the key is not already present,
  or the previous value wrapped in a Just.
What to do with that key is specified by the return value of that function:
  if it is Nothing, the key will be dropped,
  and if it is Just v, that would be the new value for the key.


It’s very common for Haskell libraries to contain a fully general function such as alter,
which is later made more concrete in other functions, usually providing an easier way to work and remember API.
Put yourself for a moment in the place of a library designer and write the functions insert, delete, and adjust using alter.
-}

-- M.insert :: Ord k => k -> a -> M.Map k a -> M.Map k a
-- M.alter :: Ord k => (Maybe a -> Maybe a) -> k -> M.Map k a -> M.Map k a

mapInsert :: Ord k => k -> v -> M.Map k v -> M.Map k v
mapInsert k v m = M.alter (\_ -> Just v) k  m


-- M.delete :: Ord k => k -> M.Map k a -> M.Map k a
mapDelete :: Ord k => k -> M.Map k a -> M.Map k a
mapDelete k m = M.alter (\prev -> Nothing) k m

-- M.adjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
mapAdjust :: Ord k => (a -> a) -> k -> M.Map k a -> M.Map k a
mapAdjust f key m = M.alter (\prev -> case prev of
                                          Nothing -> Nothing
                                          Just v -> Just (f v)) key m


data Client i = GovOrg  { clientId :: i , clientName :: String }
             | Company { clientId :: i , clientName :: String
                        , person :: Person, duty :: String }
             | Individual { clientId :: i , person :: Person }
             deriving (Show, Ord)

instance Eq (Client i) where
    g1@GovOrg{} == g2@GovOrg{} =  g1 == g2
    g1@Company{} == g2@Company{} =  g1 == g2
    g1@Individual{} == g2@Individual{} =  g1 == g2
    _ == _  = False


data Person = Person { firstName :: String, lastName  :: String }
             deriving (Show, Ord, Eq)

-- instance Eq Person where
--   (==) :: Person -> Person -> Bool
--   (==) p1{} p2{..} =  p1.firstName == p2.firstName

data ClientKind = GovOrgKind | CompanyKind | IndividualKind deriving (Ord, Eq, Show)

fbi = GovOrg 1 "FBI"
cia = GovOrg 2 "CIA"
nsa = GovOrg 3 "NSA"

eddie = Person { firstName = "Eddie", lastName = "Ashak"}
jamie = Person { firstName = "Jamie", lastName = "Mandell"}
shadi = Person { firstName = "Shadi", lastName = "Shalabi"}

eddieClient = Individual 4 eddie
jamieClient = Individual 5 jamie
shadiClient = Individual 6 shadi

zuno = Company 7 "Zuno, Inc" eddie "Future Interactions Specialist"
booz = Company 8 "Booz Allen" jamie "Senior Consultant"
stl  = Company 9 "STL Tribe" shadi "CEO"

demoClients = [fbi, jamieClient,zuno ,cia,booz, shadiClient,stl, eddieClient, nsa]
-- govSet = S.insert fbi S.empty
-- kindMap = M.insert GovOrgKind govSet M.empty
{-
For analysis purposes, it interesting to classify clients according to the kind it belongs to: government organization, company, or individual.
Now, create a function classifyClients which traverses a list of clients (with type [Client Integer], with Client defined as in the previous chapter)
  and generates a value of type Map ClientKind (Set (Client Integer)). You should create two different implementations:
    1- The first should traverse the list element by element, and perform on each element the classification, decide which of the map items to modify, and then add itself to the set;
    2- The second should first create lists corresponding to the three different kinds, and at the end convert those lists to sets and generate the mentioned map from them.
It’s interesting that you create a very large client list and run the two implementations to compare which one behaves better in speed.
 -}

--  classifyClients
--    :: Ord t => [Client t] -> M.Map ClientKind (S.Set (Client t))
classifyClients [] = M.empty
classifyClients (x:xs) = case x of
        GovOrg {} -> M.unionWith (S.union) (insertMap GovOrgKind x) (classifyClients xs)
        Company {} -> M.unionWith (S.union) (insertMap CompanyKind x) (classifyClients xs)
        Individual {} -> M.unionWith (S.union) (insertMap IndividualKind x) (classifyClients xs)
        where insertMap k y = M.insert k (S.insert y S.empty) M.empty
