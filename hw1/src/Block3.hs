{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Block3 (
    Day
  , Lord
  , Castle
  , Wall
  , Defence
  , Civil
  , People
  , House
  , Houses
  , City
  , Nat
  , Tree
  , buildWall
  , buildNewHouse
  , buildCastle
  , buildCivil
  , colonizeCity
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  , add
  , mull
  , minus
  , evenNat
  , divNat
  , modNat
  , isEmptyTree
  , countElemTree
  , findElemTree
  , insertElemTree
  , fromList
  , deleteElemTree
)
where

-- task1
data Day
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Show)

instance Eq Day where
  (==) :: Day -> Day -> Bool
  (==) Sunday Sunday = True
  (==) Monday Monday = True
  (==) Tuesday Tuesday = True
  (==) Wednesday Wednesday = True
  (==) Thursday Thursday = True
  (==) Friday Friday = True
  (==) Saturday Saturday = True
  (==) _ _ = False


instance Enum Day where
  fromEnum :: Day -> Int
  fromEnum Sunday = 0
  fromEnum Monday = 1
  fromEnum Tuesday = 2
  fromEnum Wednesday = 3
  fromEnum Thursday = 4
  fromEnum Friday = 5
  fromEnum Saturday = 6

  toEnum :: Int -> Day
  toEnum 0 = Sunday
  toEnum 1 = Monday
  toEnum 2 = Tuesday
  toEnum 3 = Wednesday
  toEnum 4 = Thursday
  toEnum 5 = Friday
  toEnum 6 = Saturday
  toEnum _ = error "number must be from 0 to 6"

instance Ord Day where
    compare :: Day -> Day -> Ordering
    compare a b = let left = fromEnum a in
                      let right = fromEnum b in
                          compare left right

nextDay :: Day -> Day
nextDay d = toEnum $ mod (fromEnum d + 1) 7

afterDays :: Day -> Int -> Day
afterDays d n = toEnum $ mod (fromEnum d + n) 7

isWeekend :: Day -> Bool
isWeekend Sunday = True
isWeekend Saturday = True
isWeekend _ = False

daysToParty :: Day -> Int
daysToParty d
    | fromEnum d <= 5 = 5 - fromEnum d
    | otherwise = 6

-- task2
newtype Lord
  = Lord String
  deriving (Show)

newtype Castle
  = Castle (Maybe Lord)
  deriving (Show)

data Wall
  = Wall
  deriving (Show)

data Defence
  = Awful
  | Weak Castle
  | Strong Castle Wall
  deriving (Show)

data Civil
  = Library
  | Church
  | Wild
  deriving (Show)

data People
  = One
  | Two
  | Three
  | Four
  deriving (Show)

instance Enum People where
  fromEnum :: People -> Int
  fromEnum One = 1
  fromEnum Two = 2
  fromEnum Three = 3
  fromEnum Four = 4

  toEnum :: Int -> People
  toEnum 1 = One
  toEnum 2 = Two
  toEnum 3 = Three
  toEnum 4 = Four
  toEnum _ = error "number must be from 0 to 4"

newtype House
  = House People
  deriving (Show)

data Houses
  = HousesOne House
  | HousesMore House Houses
  deriving (Show)

data City = City
  { defence :: Defence
  , civil :: Civil
  , houses :: Houses
  } deriving (Show)

buildCastle :: City -> Either String City
buildCastle c = let def = defence c in
  case def of
    Awful -> Right $ City (Weak (Castle Nothing)) (civil c) (houses c)
    Weak _ -> Left "Castle yet exist"
    Strong _ _ -> Left "Castle yet exist"

buildCivil :: City -> Civil -> Either String City
buildCivil city civ =
  if canBuildCivil city
    then Right $ City (defence city) civ (houses city)
    else Left (errorResponse $ civil city)
  where
      canBuildCivil :: City -> Bool
      canBuildCivil town =
        let inner = civil town
         in case inner of
              Library -> False
              Church -> False
              Wild -> True
      errorResponse :: Civil -> String
      errorResponse Library = "Library yet exist"
      errorResponse Church = "Church yet exist"
      errorResponse _ = error "unexpected case"

buildNewHouse :: City -> People -> City
buildNewHouse city family = let buildHouse = House family in
    let union = newHouse (houses city) buildHouse in
      City (defence city) (civil city) union
    where
      newHouse :: Houses -> House -> Houses
      newHouse right house = HousesMore house right

colonizeCity :: City -> Lord -> Either String City
colonizeCity city lord = let castle = defence city in
  case castle of
    Awful -> Left "Castle not exist"
    Weak (Castle (Just (Lord lordName))) -> Left $ errorEval lordName
    Weak (Castle Nothing) -> Right $ partialEval city (Weak (Castle (Just lord)))
    Strong (Castle (Just (Lord lordName))) _ -> Left $ errorEval lordName
    Strong (Castle Nothing) wall -> Right $ partialEval city (Strong (Castle (Just lord)) wall)
  where
    errorEval :: String -> String
    errorEval lrd = "Lord: "  ++ lrd ++ " yet exist here"
    partialEval :: City -> (Defence -> City )
    partialEval ct  x = City x (civil ct) (houses ct)

buildWall :: City -> Either String City
buildWall city =
  if existLord (defence city)
  then if canBuild (defence city)
       then if calculatePeopleInHouses (houses city) >= 10
            then Right $ City (Strong (Castle $ Just (lordName $ defence city)) Wall) (civil city) (houses city)
            else Left "People less than 10"
       else  Left "Wall yet exist"
  else Left "Lord not exist"
  where
    existLord :: Defence -> Bool
    existLord (Weak (Castle (Just (Lord _)))) = True
    existLord (Strong (Castle (Just (Lord _))) _) = True
    existLord _ = False

    calculatePeopleInHouses :: Houses -> Int
    calculatePeopleInHouses (HousesOne (House family)) = fromEnum family
    calculatePeopleInHouses (HousesMore (House family) other) = let inner = calculatePeopleInHouses other in
      inner + fromEnum family

    canBuild :: Defence -> Bool
    canBuild (Weak (Castle (Just (Lord _)))) = True
    canBuild _ = False

    lordName :: Defence -> Lord
    lordName (Weak (Castle (Just lord))) = lord
    lordName _ = error "unexpected case"

--task3
data Nat
  = Z
  | S Nat
  deriving (Show)

instance Enum Nat where
  fromEnum :: Nat -> Int
  fromEnum Z = 0
  fromEnum (S num) = 1 + fromEnum num
  toEnum :: Int -> Nat
  toEnum 0 = Z
  toEnum x = S (toEnum (x - 1))

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) Z Z = True
  (==) (S a) (S b) = (==) a b
  (==) _ _ = False

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  (<=)  Z _ = True
  (<=)  (S a) (S b) = (<=) a b
  (<=) (S _) Z = False

  compare :: Nat -> Nat -> Ordering
  compare a b
    | a == b = EQ
    | a <= b = LT
    | otherwise = GT

add :: Nat -> Nat -> Nat
add Z num = num
add (S x) num = S (add x num)

mull :: Nat -> Nat -> Nat
mull Z _ = Z
mull (S a) b = add b (mull a b)

minus :: Nat -> Nat -> Nat
minus Z _ = Z
minus a Z = a
minus (S a) (S b) = minus a b

evenNat :: Nat -> Bool
evenNat a = buildNum Z True
  where
      buildNum :: Nat -> Bool -> Bool
      buildNum num ord
        | a == num = ord
        | otherwise = buildNum (S num) (not ord)

-- a // b => \exist q : b * q <= a
divNat :: Nat -> Nat -> Nat
divNat _ Z = error "na nol' delit' nel'za"
divNat Z _ = Z
divNat a b = findQ (S Z)
  where
      findQ :: Nat -> Nat
      findQ n
        | mull n b < a = findQ (S n)
        | mull n b == a = n
        | otherwise =  minus n (S Z)

-- a * b - (a // b) * b
modNat :: Nat -> Nat  -> Nat
modNat a b = minus a (mull (divNat a b) b)

--task4
data Tree a
  = Leaf
  | Branch [a] (Tree a) (Tree a)
  deriving (Show)

isEmptyTree :: Tree a -> Bool
isEmptyTree Leaf = True
isEmptyTree _ = False

countElemTree :: Tree a -> Int
countElemTree Leaf = 0
countElemTree (Branch array leftBranch rightBranch) = let left = countElemTree leftBranch in
  let right = countElemTree rightBranch in
    length array + left + right

findElemTree :: Ord a => a -> Tree a  -> Bool
findElemTree _ Leaf = False
findElemTree elemTree (Branch array leftBranch rightBranch) = let firstElem = head array in
  case compare elemTree firstElem of
    LT -> findElemTree elemTree leftBranch
    EQ -> True
    GT -> findElemTree elemTree rightBranch

insertElemTree :: Ord a => a -> Tree a -> Tree a
insertElemTree elemTree Leaf = Branch [elemTree] Leaf Leaf
insertElemTree elemTree (Branch array leftBranch rightBranch) = let firstElem = head array in
  case compare elemTree firstElem of
    EQ -> Branch (elemTree:array) leftBranch rightBranch
    LT -> Branch array (insertElemTree elemTree leftBranch) rightBranch
    GT -> Branch array leftBranch (insertElemTree elemTree rightBranch)

---fromList array = foldl (\a b -> insertElemTree b a) Leaf array -- idea replace there
fromList :: Ord a => [a] -> Tree a
fromList = foldl (flip insertElemTree) Leaf

deleteElemTree :: forall a . Ord a => a -> Tree a -> Tree a
deleteElemTree _ Leaf = Leaf
deleteElemTree elemTree (Branch array leftBranch rightBranch) =
  let firstElem = head array in
    case compare elemTree firstElem of
      EQ ->
        if null (tail array)
        then removeElem (Branch array leftBranch rightBranch)
        else Branch (tail array) leftBranch rightBranch
      LT -> Branch array (deleteElemTree elemTree leftBranch) rightBranch
      GT -> Branch array leftBranch (deleteElemTree elemTree rightBranch)
  where
      removeElem :: Tree a -> Tree a
      removeElem (Branch _ Leaf Leaf) = Leaf
      removeElem (Branch _ Leaf right) = right
      removeElem (Branch _ left Leaf) = left
      removeElem (Branch _ _ right) = foldr insertElemTree right (treeToList leftBranch)
      removeElem _ = error "unexpected case"
      treeToList :: Ord a => Tree a -> [a]
      treeToList Leaf = []
      treeToList (Branch array1 leftBranch1 rightBranch1) = treeToList leftBranch1 ++ array1 ++ treeToList rightBranch1