{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Block3
where

-- | Days of week started from Sunday end with Saturday
data Day
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Show)

-- | Define Equal of Days Week with "==" function
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

-- | Define Enumeration for Days Week
-- if argument if "toEnum" not it [0..6] then raise error
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

-- | Define Order for Days Week
instance Ord Day where
    compare :: Day -> Day -> Ordering
    compare a b = let left = fromEnum a in
                      let right = fromEnum b in
                          compare left right

-- | Return day after next with module
--
-- >>> nextDay Saturday
-- Sunday
nextDay :: Day -> Day
nextDay d = toEnum $ mod (fromEnum d + 1) 7

-- | Return day with offset
afterDays :: Day -> Int -> Day
afterDays d n = toEnum $ mod (fromEnum d + n) 7

-- | Check is this day Sunday or Satureday
isWeekend :: Day -> Bool
isWeekend Sunday = True
isWeekend Saturday = True
isWeekend _ = False

-- | Return days left to Friday
daysToParty :: Day -> Int
daysToParty d
    | fromEnum d <= 5 = 5 - fromEnum d
    | otherwise = 6

-- | Representation of People who live in castle
newtype Lord
  = Lord String -- His name
  deriving (Show)

-- | Representation of big house where Lord live
newtype Castle
  = Castle (Maybe Lord) -- Lord may live in castle or not live
  deriving (Show)

-- | Representation of wall which make save people in City from enemy
data Wall
  = Wall
  deriving (Show)

-- | Representation of Defence this town
data Defence
  = Awful -- Wall not exist, nothing exist, u shouldn't live in this town
  | Weak Castle -- Exist only Castle but no guarantee that lord will let you come
  | Strong Castle Wall -- Great defence you may sleep calmly
  deriving (Show)

-- | Representation of good such as Church or Library
data Civil
  = Library -- U may be more clever
  | Church -- You may be more clever
  | Wild -- In this town nothing builded yet
  deriving (Show)

-- | Representation of people who live in house
data People
  = One -- family consist with one member
  | Two -- family consist with two members
  | Three -- family consist with three members
  | Four -- family consist with four members
  deriving (Show)

-- | Realization of enumeration for people
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

-- | Representation of house where only one family live
newtype House
  = House People -- family that live in this house
  deriving (Show)

-- | Representation of all houses in town
-- in town exist at least one house
data Houses
  = HousesOne House -- constructor for one house
  | HousesMore House Houses -- constructor for two or more houses
  deriving (Show)

-- | Representation of all town
data City = City
  { -- | set of defence for town
    defence :: Defence
    -- | set of good for town
  , civil :: Civil
    -- | set of houses for town
  , houses :: Houses
  } deriving (Show)

-- | Function that build castle
-- if castle yet exist then return string with string of error
-- else create new coty with castle
buildCastle :: City -> Either String City
buildCastle c = let def = defence c in
  case def of
    Awful -> Right $ City (Weak (Castle Nothing)) (civil c) (houses c)
    Weak _ -> Left "Castle yet exist"
    Strong _ _ -> Left "Castle yet exist"

-- | Function than bulid good for town
-- if one of good exist then error return
-- if nothing exist then build
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

-- | Function that build new house with family
buildNewHouse :: City -> People -> City
buildNewHouse city family = let buildHouse = House family in
    let union = newHouse (houses city) buildHouse in
      City (defence city) (civil city) union
    where
      newHouse :: Houses -> House -> Houses
      newHouse right house = HousesMore house right

-- | Function that add lord for city
-- return error if lord yet exist or castle not exist
-- otherwise create new lord
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

-- | Function that build wall
--  in city must be more or equal then ten people and exist lord
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

-- | Representation of numbers
data Nat
  = Z -- Zero
  | S Nat -- Next of number
  deriving (Show)

-- | Create enumeration for numbers
instance Enum Nat where
  fromEnum :: Nat -> Int
  fromEnum Z = 0
  fromEnum (S num) = 1 + fromEnum num
  toEnum :: Int -> Nat
  toEnum 0 = Z
  toEnum x = S (toEnum (x - 1))

-- | Create instance of check are two numbers equal or not
instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) Z Z = True
  (==) (S a) (S b) = (==) a b
  (==) _ _ = False

-- | Set full order for numbers
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

-- | Function that add one number for other
add :: Nat -> Nat -> Nat
add Z num = num
add (S x) num = S (add x num)

-- | Function than multiplicate two numbers
mull :: Nat -> Nat -> Nat
mull Z _ = Z
mull (S a) b = add b (mull a b)

-- | Function that substract one number from other
minus :: Nat -> Nat -> Nat
minus Z _ = Z
minus a Z = a
minus (S a) (S b) = minus a b

-- | Check is number ending with 0 or 2 or 4 or 6 or 8
evenNat :: Nat -> Bool
evenNat a = buildNum Z True
  where
      buildNum :: Nat -> Bool -> Bool
      buildNum num ord
        | a == num = ord
        | otherwise = buildNum (S num) (not ord)

-- | Function that divide two numbers
-- return error if second is zero
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

-- | Function that module two numbers
-- return error if second is zero
-- a * b - (a // b) * b
modNat :: Nat -> Nat  -> Nat
modNat a b = minus a (mull (divNat a b) b)

-- | Representation of binnary tree
data Tree a
  = Leaf -- constructor of leaf
  | Branch [a] (Tree a) (Tree a) -- constructor of node that have two child and array of "a" elemnts
  deriving (Show)

-- | Function that check is "Tree" empty
isEmptyTree :: Tree a -> Bool
isEmptyTree Leaf = True
isEmptyTree _ = False

-- | Function that calculate count of all elements in "Tree"
--
-- >>> countElemTree $ Branch [1,1,1] Leaf (Branch [2,2] Leaf Leaf)
-- 5
countElemTree :: Tree a -> Int
countElemTree Leaf = 0
countElemTree (Branch array leftBranch rightBranch) = let left = countElemTree leftBranch in
  let right = countElemTree rightBranch in
    length array + left + right

-- | Function that return True if element exist in "Tree" else False
findElemTree :: Ord a => a -> Tree a  -> Bool
findElemTree _ Leaf = False
findElemTree elemTree (Branch array leftBranch rightBranch) = let firstElem = head array in
  case compare elemTree firstElem of
    LT -> findElemTree elemTree leftBranch
    EQ -> True
    GT -> findElemTree elemTree rightBranch

-- | Function that insert element in "Tree"
insertElemTree :: Ord a => a -> Tree a -> Tree a
insertElemTree elemTree Leaf = Branch [elemTree] Leaf Leaf
insertElemTree elemTree (Branch array leftBranch rightBranch) = let firstElem = head array in
  case compare elemTree firstElem of
    EQ -> Branch (elemTree:array) leftBranch rightBranch
    LT -> Branch array (insertElemTree elemTree leftBranch) rightBranch
    GT -> Branch array leftBranch (insertElemTree elemTree rightBranch)

-- | Function that build binnary tree from array
---fromList array = foldl (\a b -> insertElemTree b a) Leaf array
fromList :: Ord a => [a] -> Tree a
fromList = foldl (flip insertElemTree) Leaf

-- | Function that remove element from tree
-- >>> deleteElemTree 1 (Branch [1] Leaf Leaf)
-- Leaf
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