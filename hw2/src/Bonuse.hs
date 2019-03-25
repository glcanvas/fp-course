{-# LANGUAGE InstanceSigs #-}

module Bonuse {-(
  sm
  , smCB
)-} where

{-
import Control.Monad.Cont

hmm'' = runCont (cont (\x -> x (\y -> y * y)) <*> cont (\f -> f 5)) (\i -> i - 1)
hmm''' = runCont (Cont (\x -> x (\y -> y * y)) <*> Cont (\f -> f 5)) (\i -> i - 1)


sm :: Int -> Int -> Int
sm x y = x + y

smCB :: Int -> Int -> (Int -> a) -> a
smCB x y callback =
  let res = sm x y in
    callback res

hmm = runCont (Cont (\f -> f 5 )) (\i -> i * 2)
hmm' = fmap (\x -> x * x) (Cont (\f -> f 5))
-}

-- | Define continuation
newtype Cont r a = Cont
    {  -- | define cont constructor
      runCont :: (a -> r) -> r
    }

instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f (Cont v) =
    Cont (\r -> v (\a -> let inner = f a in r inner))

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure a = Cont (\r -> r a)

  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont  r b
  Cont func <*> Cont value = Cont (\r -> func (\f -> value (r . f )))
  --Cont func <*> Cont value = Cont (\r -> value (\a -> func (\c ->  r (c a))))

instance Monad (Cont r) where
  return :: a -> Cont r a
  return a  = Cont (\f -> f a)

  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  Cont v >>= f = Cont (\r -> v (\a -> runCont (f a) r))

data SysCallName
  = Read
  | Write
  | Exit
  | Yield
  | Fork
  deriving Show

type SysCallArguments = Either () String
type SysCallResult = Either () String

data Context = Context SysCallName SysCallArguments (Process SysCallResult)
type Process r = Cont r Context

readLine :: Cont SysCallArguments SysCallResult
readLine = Cont (\f -> f (Right "1"))

writeLine :: String -> Cont SysCallArguments SysCallResult
writeLine s = Cont(\f -> f (Left ()))
hm =
  readLine >>= (\v ->
    let Right ok = v in
      (writeLine ok) >>= (\v' -> return v'))


systemCall :: SysCallName -> SysCallArguments -> (SysCallName, SysCallArguments, Cont r SysCallResult)

systemCall Read _ = (Read, Left (), Cont (\ f -> f (Right "5")))
systemCall Write (Right v) = (Write, Right v, Cont (\ f -> f (Right v)))

--hmm :: Cont r SysCallResult
hmm =
  let (a, b, c) = systemCall Read (Left ()) in
    undefined


ptr' = runCont hmm (\x -> x)

hm' :: Cont SysCallArguments SysCallResult
hm' =
  do
  v <- readLine
  let Right ok = v
  writeLine ok

cnt x = Cont (\f -> f x)

kernel =
  let x = hm' in
    undefined

prt = runCont hm (\x -> x)





data SysCall r = SysCall r [(SysCallName, SysCallArguments, Cont SysCallArguments SysCallResult)]

readLineCall :: SysCall String
readLineCall = SysCall "rl" [(Read, Left (), Cont (\f -> f (Right "5")))]

writeLineCall :: String -> SysCall ()
writeLineCall s = SysCall () [(Write, Right s, Cont (\f -> f (Right s)))]

instance Functor SysCall where

instance Applicative SysCall where

instance Monad SysCall where
  return :: r -> SysCall r
  return r = SysCall r [(Exit, Left (), Cont (\x -> x (Left ())))]

  (>>=) :: SysCall a -> (a -> SysCall b) -> SysCall b
  (SysCall a values) >>= function =
    let SysCall b values' = function a in
      SysCall b (values ++ values')



blia =
  do
   v <- readLineCall
   let hmm = v ++ v
   writeLineCall hmm

a f = Cont (\x -> x f)

b = a (\y -> y + 2)

c =
  do
   z <- b
   undefined