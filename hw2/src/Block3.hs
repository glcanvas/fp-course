{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Block3 (
  Parser(Parser)
  , runParser
  , ok
  , eof
  , satisfy
  , element
  , stream
  , getOne
  , parserBranchSeq
  , parserInteger
  , parserFold
) where

import Control.Applicative
import Control.Monad.State
import Data.Char(isDigit, ord, isSpace)

-- | Representation of parser
newtype Parser s a
  = Parser {runParser :: [s] -> Maybe (a, [s])} -- constructor for parser that get a function that call in parse

-- | instance Functor for parser
instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) =
    Parser
      (\ x ->
         let parserResult = parser x in
           case parserResult of
               Just (a, s) -> Just (f a, s)
               Nothing -> Nothing)

-- | instance applicative for parser
instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser (\s -> Just (a, s))

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (Parser f) <*> (Parser v) =
    Parser
      (\ x ->
        let resultFunc = f x in
        case resultFunc of
          Just (innerFunc, array) ->
            let inner' = v array in
            case inner' of
              Just (a, s) -> Just (innerFunc a, s)
              Nothing -> Nothing
          Nothing -> Nothing)

-- | instance monad for parser
instance Monad (Parser s) where
  return :: a -> Parser s a
  return = pure

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser value >>= f =
    Parser
    (\ s ->
      let value' = value s in
      case value' of
        Nothing -> Nothing
        Just (a, array) -> (runParser $ f a) array)

-- | instance alternative for parser
instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser (const Nothing)

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (Parser left) <|> (Parser right) =
    Parser
      (\ s ->
        case left s of
          Just (leftValue, leftArray) -> Just (leftValue, leftArray)
          Nothing -> case right s of
            Just (rightValue, rightArray) -> Just (rightValue, rightArray)
            Nothing -> Nothing)

-- | Parser that never "eat" input and return this input
ok :: Parser s ()
ok = Parser (\s -> Just ((), s))

-- | Parser that return success if input is end else fail
eof :: Parser s ()
eof = Parser (\case
                  [] -> Just ((), [])
                  _ -> Nothing)

-- | Parser that return success if predicate function for this element i true otherwise false
satisfy :: (s -> Bool) -> Parser s s
satisfy predicate =
  Parser
    (\case
       [] -> Nothing
       (x:xs) ->
         if predicate x
           then Just (x, xs)
           else Nothing)

-- | Parser that end with success if started element of value in runParser
-- satisfy element of current value
element :: Eq s => s -> Parser s s
element s = satisfy ( == s)

-- | Parser thar end success if subseq satisfy prefix of value in runParser value
stream :: Eq s => [s] -> Parser s [s]
stream [] = return []
stream (x:xs) = element x >>= (\s -> stream xs >>= (\xxs -> return (s : xxs)))

-- | Parser that return one element from stream
getOne :: Parser s s
getOne =
  Parser
    (\case
       [] -> Nothing
       (x:xs) -> Just (x, xs))

-- | Parser that parse bracket sequence and return Just if sequence is correct or Nothing
--
-- >>> runParser branchSeq "vsem privei ()()()"
-- >>> Nothing
--
-- >>> runParser branchSeq ""
-- >>> Just ((), "")
--
-- >>> runParser branchSeq "()()()"
-- >>> Just ((), "")
--
parserBranchSeq :: Parser Char ()
parserBranchSeq = parser 0
  where
    parser :: Int -> Parser Char ()
    parser cnt =
      Parser (\s ->
        if cnt < 0 then  Nothing
        else
        case s of
          [] -> if cnt == 0
                  then Just ((), [])
                  else Nothing
          array -> do (bracket, tail) <- runParser (element '(' <|> element ')') array
                      case bracket of
                          '(' -> runParser (parser (cnt + 1)) tail
                          ')' -> runParser (parser (cnt - 1)) tail)

-- | Parser that parse string that contains integer to integer
--
-- >>> runParser parserInteger "123"
-- >>> Just (123, "")
--
-- >>> runParser parserInteger "+123"
-- >>> Just (123, "")
--
-- >>> runParser parserInteger "-123"
-- >>> Just (-123, "")
--
-- >>> runParser parserInteger ""
-- >>> Nothing
--
-- >>> runParser parserInteger "123hmmm"
-- >>> Just(123, "hmmm")
--
parserInteger :: Parser Char Int
parserInteger = parserSign
  where
    parserNumber :: Bool -> Int -> Parser Char Int
    parserNumber readAtLeastOne value =
      Parser
        (\case
           [] -> if readAtLeastOne then Just (value, []) else Nothing
           array -> do
             (digitChar, tail) <- runParser getOne array
             if isDigit digitChar
             then runParser (parserNumber True (value * 10 + (ord digitChar - ord '0'))) tail
             else
              if readAtLeastOne
              then Just (value, digitChar:tail)
              else Nothing)
    parserSign :: Parser Char Int
    parserSign =
      Parser
        (\case
           [] -> Nothing
           array -> do
             let combine = element '+' <|> element '-' <|> satisfy isDigit
             (value, tail) <- runParser combine array
             case value of
               '+' -> runParser (parserNumber False 0) tail
               '-' -> let v = fmap ((-1) * ) (parserNumber False 0) in
                        runParser v tail
               d -> runParser (parserNumber True 0) (d:tail))

-- | Function that parse string of numbers and fold this into list of lists
--
-- >>> runParser parserFold "1,1,1,1"
-- >>> Just ([[1],[1]), "")
--
-- >>> runParser parserFold ",1,1,1"
-- >>> Nothing
--
-- >>> runParser parserFold "2, 2,1,1"
-- >>> Nothing
--
-- >>> runParser parserFold "-1,1,1,1"
-- >>> Nothing
--
-- >>> runParser parserFold "+3,2,3,3,     1, 1"
-- >>> Just ([[2,3,3],[1]],"")
--
-- >>> runParser parserFold "+3,2,3,3,     2, 1"
-- >>> Nothing
--
-- >>> runParser parserFold "+3,2,3,3,     2 1"
-- >>> Nothing
--
-- >>> runParser parserFold "+3,2,3,3,     2, 1, 2"
-- >>> Just ([[2,3,3],[1,2]],"")
--
-- >>> runParser parserFold "+3,2,3,3,     2, 1, 211"
-- >>> Just ([[2,3,3],[1,211]],"")
--
-- >>> runParser parserFold "+3,2,3,3,     2, 1, 211asddas"
-- >>> Nothing
--
parserFold :: Parser Char [[Int]]
parserFold = parserInDepend passWhitespace
  where
    passWhitespace :: Parser Char ()
    passWhitespace =
      Parser
        (\case
           [] -> Just ((), [])
           array -> case runParser (satisfy isSpace) array of
                      Just (_, tail) -> runParser passWhitespace tail
                      Nothing -> Just ((), array))
    passOneDelimiter :: Parser Char ()
    passOneDelimiter =
      Parser
        (\case
          [] -> Nothing
          array ->
            do
              (_, tail') <- runParser passWhitespace array
              (_, tail'') <- runParser (element ',') tail'
              (_, tail''') <- runParser passWhitespace tail''
              Just ((), tail'''))
    readArrayTitle :: Parser Char () -> Parser Char (Maybe Int)
    readArrayTitle passParser =
      Parser
        (\case
        [] -> Just(Nothing, [])
        array -> do
         (_, tail') <- runParser passParser array
         (num, tail'') <- runParser parserInteger tail'
         if num >= 0
          then Just (Just num, tail'')
          else Nothing)
    readArray :: Int -> Parser Char [Int]
    readArray tailSize =
      Parser (\case
                [] -> if tailSize > 0 then Nothing else Just([], [])
                array -> if tailSize == 0
                  then Just ([], array)
                  else do
                    (_, tail') <- runParser passOneDelimiter array
                    (value, tail'') <- runParser parserInteger tail'
                    fmap (\(a, b) -> (value:a, b)) (runParser (readArray (tailSize - 1)) tail''))
    parserInDepend :: Parser Char () -> Parser Char [[Int]]
    parserInDepend delimiter =
      Parser
          (\s -> do (title, tail') <- runParser (readArrayTitle delimiter) s
                    case title of
                        Nothing -> Just ([], tail')
                        Just value -> do (array, tail'') <- runParser (readArray value)
                                                              tail'
                                         fmap (\(a, b) -> (array:a, b)) (runParser (parserInDepend passOneDelimiter) tail''))