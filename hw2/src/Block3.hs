{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Block3

where

import Control.Applicative
import Control.Monad.State
import Data.Char(isDigit, ord, isSpace)

first :: (a -> c) -> (a, b) -> (c, b)
first f (r, s) = (f r, s)

newtype Parser s a = Parser {runParser :: [s] -> Maybe (a, [s])}

-- laws: fmap id  == id
--        fmap (f . g) = fmap f . fmap g
--
-- proof:
--  1) fmap (\x -> x) (Parser f) ==
--        Just (a, s) -> Just ((\x -> x) a, s) <=> Just (a, s) == Just (a, s)
--        Nothing -> Nothing
--  2) fmap (f . g) == fmap f . fmap g
--      . :: (b -> c) -> (a -> b) -> a -> c
--      Just ( (f . g) a, s) = Just ( f (g a), s)
--      Nothing -> Nothing
--
--      fmap f <=> Just( f a, s)
--      (fmap f)
instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) =
    Parser
      (\ x ->
         let parserResult = parser x in
           case parserResult of
               Just (a, s) -> Just (f a, s)
               Nothing -> Nothing)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser (\s -> Just (a, s))

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (Parser f) <*> (Parser v) = Parser
                                (\ x ->
                                   let resultFunc = f x in
                                     case resultFunc of
                                         Just (innerFunc, array) ->
                                          let inner' = v array in
                                              case inner' of
                                                Just (a, s) -> Just (innerFunc a, s)
                                                Nothing -> Nothing
                                         Nothing -> Nothing)

instance Monad (Parser s) where
  return :: a -> Parser s a
  return = pure

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser value >>= f = Parser
                         (\ s ->
                            let value' = value s in
                              case value' of
                                  Nothing -> Nothing
                                  Just (a, array) -> (runParser $ f a) array)


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
-- >>> runParser parserInteger "hmmm123"
-- >>> Nothing
--
parserInteger :: Parser Char Int
parserInteger = parserSign
  where
    parserNumber :: Int -> Parser Char Int
    parserNumber value =
      Parser
        (\case
           [] -> Just (value, [])
           array -> do
             (digitChar, tail) <- runParser (satisfy isDigit) array
             let digit = ord digitChar - ord '0'
             runParser (parserNumber (value * 10 + digit)) tail)
    parserSign :: Parser Char Int
    parserSign =
      Parser
        (\case
           [] -> Nothing
           array -> do
             let combine = element '+' <|> element '-' <|> satisfy isDigit
             (value, tail) <- runParser combine array
             case value of
               '+' -> runParser (parserNumber 0) tail
               '-' -> let v = fmap ((-1) * ) (parserNumber 0) in
                        runParser v tail
               d -> runParser (parserNumber 0) (d:tail))

-- | Function that parse string of numbers and fold this into list of lists
parserFold :: Parser Char [[Int]]
parserFold = undefined
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
          [] -> Nothing -- or Just ((), []) i'm mot uveren
          array ->
            do
              (_, tail') <- runParser passWhitespace array
              (_, tail'') <- runParser (element ',') tail'
              (_, tail''') <- runParser passWhitespace tail''
              Just ((), tail'''))
    parseSizeArray :: Parser Char Int
    parseSizeArray = undefined
      where
        parsePlusOrDigit :: Parser Char Int
        parsePlusOrDigit =
          Parser
            (\case
              [] -> Nothing
              array -> do
                let combine = element '+' <|> satisfy isDigit
                (evalResult, tail) <- runParser combine array
                case evalResult of
                  '+' -> runParser (parseNumberWhileCan 0) tail
                  digit -> runParser (parseNumberWhileCan 0) (digit:tail)
            )
        parseNumberWhileCan :: Int -> Parser Char Int
        parseNumberWhileCan value =
          Parser
            (\case
              [] -> Just (value, [])
              array -> do
                (digitChar, tail) <- runParser (satisfy isDigit) array
                let digit = ord digitChar - ord '0'
                runParser (parseNumberWhileCan (value * 10 + digit)) tail
            )




--Парсер целого числа, перед которым может быть знак + или -.