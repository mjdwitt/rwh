import Data.Char
import Data.List

-- First set ---------------------------------------------------------

-- 1) Write your own "safe" definitions of the standard partial list
-- functions, but make sure that yours never fail.

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _      = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail _      = Nothing

safeLast :: [a] -> Maybe a
safeLast (x:xs)
        | null xs   = Just x
        | otherwise = safeLast xs
safeLast _          = Nothing

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (beginning xs)
    where beginning (x:xs)
                  | null xs   = []
                  | otherwise = x : (beginning xs)

-- 2) Write a function `splitWith` that acts similarly to `words`, but
-- takes a predicate and a list of any type, and splits its input list
-- on every element for which the predicate returns `False`.

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith notDelim xs = let sublist   = takeWhile notDelim xs
                            remainder = drop (1 + (length sublist)) xs
                        in sublist : (splitWith notDelim remainder)

-- 3) See the file ./Ex1-3.hs

-- 4) See the file ./Ex1-4.hs



-- Second set --------------------------------------------------------

-- 1) Use a fold (choosing the appropriate fold will make your code
-- much simpler) to rewrite and improve upone the `asInt` function
-- from the "Explicit Recursion" section.

asInt :: String -> Int
asInt (c:cs) | c == '-'  = (-1) * (foldl addDigit 0 cs)
             | otherwise = foldl addDigit 0 (c:cs)
    where addDigit acc d | d == '.'   = error "asInt: cannot transform floating-point numbers"
                         | notDigit d = error "asInt: non-digit characters"
                         | overflow   = error "asInt: Int overflow"
                         | otherwise  = acc*10 + (digitToInt d)
              where notDigit c = c < '0' || c > '9'
                    overflow = let acc' = (fromIntegral acc)*10 + (fromIntegral (digitToInt d))
                                   maxInt = 2^63 - 1
		               in acc' > maxInt

-- 2) The `asInt` function above uses `error`, so its callers cannot
-- handle errors. Rewrite it to fix this problem.

type ErrorMessage = String
safeAsInt :: String -> Either ErrorMessage Int
safeAsInt (c:cs) | c == '-'  = case (foldl safeAddDigit (Right 0) cs) of
                                   (Left err) -> Left err
                                   (Right x)  -> Right (-1 * x)
                 | otherwise = foldl safeAddDigit (Right 0) (c:cs)
    where safeAddDigit (Left err)  _              = Left err
          safeAddDigit (Right acc) d | d == '.'   = Left "cannot transform floating-point numbers"
                                     | notDigit d = Left ("non-digit '" ++ [d] ++ "'")
                                     | overflow   = Left "Int overflow"
                                     | otherwise  = Right (acc*10 + (digitToInt d))
              where notDigit c = c < '0' || c > '9'
                    overflow = let acc' = (fromIntegral acc)*10 * (fromIntegral (digitToInt d))
                                   maxInt = 2^63 - 1
                               in acc' > maxInt

-- 3) The Prelude function `concat` concatenates a list of lists into
-- a single list, and has the following type:
--
--     concat :: [[a]] -> [a]
--
-- Write a definition of `concat` using `foldr`.

foldcat :: [[a]] -> [a]
foldcat = foldr (++) []

-- 4) Write your own definition of the standard `takeWhile` functions,
-- first using explicit recursion, then foldr.

recTakeWhile :: (a -> Bool) -> [a] -> [a]
recTakeWhile pred (x:xs) | pred x    = x : (recTakeWhile pred xs)
                         | otherwise = []
recTakeWhile _    _                  = []

foldTakeWhile :: (a -> Bool) -> [a] -> [a]
foldTakeWhile pred xs = foldr takeIfPred [] xs
    where takeIfPred x xs | pred x    = x:xs
                          | otherwise = []

-- 5) Implement Data.List's `groupBy` using `foldr`.

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy p = foldr f []
    where f x [] = [[x]]
          f x xs | x `p` head (head xs) = ( x : head xs ) : tail xs
                 | otherwise            = [x] : xs

-- 6) Implement Prelude's `any`, `cycle`, `words`, and `unlines` using
-- folds, if possible.

fany :: (a -> Bool) -> [a] -> Bool
fany p xs = or (let f y ys = (p y) : ys
                in foldr f [] xs)

fcycle :: [a] -> [a]
fcycle xs = foldr (:) (fcycle xs) xs

fwords :: String -> [String]
fwords = foldl' wat []
    where wat []   c = [[c]]
          wat left c | isSpace c = left ++ [[]]
                     | otherwise = init left ++ ((last left ++ [c]) : [])

-- accidentally implemented this instead of funlines at first
funwords :: [String] -> String
funwords = foldr join []
    where join :: String -> String -> String
          join s []    = s
          join s right = s ++ " " ++ right

funlines :: [String] -> String
funlines ls = (foldr join [] ls) ++ "\n"
    where join s []    = s
          join s right = s ++ "\n" ++ right

-- funlines and funwords are easily generalized to `fjoin`, which takes a
-- list of lists and flattens them into a single list and interspersed with
-- a given delimiter
fjoin :: [a] -> [[a]] -> [a]
fjoin delim = foldr join []
    where join s []    = s
          join s right = s ++ delim ++ right

-- this general joiner allows us to rewrite funwords and funlines more simply:
funwords' :: [String] -> String
funwords' = fjoin " "
funlines' :: [String] -> String
funlines' ls = (fjoin "\n" ls) ++ "\n"
