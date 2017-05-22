import Control.Arrow
import Data.List(break)

-- (*) Find the last element of a list.
--v1
myLast [x] = x
myLast (_:xs) = myLast xs

--v2
myLast' = head . reverse

--v3
myLast'' xs = xs !! i where i = length xs - 1



-- (*) Find the last but one element of a list.
butLast [x,_] = x
butLast (x:xs) = butLast xs

--v1
butLast' = last . init


--v3
butLast'' xs = xs !! i where i = length xs - 2





--(*) Find the K'th element of a list. The first element in the list is number 1.
--v1
nth xs n = xs !! n-1
--v2
nth' xs n =  head $ drop (n-1) xs



--(*) Find the number of elements of a list.
--v1
myLength list = sum [1 | _ <- list]



--(*) Reverse a list.
--v1
myReverse = reverse

--v2
myReverse' (x:xs) = (myReverse' xs) ++ [x]
myReverse' xs = xs



-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome list = (uncurry (==)) $ (id &&& reverse) list

--v2
isPalindrome' list = list == reverse list



-- Problem 7
-- (**) Flatten a nested list structure.
--
-- Transform a list, possibly holding lists as elements into a `flat' list
-- by replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))


-- 8 Problem 8
-- (**) Eliminate consecutive duplicates of list elements.

compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress (dropWhile (==x) xs)

-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack list@(x:_) = xs : pack rest where (xs, rest) = break (/=x) list


-- Problem 10
-- (*) Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length
-- encoding data compression method.

encode :: Eq a => [a] -> [(Int, a)]
encode = map (length &&& head) . pack



-- Problem 11
-- (*) Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element
-- has no duplicates it is simply copied into the result list.
-- Only elements with duplicates are transferred as (N E) lists.
data Encoded a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = map convert . encode
            where convert (1, x) = Single x
                  convert (n, x) = Multiple n x


-- Problem 12
-- (**) Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11.
-- Construct its uncompressed version.
decodeModified :: [Encoded a] -> [a]
decodeModified = (>>= decoded)
            where decoded (Single x) = [x]
                  decoded (Multiple n x) = replicate n x


-- Problem 13
-- (**) Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates,
-- as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

encodeDirect :: Eq a => [a] -> [Encoded a]

--v1
encodeDirect [] = []
encodeDirect list@(x:_) = encoded n : encodeDirect rest
            where (n, rest) = (length . fst &&& snd) . break (/=x) $ list
                  encoded 1 = Single x
                  encoded n = Multiple n x


-- Problem 14
-- (*) Duplicate the elements of a list.
--
dupli, dupli', dupli'', dupli''' :: [a] -> [a]
dupli = ( >>= replicate 2)
dupli' = concatMap (replicate 2)
dupli'' = foldr (\x l -> x:x:l) []
dupli''' [] = []
dupli''' (x:xs) = x:x:dupli''' xs


-- 5 Problem 15
-- (**) Replicate the elements of a list a given number of times.
repli, repli' :: [a] -> Int -> [a]
repli xs n = (xs >>= replicate n)
repli' xs n = concatMap (replicate n) xs

--
-- 6 Problem 16
-- (**) Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map fst . filter (not . (multipleOf n) . snd)  $ zip xs [1..]
                where multipleOf a b = (==0) $ mod a b
