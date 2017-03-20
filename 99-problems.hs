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



-- 7 Problem 7
-- (**) Flatten a nested list structure.
--
-- Transform a list, possibly holding lists as elements into a `flat' list
-- by replacing each list with its elements (recursively).




-- 9 Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack list@(x:_) = xs : pack rest where (xs, rest) = break (/=x) list
