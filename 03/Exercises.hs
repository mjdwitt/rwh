import ListADT
import Tree
import qualified Data.List as L



-- First set ---------------------------------------------------------

-- 1) Write the converse of `fromList` for the `List` type: a
-- function that takes a `List a` and generates a `[a]`.
fromCons (Cons x xs) = x:(fromCons xs)
fromCons Nil	     = []

-- 2) Define a tree type that has only one constructor, like our Java
-- example. Instead of the `Empty` constructor, use the `Maybe` type to
-- refer to a node's children.
data AltTree a = AltNode a (Maybe (AltTree a)) (Maybe (AltTree a))
		 deriving (Show)

rightsRight = AltNode "rightsRight" Nothing Nothing
left	    = AltNode "left" Nothing Nothing
right	    = AltNode "right" Nothing (Just rightsRight)
parent	    = AltNode "parent" (Just left) (Just right)

-- Some comments at the site seem to think that an empty tree is
-- sometimes needed. To do such a thing, Morten Krogh proposes this:

data KTree a = KTree (Maybe (a, KTree a, KTree a))
	       deriving (Show)

emptyTree = KTree Nothing



-- Second set --------------------------------------------------------

-- 1) Write a function that computes the number of elements in a list.
-- To test it, ensure that it gives the same answers as the standard
-- `length` function.

myLen :: [a] -> Integer  -- 2) Add a type signature
myLen (x:xs) = 1 + myLen xs
myLen []     = 0

-- 3) Write a function that computes the mean of a list.

mean list = (sum list) / (fromIntegral (length list))

-- 4) Turn a list into a palindrom, i.e. it should read the same both
-- backwards and forwards. For example, given [1,2,3],
-- `palindromize [1,2,3]` should return `[1,2,3,3,2,1]`.

palindromize :: [a] -> [a]
palindromize list = list ++ reverse list

-- 5) Write a function that determines whether the given list is a
-- palindrome.

palindrome :: Eq a => [a] -> Bool
palindrome (x:xs)
        | x == last xs = palindrome (init xs)
        | otherwise    = False
palindrome []         = True

-- 6) Create a function that sorts a list of lists based on the length
-- of each sublist. (See `sortBy` from `Data.List`.)

sortListOfLists :: [[a]] -> [[a]]
sortListOfLists lists = L.sortBy compLists lists
    where compLists :: [a] -> [a] -> Ordering
          compLists xs ys = compare (length xs) (length ys)

-- 7) Define a function that joins a list of lists together using a
-- separator value.

intersperse :: a -> [[a]] -> [a]
intersperse sep (xs:xss)
        | null xss  = xs
        | otherwise = xs ++ [sep] ++ (intersperse sep xss)
intersperse _   []  = []

-- 8) Using the binary tree type that we defined earlier in this
-- chapter, write a function that will determine the height of a tree.

height :: Tree a -> Integer
height (Node _ left right) = 1 + (max (height left) (height right))
height Empty               = 0

-- 9) Consider three two-dimensional points a, b, and c. If we look
-- at the angle formed by the line segment from a to b and the line
-- segment from b to c, it either turns left, turns right, or forms a
-- straight line. Define a `Direction` data type that lets you
-- represent these possibilities.

data Direction = Left
               | Right
               | Straight
	         deriving(Eq,Show)

-- 10) Write a function that calculates the turn made by three 2D
-- points and returns a `Direction`.

direction :: (Ord a, Num a) => (a,a) -> (a,a) -> (a,a) -> Direction
direction a b c | theta < 0  = Main.Right
                | theta == 0 = Main.Straight
                | theta > 0  = Main.Left
    where theta = ccw b a c
          ccw :: Num a => (a,a) -> (a,a) -> (a,a) -> a
          ccw (bx,by) (ax,ay) (cx,cy) = (ax - bx)*(cy - by) - (by - ay)*(cx - ax)

-- 11) Define a function that takes a list of points and computes the
-- direction of each successive triple.

directionList :: (Ord a, Num a) => [(a, a)] -> [Direction]
directionList (a:b:c:ps) = (direction a b c) : (directionList (b:c:ps))
directionList _          = []

-- 12) Implement Graham's scan algorithm for the convex hull of a set
-- of 2D points (See Wikipedia).

--grahams :: (Ord a, Num a) => [(a, a)] -> [(a, a)]
grahams ps 
        | length ps >= 3 = scan (sort (L.nub ps))
        | otherwise      = []
    where scan (a:b:c:ps) 
              | direction a b c == Main.Left = grahams (b:c:ps)
              | otherwise                    = grahams (a:c:ps)
          scan (y:z:_)                       = [y, z]
          sort ps = let p = L.minimumBy lower ps
                          where lower :: (Ord a, Num a) => (a,a) -> (a,a) -> Ordering
                                lower (_, yp) (_, yq) = compare yp yq
                    in  p : (L.sortBy (compareAngles p) (L.delete p ps))
                          where compareAngles p a b =
                                    compare (angle p a) (angle p b)
                                angle (xp, yp) (xq, yq) =
                                    (sqrt (dy^2 + dx^2)) / dx
                                  where dy = yq - yp
                                        dx = xq - xp
-- well, that compiles and that's good enough for me
