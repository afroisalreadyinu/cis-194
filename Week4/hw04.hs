-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

funz x = if even x then x `div` 2 else 3*x+1

fun2' :: Integer -> Integer
fun2' n = sum . (filter even) . takeWhile (/= 1) $ iterate funz n

--------------------------------------------------------------------------------
-- Exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)


foldTree :: [a] -> Tree a
foldTree as = foldl makeTree Leaf as

treeDepth :: Tree a -> Integer
treeDepth Leaf = (-1)
treeDepth (Node d _ _ _) = d

makeTree :: Tree a -> a -> Tree a
makeTree Leaf a = Node 0 Leaf a Leaf
makeTree (Node depth leftNode val rightNode) a
    | treeDepth leftNode <= treeDepth rightNode =
        let newLeft = makeTree leftNode a
            leftDepth = treeDepth newLeft
        in Node ((max leftDepth (treeDepth rightNode)) + 1) newLeft val rightNode
    | otherwise =
        let newRight = makeTree rightNode a
            rightDepth = treeDepth newRight
        in Node ((max (treeDepth leftNode) rightDepth) + 1) leftNode val newRight

--------------------------------------------------------------------------------
-- Exercise 3

xor :: [Bool] -> Bool
xor = foldl blah False
    where blah b1 b2 = if (not b1) then b2 else b1 && (not b2)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr yada []
    where yada a rs = (f a) : rs


sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
    let skipped = [i + j + 2*i*j | i <- [1..n], j <- [1..n], i <= j]
        picked = [ x | x <- [1..n], x `notElem` skipped]
    in map (\x -> 2*x + 1) picked
