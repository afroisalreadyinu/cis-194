module Party where

import Employee
import Data.Monoid
import Data.Tree
import System.IO

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp:emps) (fun + empFun emp)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL emps fun) (GL emps2 fun2) = GL (emps ++ emps2) (fun + fun2)

getFun :: GuestList -> Fun
getFun (GL _ fun) = fun

getEmployees :: GuestList -> [Employee]
getEmployees (GL employees _) = employees

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = if (getFun gl1 > getFun gl2) then gl1 else gl2

treeFold :: (b -> a -> [b] -> b) -> b -> Tree a -> b
treeFold fun seed (Node a []) = fun seed a []
treeFold fun seed (Node a cs) = fun seed a $ map (treeFold fun seed) cs

-- without, with
guestListsF :: (GuestList, GuestList) ->
               Employee ->
               [(GuestList, GuestList)] ->
               (GuestList, GuestList)
guestListsF (glSeed1, glSeed2) emp [] = (glSeed1, mappend glSeed2 (GL [emp] (empFun emp)))
guestListsF (glSeed1, glSeed2) emp subGls =
    let without_subordinates = map fst subGls
    in (mconcat . map (uncurry moreFun) $ subGls,
        GL [emp] (empFun emp) `mappend` (mconcat without_subordinates)
       )

bestGuestListF :: Tree Employee -> GuestList
bestGuestListF = (uncurry moreFun) . (treeFold guestListsF (mempty, mempty))

intTree :: Tree Integer
intTree = Node 5 [Node 3 [], Node 2 []]

maxx :: Integer -> Integer -> [Integer] -> Integer
maxx _ _ [] = 0
maxx a b cs = max a $ max b $ foldl1 max cs

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel employee funList =
    ((foldl moreFun mempty (map fst funList)),
     (foldl moreFun mempty (map snd funList)))

--(without, with)
guestLists :: Tree Employee -> (GuestList, GuestList)
guestLists (Node employee []) = (mempty, GL [employee] $ empFun employee)
guestLists (Node employee subordinates) =
    let subGls = map guestLists subordinates
        without_subordinates = map fst subGls
    in (mconcat . map (uncurry moreFun) $ subGls,
       GL [employee] (empFun employee) `mappend` (mconcat without_subordinates)
       )

bestGuestList = (uncurry moreFun) . guestLists


main :: IO ()
main = do
  contents <- readFile "company.txt"
  let company = read contents :: Tree Employee
      gl = guestLists company
      mostFunGuests = uncurry moreFun $ gl
  putStrLn $ "Total fun: " ++ (show $ getFun mostFunGuests)
  putStrLn . unlines . (map empName) . getEmployees $ mostFunGuests
