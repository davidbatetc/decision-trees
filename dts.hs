--So as to use group and sort
import           Data.List

main :: IO ()
main = do
    contents <- readFile "agaricus-lepiota.data"
    putStrLn contents

{-Examples to run
Node "cap-color" [("brown",Leaf "poisonous"),("yellow",Leaf "edible"),("white",Node "cap-shape" [("bell",Leaf "edible"),("convex",Leaf "poisonous")])]
[Specimen "poisonous" ["convex", "brown", "black"], Specimen "edible" ["convex", "yellow", "black"], Specimen "edible" ["bell", "white", "brown"], Specimen "poisonous" ["convex", "white", "brown"], Specimen "edible" ["convex", "yellow", "brown"], Specimen "edible" ["bell", "white", "brown"], Specimen "poisonous" ["convex", "white", "pink"]]
[Specimen "edible" ["bell", "brown"], Specimen "poisonous" ["convex", "brown"], Specimen "edible" ["bell", "brown"], Specimen "poisonous" ["convex", "pink"]]
-}

data Specimen a b = Specimen a [b] deriving Show
data DT a b = Leaf a | Node String [(b, DT a b)]


--Creates a String only containing the given number of spaces
spaces :: Int -> String
spaces n = replicate n ' '


--Instantiation of DT as Show
instance (Show a, Show b) => Show (DT a b) where
    show = show' 0 where
        show' n (Leaf leaf) = spaces n ++ show leaf ++ ['\n']
        show' n (Node node list) =
            spaces n ++ show node ++ ['\n'] ++ concatMap (show'' (n + 2)) list where
                show'' m (branch, dt') = spaces m ++ show branch ++ ['\n'] ++ show' (m + 2) dt'


--The words function generalized so that it takes the character separing each
-- of the words as an argument
wordsCustom :: Char -> String -> [String]
wordsCustom sep str = words $ map replace str
  where
    replace char
        | char == sep   = ' '
        | otherwise     = char


--Reads a Specimen from a String, whose elements are separed by the character sep
-- using the built-in read function. This means that Strings have to be given
-- with \"\" and Chars with \'\'. This function is not used in the program, but
-- it is provided so as to show how a generalized Specimen would be read.
readSpecimen :: (Read a, Read b) => Char -> String -> Specimen a b
readSpecimen sep str = Specimen (read $ head splitStr) (map read (tail splitStr))
    where splitStr = wordsCustom sep str


--Reads a Specimen Char Char from a String, whose elements are separed by the
-- character sep. Note that in this case "x,y,z" will give (Specimen x [y,z]),
-- whereas with readSpecimen, in order to obtain the same result, we would have
-- to write readSpecimen "\'x\',\'y\',\'z\'" :: Specimen Char Char.
readSpecimenCC :: Char -> String -> Specimen Char Char
readSpecimenCC sep str = Specimen (unpack $ head splitStr) (map unpack $ tail splitStr)
  where
    splitStr = wordsCustom sep str
    unpack [x] = x  --We could have used head instead, but it would have matched
                    -- Strings that don't have only one character, potentially
                    -- giving unexpected behavior in other parts of the program.


spClass :: Specimen a b -> a
spClass (Specimen x _) = x


--(Eq a) suffices, using (Ord a) is a too strong condition
--Returns the most common element in a list
mostCommon :: (Ord a) => [a] -> a
mostCommon xs = snd $ maximum (gathered xs)
    where gathered = map (\ys -> (length ys, head ys)) . group . sort


--Creates a list in which every element is a pair that contains one of the
-- attribute id's provided in 'unused', and a sublist made out of triplets
-- (value, appearances, class), where
-- 1. value: is one of the values of the attribute corresponding to the id.
-- 2. class: is one of the possible classes.
-- 3. appearances: is the number of appearances of the combination class-value
--   in the list of Specimens provided.
appsList :: (Ord a, Ord b) => [Int] -> [Specimen a b] -> [(Int, [(b, Int, a)])]
appsList unused sps = map gathered unused
  where
    gathered attrId = (attrId, sortBy customOrder $ map valNumClassTriplet (groupedBag attrId))
    valNumClassTriplet zs = (fst $ head zs, length zs, snd $ head zs)
    groupedBag attrId = group $ sort $ map (classValPair attrId) sps
    classValPair attrId (Specimen x ys) = (ys !! attrId, x)
    customOrder (val1, app1, _) (val2, app2, _)
        | val1 < val2                       = LT
        | (val1 == val2) && (app1 >= app2)  = LT
        | otherwise                         = GT

-- Accuracy list
accList :: (Ord a, Ord b) => [(Int, [(b, Int, a)])] -> [(Int, Int)]
accList ws = sortBy (\(x, _) (y, _) -> compare y x) $ map whatever ws
  where
    whatever (attrId, zs) = (countFirstApp zs, attrId)
    countFirstApp []                  = 0
    countFirstApp ((val, app, _):zs') = countFirstApp' zs' val app
    countFirstApp' [] _ n = n
    countFirstApp' ((newval, app, _):zs') val n
        | newval == val   = countFirstApp' zs' val n
        | otherwise       = countFirstApp' zs' newval (n + app)


--generateDT :: [Specimen a b] -> DT a b
--generateDT sps = generateDT' [] sps
--  where
--    generateDT' used sps
