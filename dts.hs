--So as to use group and sort
import           Data.List

{-Examples to run
Node "cap-color" [("brown",Leaf "poisonous"),("yellow",Leaf "edible"),("white",Node "cap-shape" [("bell",Leaf "edible"),("convex",Leaf "poisonous")])]
[Specimen "poisonous" ["convex", "brown", "black"], Specimen "edible" ["convex", "yellow", "black"], Specimen "edible" ["bell", "white", "brown"], Specimen "poisonous" ["convex", "white", "brown"], Specimen "edible" ["convex", "yellow", "brown"], Specimen "edible" ["bell", "white", "brown"], Specimen "poisonous" ["convex", "white", "pink"]]
[Specimen "edible" ["bell", "brown"], Specimen "poisonous" ["convex", "brown"], Specimen "edible" ["bell", "brown"], Specimen "poisonous" ["convex", "pink"]]
-}

data Specimen a b = Specimen a [b]
data DT a b = Leaf a | Node String [(b, DT a b)]


--Creates a String only containing the given number of spaces
spaces :: Int -> String
spaces n = replicate n ' '

--Instantiation of Specimen as Show
instance (Show a, Show b) => Show (Specimen a b) where
    show (Specimen x ys) = "\x1b[31;1mSpecimen\x1b[33;1m " ++ show x ++ " \x1b[0m" ++ show ys

--Instantiation of DT as Show
instance (Show a, Show b) => Show (DT a b) where
    show = show' 0 where
        show' n (Leaf leaf) = spaces n ++ show leaf ++ "\n"
        show' n (Node node list) =
            spaces n ++ "\x1b[32;1m" ++ show node ++ "\x1b[0m\n" ++ concatMap (show'' (n + 2)) list where
                show'' m (branch, dt') = spaces m ++ "\x1b[33;1m" ++ show branch ++ "\x1b[0m\n" ++ show' (m + 2) dt'


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
readSpecimenCc :: Char -> String -> Specimen Char Char
readSpecimenCc sep str = Specimen (unpack $ head splitStr) (map unpack $ tail splitStr)
  where
    splitStr = wordsCustom sep str
    unpack [x] = x  --We could have used head instead, but it would have matched
                    -- Strings that don't have only one character, potentially
                    -- giving unexpected behavior in other parts of the program.


--(Eq a) suffices, using (Ord a) is a too strong condition
--Returns the most common element in a list
findNCountMode :: (Ord a) => [a] -> (Int, a)
findNCountMode xs = maximum (gathered xs)
    where gathered = map (\ys -> (length ys, head ys)) . group . sort


findNCountClassMode :: (Ord a) => [Specimen a b] -> (Int, a)
findNCountClassMode sps = findNCountMode (map getClass sps)
  where getClass (Specimen x _) = x


--Creates a list in which every element is a pair that contains one of the
-- attribute id's provided in 'unused', and a sublist made out of triplets
-- (value, appearances, class), where
-- 1. value: is one of the values of the attribute corresponding to the id.
-- 2. class: is one of the possible classes.
-- 3. appearances: is the number of appearances of the combination class-value
--   in the list of Specimens provided.
createAppsList :: (Ord a, Ord b) => [Int] -> [Specimen a b] -> [(Int, [(b, Int, a)])]
createAppsList unused sps = map gathered unused
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
createAccList :: (Ord a, Ord b) => [(Int, [(b, Int, a)])] -> [(Int, Int)]
createAccList ws = sortBy (\(x, _) (y, _) -> compare y x) $ map appAttrIdPair ws
  where
    appAttrIdPair (attrId, zs) = (countFirstApp zs, attrId)
    countFirstApp []                  = 0
    countFirstApp ((val, app, _):zs') = countFirstApp' zs' val app
    countFirstApp' [] _ n = n
    countFirstApp' ((newVal, app, _):zs') oldVal n
        | newVal == oldVal   = countFirstApp' zs' oldVal n
        | otherwise          = countFirstApp' zs' newVal (n + app)


createBranchingList :: (Ord a, Ord b) => [(b, Int, a)] -> [(b, Int, a)]
createBranchingList [] = []
createBranchingList ((val, app, class'):zs) = (val, app, class') : onlyFirstApp zs val
  where
    onlyFirstApp [] _ = []
    onlyFirstApp ((newVal, app', class''):zs') oldVal
        | newVal == oldVal   = onlyFirstApp zs' oldVal
        | otherwise          = (newVal, app', class'') : onlyFirstApp zs' newVal


generateDT' :: (Ord a, Ord b) => [Int] -> [Specimen a b] -> a -> Int -> DT a b
generateDT' [] _ clMode' _ = Leaf clMode'
generateDT' unused sps' clMode' clModeCount'
    | length sps' == clModeCount'   = Leaf clMode'
    | otherwise
    =   Node ("Attribute " ++ show bestAttrId) (map newTree branchingList)
  where
    newTree (val, app, class') = (val, generateDT' newUnused (cleanSps val) class' app)
    newUnused = filter (bestAttrId /=) unused
    cleanSps val = filter (spMatchesVal val) sps'
    spMatchesVal val (Specimen _ ys) = (ys !! bestAttrId) == val
    bestAttrId = snd $ head $ createAccList appsList
    branchingList = createBranchingList . (\(Just x) -> x) $ lookup bestAttrId appsList
    appsList = createAppsList unused sps'


generateDT :: (Ord a, Ord b) => [Specimen a b] -> DT a b
generateDT sps = generateDT' [0..nAttrs sps - 1] sps clMode clModeCount
  where
    nAttrs (Specimen _ ys : _) = length ys
    (clModeCount, clMode) = findNCountClassMode sps


classifySpecimen :: (Eq b, Show a, Read b) => DT a b -> IO String
classifySpecimen (Leaf class')    = return $ "\x1b[31;1mPrediction: \x1b[0m" ++ show class'
classifySpecimen (Node name list) = do
    putStrLn $ "\x1b[32;1mWhich " ++ show name ++ "?\x1b[0m"
    val <- getLine
    classifySpecimen $ (\(Just x) -> x) $ lookup (read val) list


classifySpecimenCc :: DT Char Char -> IO String
classifySpecimenCc (Leaf class')    = return $ "\x1b[31;1mPrediction: \x1b[0m" ++ [class']
classifySpecimenCc (Node name list) = do
    putStrLn $ "\x1b[32;1mWhich " ++ show name ++ "?\x1b[0m"
    val <- getLine
    classifySpecimenCc $ (\(Just x) -> x) $ lookup ((\[x] -> x) val) list


--Provided as an example, not used
generalizedMain :: String -> IO ()
generalizedMain fileName = do
    content <- readFile fileName
    interaction <- classifySpecimen (readSpecimenList content :: DT String (Int, String))
    putStrLn interaction
  where
    readSpecimenList = generateDT . map (readSpecimen ';') . lines


main :: IO ()
main = do
    content <- readFile "agaricus-lepiota.data"
    interaction <- classifySpecimenCc (readSpecimenCcList content)
    putStrLn interaction
  where
    readSpecimenCcList = generateDT . map (readSpecimenCc ',') . lines
