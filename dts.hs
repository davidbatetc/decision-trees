{-Examples to run
Node "cap-color" [("brown",Leaf "poisonous"),("yellow",Leaf "edible"),("white",Node "cap-shape" [("bell",Leaf "edible"),("convex",Leaf "poisonous")])]
[Specimen "poisonous" ["convex", "brown", "black"], Specimen "edible" ["convex", "yellow", "black"], Specimen "edible" ["bell", "white", "brown"], Specimen "poisonous" ["convex", "white", "brown"], Specimen "edible" ["convex", "yellow", "brown"], Specimen "edible" ["bell", "white", "brown"], Specimen "poisonous" ["convex", "white", "pink"]]
[Specimen "edible" ["bell", "brown"], Specimen "poisonous" ["convex", "brown"], Specimen "edible" ["bell", "brown"], Specimen "poisonous" ["convex", "pink"]]
merge' (<) (==) (zip [1, 1, 1, 1, 2, 3, 4] [1, 1, 1, 1, 1, 1, 1]) (zip [1, 1, 1, 2, 2, 3] [1, 1, 1, 1, 1, 1])
-}

{-Benchmarking
Version with naïve merge-group sort: 4.45s
Version with optimized merge-group sort: 2.17s
Version with optimized access: 1.6s
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
wordsCustom sep = words . map replace
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


merge :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge _ [] ms = ms
merge _ ns [] = ns
merge comp (n:ns) (m:ms)
  | comp n m == LT  = n : merge comp ns (m:ms)
  | otherwise       = m : merge comp (n:ns) ms


--Given a comparison function, runs merge sort on a given list
msortBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
msortBy _ [] = []
msortBy comp ws = mergeAll (map (:[]) ws)
  where
    mergeAll [xs] = xs
    mergeAll xss  = mergeAll (mergePairs xss)
    mergePairs (xs:ys:zss) = merge comp xs ys : mergePairs zss
    mergePairs xs          = xs


mergeGroup :: (Ord a) => (a -> a -> Ordering) -> [(Int, a)]
    -> [(Int, a)] -> [(Int, a)]
mergeGroup _ [] cxs = cxs
mergeGroup _ cys [] = cys
mergeGroup comp ((c1, x):cxs) ((c2, y):cys)
    | compRes == EQ   = (c1 + c2, x) : mergeGroup comp cxs cys
    | compRes == LT   = (c1, x) : mergeGroup comp cxs ((c2, y):cys)
    | otherwise        = (c2, y) : mergeGroup comp ((c1, x):cxs) cys
  where compRes = comp x y


mgsortBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> [(Int, a)]
mgsortBy _ [] = []
mgsortBy comp (w:ws) = mergeAll $ map (:[]) (spanCount ws w 1)
  where
    spanCount [] oldx counter = [(counter, oldx)]
    spanCount (x:xs) oldx counter
        | comp x oldx == EQ   = spanCount xs oldx (counter + 1)
        | otherwise           = (counter, oldx) : spanCount xs x 1

    mergeAll []    = []
    mergeAll [cxs] = cxs
    mergeAll cxss  = mergeAll (mergePairs cxss)
    mergePairs (cxs:cys:czss) = mergeGroup comp cxs cys : mergePairs czss
    mergePairs cxs            = cxs


--Creates a list in which every element is a pair that contains one of the
-- attribute id's provided in 'unused', and a sublist made out of
-- (appearances, (value, class)), where
-- 1. value: is one of the values of the attribute corresponding to the id.
-- 2. class: is one of the possible classes.
-- 3. appearances: is the number of appearances of the combination class-value
--   in the list of Specimens provided.
createAppsList :: (Ord a, Ord b) => [Specimen a b] -> [[(Int, (b, a))]]
createAppsList sps = createAppsList' attrsMat
  where
    cls = map (\(Specimen cl _) -> cl) sps
    attrsMat = map (\(Specimen _ attrs) -> attrs) sps

    createAppsList' [] = []
    createAppsList' attrsMat'
      | null $ head attrsMat'   = []
      | otherwise
      =    singleList (map head attrsMat') : createAppsList' (map tail attrsMat')

    singleList = msortBy customOrder . mgsortBy compare . flip zip cls
    customOrder (app1, (val1, _)) (app2, (val2, _))
        | val1 < val2                       = LT
        | (val1 == val2) && (app1 >= app2)  = LT
        | otherwise                         = GT


--Chooses the best attribute
chooseBestAttrId :: (Ord a, Ord b) => [[(Int, (b, a))]] -> Int
chooseBestAttrId ws = snd $ maximum $ zip (map countFirstApp ws) [0..length ws - 1]
  where
    countFirstApp [] = 0
    countFirstApp ((app, (val, _)):zs')
        = app + countFirstApp (dropWhile (\(_, (v, _)) -> v == val) zs')


--Creates a list choosing only the information of the most common class
-- for each of the values
createBranchingList :: (Ord a, Ord b) => [(Int, (b, a))] -> [(Int, (b, a))]
createBranchingList [] = []
createBranchingList ((app, (val, cl)):zs)
    = (app, (val, cl)) : createBranchingList ys
  where
    ys = dropWhile (\(_, (v, _)) -> v == val) zs


--Returns the number of appearances of the most commont class in
-- a list of specimens, along with the class itself
findNCountClassMode :: (Ord a) => [Specimen a b] -> (Int, a)
findNCountClassMode = maximum . mgsortBy compare . map (\(Specimen x _) -> x)


removeNth :: [a] -> Int -> [a]
removeNth xs n = removeNth' xs n 0
  where
    removeNth' [] _ _ = []
    removeNth' (y:ys) m i
      | i == n      = ys
      | otherwise   = y : removeNth' ys m (i + 1)


generateDT' :: (Ord a, Ord b) => [String] -> [Specimen a b] -> a -> Int -> DT a b
generateDT' [] _ clMode' _ = Leaf clMode'
generateDT' attrNames sps' clMode' clModeCount'
    | length sps' == clModeCount'   = Leaf clMode'
    | otherwise
    =   Node (attrNames !! bestAttrId) (map newTree branchingList)
  where
    newTree (app, (val, cl)) = (val, generateDT' newAttrNames (cleanSps val) cl app)
    cleanSps val = removeVals $ filter (spMatchesVal val) sps'
    spMatchesVal val (Specimen _ ys) = (ys !! bestAttrId) == val
    removeVals = map (\(Specimen x ys) -> Specimen x (removeNth ys bestAttrId))
    newAttrNames = removeNth attrNames bestAttrId

    branchingList = createBranchingList (appsList !! bestAttrId)
    bestAttrId = chooseBestAttrId appsList
    appsList = createAppsList sps'


generateDT :: (Ord a, Ord b) => [String] -> [Specimen a b] -> DT a b
generateDT attrNames sps = generateDT' attrNames sps clMode clModeCount
  where (clModeCount, clMode) = findNCountClassMode sps


classifySpecimen :: (Eq b, Show a, Read b) => DT a b -> IO String
classifySpecimen (Leaf cl)    = return $ "\x1b[31;1mPrediction: \x1b[0m" ++ show cl
classifySpecimen (Node name list) = do
    putStrLn $ "\x1b[32;1mWhich " ++ name ++ "?\x1b[0m"
    val <- getLine
    classifySpecimen $ (\(Just x) -> x) $ lookup (read val) list


classifySpecimenCc :: DT Char Char -> IO String
classifySpecimenCc (Leaf cl)    = return $ "\x1b[31;1mPrediction: \x1b[0m" ++ [cl]
classifySpecimenCc (Node name list) = do
    putStrLn $ "\x1b[32;1mWhich " ++ name ++ "?\x1b[0m"
    val <- getLine
    classifySpecimenCc $ (\(Just x) -> x) $ lookup ((\[x] -> x) val) list


--Provided as an example, not used
generalizedMain :: String -> IO ()
generalizedMain fileName = do
    content <- readFile fileName
    interaction <- classifySpecimen (readSpecimenList content :: DT String (Int, String))
    putStrLn interaction
  where
    readSpecimenList content' = generateDT attrNames sps
      where
        sps = map (readSpecimen ';') $ lines content'
        attrNames = ["Attribute " ++ show i | i <- [1..length sps]]


showTree :: String -> IO (DT Char Char)
showTree fileName = do
    content <- readFile fileName
    return $ readSpecimenCcList content
  where
    readSpecimenCcList content' = generateDT attrNames sps
      where
        sps = map (readSpecimenCc ',') $ lines content'
        attrNames = ["Attribute " ++ show i | i <- [1..length sps]]


main :: IO ()
main = do
    content <- readFile "agaricus-lepiota.data"
    interaction <- classifySpecimenCc (readSpecimenCcList content)
    putStrLn interaction
  where
    readSpecimenCcList content' = generateDT attrNames sps
      where
        sps = map (readSpecimenCc ',') $ lines content'
        attrNames = ["cap-shape", "cap-surface", "cap-color"
            , "bruises?", "odor", "gill-attachment", "gill-spacing"
            , "gill-size", "gill-color", "stalk-shape"
            , "stalk-root", "stalk-surface-bove-ring"
            , "stalk-color-above-ring", "stalk-color-below-ring"
            , "veil-type", "veil-color", "ring-number", "ring-type"
            , "spore-print-color", "population", "habitat"]
