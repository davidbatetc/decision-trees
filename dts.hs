--------------------------------------------------------------------------------
-- | UTILITIES: Functions that are not directly related to this project.
--------------------------------------------------------------------------------

-- | Creates a String which only contains the given number of spaces.
spaces :: Int -> String
spaces n = replicate n ' '

-- | The words function generalized so that it takes the character separing
-- each of the words as an argument. Adapted from the official implementation
-- of words.
wordsCustom :: Char -> String -> [String]
wordsCustom sep s = case dropWhile (== sep) s of
    "" -> []
    s' -> w : wordsCustom sep s'' where (w, s'') = break (== sep) s'

-- | Given a comparison function, runs merge sort on a given list.
msortBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
msortBy _ [] = []
msortBy comp ws = mergeAll (map (:[]) ws)
  where
    mergeAll [xs] = xs
    mergeAll xss  = mergeAll (mergePairs xss)

    mergePairs (xs:ys:zss) = merge xs ys : mergePairs zss
    mergePairs xs          = xs

    merge [] ms = ms
    merge ns [] = ns
    merge (n:ns) (m:ms)
        | comp n m == LT   = n : merge ns (m:ms)
        | otherwise        = m : merge (n:ns) ms

-- | Auxiliary function for mgsortBy, which is defined below this function.
-- Given a comparison function and two lists of pairs (repetitions, value)
-- merges the two lists. The pairs which have the same value are merged into one.
--
-- Preconditions:
-- 1. The values of the lists are sorted according to the comparison function.
-- 2. In each of the lists, each value appears at most once.
mergeGroup :: (Ord a) => (a -> a -> Ordering) -> [(Int, a)]
    -> [(Int, a)] -> [(Int, a)]
mergeGroup _ [] cxs = cxs
mergeGroup _ cys [] = cys
mergeGroup comp ((c1, x):cxs) ((c2, y):cys)
    | compRes == EQ    = (c1 + c2, x) : mergeGroup comp cxs cys
    | compRes == LT    = (c1, x) : mergeGroup comp cxs ((c2, y):cys)
    | otherwise        = (c2, y) : mergeGroup comp ((c1, x):cxs) cys
  where compRes = comp x y

-- | Given a comparison function, orders a list using an adapted version of a
-- merge sort. This version merges the repeated values of the list into one
-- while counting its repetitions.
--
-- >>> mgsortBy compare [-1, -1, 2, -1, 4, 5, -1, 5, 4, 2, 5]
-- [(4,-1),(2,2),(2,4),(3,5)]
mgsortBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> [(Int, a)]
mgsortBy _ [] = []
mgsortBy comp (w:ws) = mergeAll $ map (:[]) (initialGrouping ws w 1)
  where
    -- This function is thought for cases in which there are a lot of repeated
    -- values in the initial list.
    initialGrouping [] oldx counter = [(counter, oldx)]
    initialGrouping (x:xs) oldx counter
        | comp x oldx == EQ   = initialGrouping xs oldx (counter + 1)
        | otherwise           = (counter, oldx) : initialGrouping xs x 1

    mergeAll []    = []
    mergeAll [cxs] = cxs
    mergeAll cxss  = mergeAll (mergePairs cxss)

    mergePairs (cxs:cys:czss) = mergeGroup comp cxs cys : mergePairs czss
    mergePairs cxs            = cxs

-- | Removes the nth element of a list. Equivalent to using
-- (drop n . take (n + 1)) xs.
removeNth :: [a] -> Int -> [a]
removeNth xs n = removeNth' xs n 0
  where
    removeNth' [] _ _ = []
    removeNth' (y:ys) m i
      | i == n      = ys
      | otherwise   = y : removeNth' ys m (i + 1)


--------------------------------------------------------------------------------
-- | DEFINITIONS, INSTANTIATIONS AND FUNCTIONS TO READ
--------------------------------------------------------------------------------

-- | Specimen encapsulates the concept of example to be fed into the decision
-- tree, while DT is the decision tree itself. Note that the classes and the
-- values can be of any type.
data Specimen a b = Specimen a [b]
data DT a b = Leaf a | Node String [(b, DT a b)]

-- | Specimen is instantiated as Show adding colors to the default instantiation
-- that would arise from using "deriving Show" in the definition of Specimen.
--
-- >>> Specimen "a" [1, 2, 3]
-- Specimen "a" [1,2,3]
instance (Show a, Show b) => Show (Specimen a b) where
    show (Specimen x ys)
        = "\x1b[31;1mSpecimen\x1b[33;1m " ++ show x ++ " \x1b[0m" ++ show ys

-- | DT is instantiated as Show and is displayed as a list with colors in which,
-- using spaces, the children are shown more to the right than their parents .
--
-- >>> Node "a" [(1, Leaf 'A'), (2, Node "b" [(3, Leaf 'B')])]
-- "a"
--   1
--     'A'
--   2
--     "b"
--       3
--         'B'
instance (Show a, Show b) => Show (DT a b) where
    show = show' 0
      where
        show' n (Leaf cl) = spaces n ++ show cl ++ "\n"
        show' n (Node attr list) = spaces n ++ "\x1b[32;1m" ++ show attr
            ++ "\x1b[0m\n" ++ concatMap (show'' (n + 2)) list
        show'' m (val, dt') = spaces m ++ "\x1b[33;1m" ++ show val ++ "\x1b[0m\n"
            ++ show' (m + 2) dt'

-- | Reads a Specimen from a String, whose elements are separed by a given
-- character 'sep', using the built-in 'read' function.
--
-- >>> readSpecimen ';' "1;2.34;4.57;3.698" :: Specimen Int Double
-- Specimen 1 [2.34,4.57,3.698]
-- >>> readSpecimen ',' "e,c,b,n" :: Specimen Char Char
-- *** Exception: Prelude.read: no parse
-- >>> readSpecimen ',' "'e','c','b','n'" :: Specimen Char Char
-- Specimen 'e' "cbn"
--
-- Note: function not used in the main part of the program, but provided for the
-- sake of generality.
readSpecimen :: (Read a, Read b) => Char -> String -> Specimen a b
readSpecimen sep str = Specimen (read $ head splitStr) (map read (tail splitStr))
  where splitStr = wordsCustom sep str

-- | Reads a Specimen Char Char from a String, whose elements are separed by a
-- given character 'sep'.
--
-- >>> readSpecimenCc ',' "e,c,b,n"
-- Specimen 'e' "cbn"
readSpecimenCc :: Char -> String -> Specimen Char Char
readSpecimenCc sep str = Specimen (unpack $ head splitStr) (map unpack $ tail splitStr)
  where
    splitStr = wordsCustom sep str
    unpack [x] = x  -- We could have used head instead, but it would have matched
                    -- Strings that don't have only one character, potentially
                    -- giving unexpected behavior in other parts of the program
                    -- if there was a bug.


--------------------------------------------------------------------------------
-- | CONSTRUCTION OF THE DECISION TREE
--------------------------------------------------------------------------------

-- | Creates a list of sublists with each sublist corresponding to one of the
-- attributes. The sublists consist of tuples (appearances, (value, class)),
-- where
-- 1. value: is one of the values of the attribute associated with the sublist.
-- 2. class: is the most common class among the Specimens with this value
--  for the attribute.
-- 3. appearances: is the number of appearances of the combination value-class
--  in the list of Specimens provided.
--
-- Postcondition: each of the sublists is sorted according to the order defined
-- in 'customOrder'.
createAppsList :: (Ord a, Ord b) => [Specimen a b] -> [[(Int, (b, a))]]
createAppsList sps = createAppsList' valsMat
  where
    cls = map (\(Specimen cl _) -> cl) sps
    valsMat = map (\(Specimen _ vals) -> vals) sps

    -- It goes column by column of the valsMat
    createAppsList' [] = []
    createAppsList' valsMat'
      | null $ head valsMat'   = []  -- If the end of valsMat' has been reached.
      | otherwise
      =   singleList (map head valsMat') : createAppsList' (map tail valsMat')

    singleList = msortBy customOrder . mgsortBy compare . flip zip cls
    customOrder (app1, (val1, cl1)) (app2, (val2, cl2))
        | val1 /= val2   = compare val1 val2
        | app1 /= app2   = compare app2 app1  -- Note the inversion
        | otherwise      = compare cl2 cl1    -- Note the inversion

-- | Given the list resulting from using createAppsList, returns the index of
-- the attribute that is considered the best.
chooseBestAttrId :: (Ord a, Ord b) => [[(Int, (b, a))]] -> Int
chooseBestAttrId ws = snd $ maximum $ zip (map countFirstApp ws) [0..length ws - 1]
  where
    countFirstApp [] = (0, 0, 0) :: (Int, Int, Int)  -- To resolve ambiguity.
    countFirstApp ((app, (val, _)):zs)
        | null zs'    = (app' + app, superAcc' + app, diffVals + 1)
        | otherwise   = (app' + app, superAcc', diffVals + 1)
      where
        (zs', zs'') = span (\(_, (v, _)) -> v == val) zs
        (app', superAcc', diffVals) = countFirstApp zs''

-- | Given one of the sublists resulting from using createAppsList, returns a
-- list selecting only the tuples that contain the most common class for each of
-- the values.
createBranchingList :: (Ord a, Ord b) => [(Int, (b, a))] -> [(Int, (b, a))]
createBranchingList [] = []
createBranchingList ((app, (val, cl)):zs)
    = (app, (val, cl)) : createBranchingList ys
  where ys = dropWhile (\(_, (v, _)) -> v == val) zs

-- | Given a list of Specimens, returns the number of appearances of the most
-- commont class along with the class itself.
findNCountClassMode :: (Ord a) => [Specimen a b] -> (Int, a)
findNCountClassMode = maximum . mgsortBy compare . map (\(Specimen x _) -> x)

-- | Auxiliary function for generateDT.
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

-- | Given the names of the attributes of the Specimens and a list of Specimens,
-- generates a decision tree.
generateDT :: (Ord a, Ord b) => [String] -> [Specimen a b] -> DT a b
generateDT attrNames sps = generateDT' attrNames sps clMode clModeCount
  where (clModeCount, clMode) = findNCountClassMode sps


--------------------------------------------------------------------------------
-- | CLASSIFICATION
--------------------------------------------------------------------------------

-- | Given a decision tree, classifies a Specimen using user interaction.
--
-- >>> classifySpecimen (Node "name" [('P', Leaf 1), ('M', Leaf 2)])
-- <system> Which name?
-- <user> P
-- <system> *** Exception: Prelude.read: no parse
--
-- >>> classifySpecimen (Node "name" [('P', Leaf 1), ('M', Leaf 2)])
-- <system> Which name?
-- <user> 'P'
-- <system> Right "Prediction: 1"
--
-- >>> classifySpecimen (Node "name" [('P', Leaf 1), ('M', Leaf 2)])
-- <system> Which name?
-- <user> 'F'
-- <system> Left "ERROR. Value "'F'" for attribute "name" missing."
--
-- Note: function not used in the main part of the program, but provided for the
-- sake of generality.
classifySpecimen :: (Eq b, Show a, Read b) => DT a b -> IO (Either String String)
classifySpecimen (Leaf cl) = return $ Right ("\x1b[31;1mPrediction: \x1b[0m" ++ show cl)
classifySpecimen (Node name list) = do
    putStrLn $ "\x1b[32;1mWhich " ++ name ++ "?\x1b[0m"
    val <- getLine
    case lookup (read val) list of
        Nothing -> return $ Left ("\x1b[31;1mERROR. \x1b[0mValue "
            ++ show val ++ " for attribute " ++ show name ++ " missing.")
        Just dt -> classifySpecimen dt


-- | Given a decision tree, classifies a Specimen Char Char using user interaction.
--
-- >>> classifySpecimenCc (Node "name" [('P', Leaf 'A'), ('M', Leaf 'B')])
-- <system> Which name?
-- <user> P
-- <system> Right "Prediction: A"
--
-- >>> classifySpecimenCc (Node "name" [('P', Leaf 'A'), ('M', Leaf 'B')])
-- <system> Which name?
-- <user> F
-- <system> Left "ERROR. Value "F" for attribute "name" missing."
classifySpecimenCc :: DT Char Char -> IO (Either String String)
classifySpecimenCc (Leaf cl) = return $ Right ("\x1b[31;1mPrediction: \x1b[0m" ++ [cl])
classifySpecimenCc (Node name list) = do
    putStrLn $ "\x1b[32;1mWhich " ++ name ++ "?\x1b[0m"
    val <- getLine
    case lookup (unpack val) list of
        Nothing -> return $ Left ("\x1b[31;1mERROR. \x1b[0mValue "
            ++ show val ++ " for attribute " ++ show name ++ " missing.")
        Just dt -> classifySpecimenCc dt
  where
    unpack [x] = x
    unpack _   = ' '


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
    case interaction of
        Left errorMessage -> putStrLn errorMessage
        Right prediction  -> putStrLn prediction
  where
    readSpecimenCcList content' = generateDT attrNames sps
      where
        sps = map (readSpecimenCc ',') $ lines content'
        attrNames = ["cap-shape", "cap-surface", "cap-color"
            , "bruises?", "odor", "gill-attachment", "gill-spacing"
            , "gill-size", "gill-color", "stalk-shape"
            , "stalk-root", "stalk-surface-above-ring", "stalk-surface-below-ring"
            , "stalk-color-above-ring", "stalk-color-below-ring"
            , "veil-type", "veil-color", "ring-number", "ring-type"
            , "spore-print-color", "population", "habitat"]
