main :: IO ()
main = do
    contents <- readFile "agaricus-lepiota.data"
    putStrLn contents

{-Example to run
Node "cap-color" [("brown",Leaf "poisonous"),("yellow",Leaf "edible"),("white",Node "cap-shape" [("bell",Leaf "edible"),("convex",Leaf "poisonous")])]
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

--generateDT :: [Specimen a b] -> DT a b
--generateDT sps = generateDT' [] sps
--  where
--    generateDT' used sps
