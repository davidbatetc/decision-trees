main :: IO ()
main = do
    contents <- readFile "agaricus-lepiota.data"
    putStrLn contents

{-Example to run
Node "cap-color" [("brown",Leaf "poisonous"),("yellow",Leaf "edible"),("white",Node "cap-shape" [("bell",Leaf "edible"),("convex",Leaf "poisonous")])]
-}

data Specimen a b = Specimen a [b] deriving Show
data DT a b = Leaf a | Node String [(b, DT a b)]

-- Creates a String only containing the given number of spaces
spaces :: Int -> String
spaces n = replicate n ' '

-- Instantiation of DT as Show
instance (Show a, Show b) => Show (DT a b) where
    show = show' 0 where
        show' n (Leaf leaf) = spaces n ++ show leaf ++ ['\n']
        show' n (Node node list) =
            spaces n ++ show node ++ ['\n'] ++ concatMap (show'' (n + 2)) list where
                show'' m (branch, dt') = spaces m ++ show branch ++ ['\n'] ++ show' (m + 2) dt'

-- The words function generalized so that it takes the character separing each
--  of the words as an argument
wordsCustom :: Char -> String -> [String]
wordsCustom sep str = words $ map replace str
  where
    replace char
        | char == sep   = ' '
        | otherwise     = char

-- Reads a Specimen from a String, whose elements are separed by the character sep
readSpecimen :: (Read a, Read b) => Char -> String -> Specimen a b
readSpecimen sep str = Specimen (read $ head splitStr) (map read (tail splitStr))
  where
    splitStr = wordsCustom sep str
