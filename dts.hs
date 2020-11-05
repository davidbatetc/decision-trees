main :: IO ()
main = putStrLn "Decision trees."

{-Example to run
Node "cap-color" [("brown",Leaf "poisonous"),("yellow",Leaf "edible"),("white",Node "cap-shape" [("bell",Leaf "edible"),("convex",Leaf "poisonous")])]
-}

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
