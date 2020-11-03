main :: IO ()
main = putStrLn "Decision trees."

data DT a b = Leaf a | Node String [(b, DT a b)]
    deriving Show  -- This "deriving Show" is temporary.
