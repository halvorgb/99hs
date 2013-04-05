data Encoding a = Single a | Multiple Int a
                deriving (Show)


myDecode :: [Encoding a] -> [a]
myDecode = concatMap halp
  where
    halp (Single x) =  [x]
    halp (Multiple n x) =  replicate n x