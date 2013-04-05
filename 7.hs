data NestedList a = Elem a | List [NestedList a]


myFlatten :: NestedList a -> [a]

myFlatten (Elem x) = [x]
myFlatten (List xs) = concatMap myFlatten xs

