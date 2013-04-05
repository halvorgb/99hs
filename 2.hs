myButLast :: [a] -> a
myButLast [x,_] = x
myButLast (_:y:ys) = myButLast (y:ys)
