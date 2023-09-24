piece' :: Int -> Int
piece' x
    | x < -1 = 2*x + 1
    | x > 3 = -3*x + 7
    | otherwise = -2

exp' :: Int -> Int -> Int
exp' x y
    | y == 0 = 1
    | y == 1 = x
    | otherwise = x * exp' x (y-1)

fxs :: [Int]->[Int]
fxs = map (^2)

gxs :: [Int]->Int
gxs = sum

gfx :: [Int] -> Int
gfx = gxs.fxs

count :: (a -> Bool) -> [a] -> Int
count f [] = 0
count f (h:t)
    | f h = count f t + 1
    | otherwise = count f t

count'filter :: (a -> Bool) -> [a] -> Int
count'filter f xs = length $ filter f xs

count'lc :: (a -> Bool) -> [a] -> Int
count'lc f l = length [ x | x<-l, f x]
