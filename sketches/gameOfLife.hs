-- game of life naive / own implementation
-- 0 are dead, 1s are alive

{-
Any live cell with fewer than two live neighbors dies, as if by underpopulation.
Any live cell with two or three live neighbors lives on to the next generation.
Any live cell with more than three live neighbors dies, as if by overpopulation.
Any dead cell with exactly three live neighbors becomes a live cell, as if by reproduction.
-}

sampleMatrix = [[0, 1, 0], [1, 0, 1], [0, 0, 0]]

getIndexValue ms (x,y) | y < 0 || x < 0 = 0
                       | (y < length ms) && (x < length (ms !! 0)) = ms !! y !! x
                       | otherwise = 0

sumNeigbours x y ms = sum (map getNeighbours [(x, y-1), (x-1, y), (x+1, y), (x, y+1)])
                      where
                        getNeighbours = getIndexValue ms

decideIfAlive value neighbours | value == 1 && neighbours < 2  = 0
                               | value == 1 && neighbours < 4  = 1
                               | value == 1 && neighbours == 4 = 0
                               | value == 0 && neighbours == 3 = 1
                               | otherwise                     = 0

calculateCell x y ms = decideIfAlive value neighbours
                       where
                         value = ms !! y !! x
                         neighbours = sumNeigbours x y ms

calculateTable ms = [[ calculateCell x y ms | x <- [0..((length (ms !! 0))-1)]] | y <- [0..((length ms)-1)] ]
