main :: IO ()
main = return ()

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 30

height :: Int
height = 30

type Board = [Pos]

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width)+1, ((y-1) `mod` height)+1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

surviors :: Board -> [Pos]
surviors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)), isEmpty b p, liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = surviors b ++ births b

boardEmpty :: Board -> Bool
boardEmpty b = and [isEmpty b p | p <- rmdups (concat (map neighbs b))]

-- glider intial condition
-- moves across grid infinitely
glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

-- square initial condition
-- stable life
square :: Board
square = [(4,4),(3,4),(4,3),(3,3)]

-- initail condition that results in death after a few turns
death :: Board
death = [(2,2),(3,3),(4,3),(5,3)]


-- funtion to run the game of life
-- type life (glider,square, death, etc.)
-- Game is over when there are no live cells
-- BEWARE, this could run indefinately 
life :: Board -> IO ()
life b = if not (boardEmpty b) then
            do cls
               showcells b
               wait 500000
               life (nextgen b)
            else
                do
                    cls
                    goto (1,1)
                    putStrLn "Game Over"

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]