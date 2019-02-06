import Control.Monad

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c', r') <- [ (c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
                 ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
                ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
    first <- moveKnight start
    second <- moveKnight first
    moveKnight second

moveXplus :: KnightPos -> KnightPos
moveXplus start = (fst start +1, snd start)
moveXminus :: KnightPos -> KnightPos
moveXminus start = (fst start -1, snd start)
moveYplus :: KnightPos -> KnightPos
moveYplus start = (fst start, snd start + 1)
moveYminus :: KnightPos -> KnightPos
moveYminus start = (fst start, snd start - 1)

modifyStart :: KnightPos -> [KnightPos]
modifyStart start = do
    (c', r') <- [moveXplus start] ++ [moveXminus start] ++ [moveYplus start] ++ [moveYminus start]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

findValidPos :: KnightPos -> KnightPos -> [KnightPos]
findValidPos start end 
    | end `elem` (concat $ in3 <$> modifyStart start) = [pos | pos <- (modifyStart start), end `elem` (in3 pos)]
    | otherwise = [(0,0)]

canReachIn3 :: KnightPos -> KnightPos -> String
canReachIn3 start end
    | end `elem` (in3 start) = show start ++ " can reach " ++ show end ++ " in 3 steps"
    | (findValidPos start end) /= [(0,0)] = show (findValidPos start end) ++ " can reach " ++ show end ++ " in 3 steps"
    | otherwise = show start ++ " can NOT reach " ++ show end ++ " in 3 steps"
