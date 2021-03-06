import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    pop
    pop

--pop :: Stack -> (Int, Stack)
--pop (x:xs) = (x, xs)
--
--push :: Int -> Stack -> ((), Stack)
--push a xs = ((), a:xs)
--
--stackManip :: Stack -> (Int, Stack)
--stackManip stack = let
--    ((), newStack1) = push 3 stack
--    (a, newStack2)  = pop newStack1
--    in pop newStack2
