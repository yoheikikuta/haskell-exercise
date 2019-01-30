import Shapes
main = do
    --print $ Circle (Point 10 20) 30 -- Does not work (expected)
    print $ nudge (baseCircle 30) 10 20
