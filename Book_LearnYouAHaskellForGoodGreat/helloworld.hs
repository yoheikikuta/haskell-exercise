main = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("hey " ++ name ++ ", your rock!")
