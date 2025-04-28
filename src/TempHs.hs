
    module TempHs(main) where
    import Factorials (combinations_count)
    main :: IO ()
    main = do 
        putStrLn ""
        putStrLn "Number of ways to place 8 queens on an 8x8 board:"
        print $ combinations_count 64 8

