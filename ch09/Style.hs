-- file: ch09/Style.hs
tidyLet = let foo = undefined
              bar = foo * 2
          in undefined

commonDo = do
    something <- undefined
    return ()

-- curly braces and semicolons
preferredLayout = [ (x,y) | x <- [1..a], y <- [1..b] ]
    where b = 7
          a = 6
