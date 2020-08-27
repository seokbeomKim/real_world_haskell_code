-- file: ch07/filter.hs

main :: IO ()
main = interact (unlines . filter (elem 'a') . lines)