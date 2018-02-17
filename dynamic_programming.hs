fibs = scanl (+) 0 (1:fibs)

r :: [(Int, [Int])]
r = [(0, [])]

p :: [(Int, [Int])]
p = [(1, [1]), (5, [2]), (8, [3]), (9, [4]), (10, [5]), (17, [6]), (17, [7]), (20, [8]), (24, [9]), (30, [10])]

rod_cut_aux :: Int ->[(Int, [Int])] ->  [(Int, [Int])] -> (Int, [Int]) 
rod_cut_aux n p r = foldr max (0,[]) $ zipWith (\ a b -> (fst a + fst b, snd a ++ snd b)) (take n p) r

rod_cut n p r = loop 1 p r
                where loop i p r
                        | i > n = r
                        | otherwise = loop (i + 1) p (rod_cut_aux i p r : r)
