-- returns a list of all permutations of n Bool values (each permutation being a list of Bool)
booleanPermutations :: Int -> [[Bool]]
booleanPermutations 0 = [[]]
booleanPermutations n = (map (True:) (booleanPermutations (n-1))) ++ (map (False:) (booleanPermutations (n-1)))

