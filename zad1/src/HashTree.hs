-- Mikołaj Drzewecki 406134


module HashTree (
    MerkleProof (..),
    MerklePath,
    Tree (..),
    leaf,
    twig,
    node,
    treeHash,
    drawTree,
    buildTree,
    showMerklePath,
    merklePaths,
    buildProof,
    verifyProof
) where
    import Hashable32 ( Hashable, Hash, showHash, hash )

    type MerklePath = [Either Hash Hash]

    data Tree a = Leaf Hash a | Twig Hash (Tree a) | Node Hash (Tree a) (Tree a)

    data MerkleProof a = MerkleProof a MerklePath

    treeHash :: Tree a -> Hash
    treeHash (Leaf h _) = h
    treeHash (Twig h _) = h
    treeHash (Node h _ _) = h

    leaf :: Hashable a => a -> Tree a
    leaf x = Leaf (hash x) x

    twig :: Hashable a => Tree a -> Tree a
    twig t = Twig ( hash(treeHash t, treeHash t) ) t

    node :: Hashable a => Tree a -> Tree a -> Tree a
    node l r = Node ( hash(treeHash l, treeHash r)) l r

    buildTree :: Hashable a => [a] -> Tree a
    buildTree (x:xs) = head $ buildTree' $ map leaf (x:xs)
        where
            buildTree' :: Hashable a => [Tree a] -> [Tree a]
            buildTree' [y] = [y]
            buildTree' ys = buildTree' $ group ys
                where
                    group :: Hashable a => [Tree a] -> [Tree a]
                    group [] = []
                    group [y] = [twig y]
                    group (st:nd:zs) = node st nd : group zs

    drawTree :: Show a => Tree a -> String
    drawTree t = drawNestedTree t 0 0
        where
            drawNestedTree :: Show a => Tree a -> Int -> Int -> String
            drawNestedTree (Leaf h v) 0 _ = showHash h ++ " " ++ show v ++ "\n"
            drawNestedTree (Twig h l) 0 n = showHash h ++ " +\n" ++ drawNestedTree l (n + 1) (n + 1)
            drawNestedTree (Node h l r) 0 n = showHash h ++ " -\n" ++ drawNestedTree l (n + 1) (n + 1) ++ drawNestedTree r (n + 1) (n + 1)
            drawNestedTree nt m n = ' ' : ' ' : drawNestedTree nt (m - 1) n

    showMerklePath :: MerklePath -> String
    showMerklePath mp = let xs = map fromEither' mp in concat xs
        where
            fromEither' :: Either Hash Hash -> String
            fromEither' (Left h) = '>' : showHash h
            fromEither' (Right h) = '<' : showHash h

    instance Show a => Show (MerkleProof a) where
        showsPrec 11 (MerkleProof a mp) = showString "(MerkleProof " . shows a . showChar ' ' . showString (showMerklePath mp) . showChar ')'
        showsPrec _ (MerkleProof a mp) = showString "MerkleProof (" . shows a . showString ") " . showString (showMerklePath mp)

    merklePaths :: Hashable a => a -> Tree a -> [MerklePath]
    merklePaths _ (Leaf _ _) = []
    merklePaths el (Twig _ (Leaf h _))
        | hash el == h = [[Right h]]
        | otherwise = []
    merklePaths el (Twig _ t) = 
        let res = merklePaths el t in 
          case res of 
            [] -> []
            xs -> map (Right (treeHash t) : ) xs
    merklePaths el (Node _ (Leaf hl _) (Leaf hr _))
        | hash el == hl && hash el == hr = [[Right hr], [Left hl]]
        | hash el == hl && hash el /= hr = [[Right hr]]
        | hash el /= hl && hash el == hr = [[Left hl]]
        | otherwise = []
    merklePaths el (Node _ l r) =
        let lres = merklePaths el l in
            let rres = merklePaths el r in
                case lres of
                    [] -> case rres of
                        [] -> []
                        rs -> map (Left (treeHash l) : ) rs
                    ls -> case rres of
                        [] -> map (Right (treeHash r) : ) ls
                        rs -> map (Right (treeHash r) : ) ls ++ map (Left (treeHash l) : ) rs

    buildProof :: Hashable a => a -> Tree a -> Maybe (MerkleProof a)
    buildProof el (Leaf h _)
      | hash el == h = Just $ MerkleProof el []
      | otherwise = Nothing
    buildProof el t = let res = merklePaths el t in
        case res of
            [] -> Nothing
            (x:_) -> Just $ MerkleProof el x
    
    verifyProof :: Hashable a => Hash -> MerkleProof a -> Bool
    verifyProof h (MerkleProof el []) = hash el == h
    verifyProof h (MerkleProof el xs) = let res = foldr hash' (hash el) xs in res == h
      where
        hash' :: Either Hash Hash -> Hash -> Hash
        hash' (Left hl) hr = hash (hl, hr)
        hash' (Right hr) hl = hash (hl, hr)
