{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module HashTree (
    Tree,
    leaf,
    twig,
    node,
    treeHash,
    drawTree,
) where

    import Hashable32 ( Hashable(hash), Hash, showHash )
    
    data Tree a = Leaf Hash a | Twig Hash (Tree a) | Node Hash (Tree a) (Tree a)

    -- A hash of the Tree
    treeHash :: Tree a -> Hash 
    treeHash (Leaf h _) = h
    treeHash (Twig h _) = h
    treeHash (Node h _ _) = h

    -- Creates a Tree without children
    leaf :: Hashable a => a -> Tree a
    leaf x = Leaf (hash x) x
    
    -- Creates a Tree with 1 child
    twig :: Hashable a => Tree a -> Tree a
    twig t = Twig ( hash(treeHash t, treeHash t) ) t

    -- Creates a Tree with two children
    node :: Hashable a => Tree a -> Tree a -> Tree a
    node l r = Node ( hash(treeHash l, treeHash r)) l r

    -- Builds Tree out of a list of elements
    buildTree :: Hashable a => [a] -> Tree a
    buildTree (x:xs) = head $ buildTree' $ map leaf (x:xs)
        where
            buildTree' :: Hashable a => [Tree a] -> [Tree a]
            buildTree' [x] = [x]
            buildTree' xs = buildTree' $ group xs
                where
                    group :: Hashable a => [Tree a] -> [Tree a]
                    group [] = []
                    group [x] = [twig x]
                    group (st:nd:xs) = node st nd : group xs
                
    -- Draws a representation of a Tree
    drawTree :: Show a => Tree a -> String
    drawTree t = drawNestedTree t 0 0
        where 
            drawNestedTree :: Show a => Tree a -> Int -> Int -> String 
            drawNestedTree (Leaf h v) 0 _ = showHash h ++ " " ++ show v ++ "\n"
            drawNestedTree (Twig h t) 0 n = showHash h ++ " +\n" ++ drawNestedTree t (n + 1) (n + 1)
            drawNestedTree (Node h l r) 0 n = showHash h ++ " -\n" ++ drawNestedTree l (n + 1) (n + 1) ++ drawNestedTree r (n + 1) (n + 1)
            drawNestedTree t m n = ' ' : drawNestedTree t (m - 1) n

