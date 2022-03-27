module Main where

import HashTree
import Debug.Trace


main :: IO ()
main = print $
  let t = buildTree "bitcoin" in
    let proof = buildProof 'i' t in 
      trace (show proof) verifyProof (treeHash t) <$> proof
 
