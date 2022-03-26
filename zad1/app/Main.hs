module Main where

import HashTree

main :: IO ()
main = print $
  buildProof 'i' $ buildTree "bitco"
