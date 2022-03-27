module Main where

import HashTree
import Blockchain (mineBlock, Transaction(Tx), txFrom, txTo, txAmount, coin, verifyChain)
import Hashable32 (hash, VisiHash(VH))
import Debug.Trace


main :: IO ()
main = print $
  let tx1 = Tx { txFrom = hash "Alice", txTo = hash "Bob", txAmount = 1 * coin } in
    let block0 = mineBlock (hash "Satoshi") 0 [] in
       let block1 = mineBlock (hash "Alice") (hash block0) [] in
          let block2 = mineBlock (hash "Charlie") (hash block1) [tx1] in
             VH <$> verifyChain [block2,block1,block0]

