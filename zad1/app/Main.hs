module Main where

import HashTree
import Blockchain (mineBlock, Transaction(Tx), txFrom, txTo, txAmount, coin, verifyChain, mineTransactions, block0, block1, tx1, blockHdr, validateReceipt)
import Hashable32 (hash, VisiHash(VH))
import Debug.Trace


main :: IO ()
main = print $
  let charlie = hash "Charlie" in
    let (block, [receipt]) = mineTransactions charlie (hash block1) [tx1] in
       let block1 = mineBlock (hash "Alice") (hash block0) [] in
          validateReceipt receipt (blockHdr block)

