module Main where

import HashTree
import Blockchain (mineBlock, Transaction(Tx), txFrom, txTo, txAmount, coin, verifyChain, mineTransactions, block0, block1, tx1, blockHdr, validateReceipt, pprBlock, block2)
import Hashable32 (hash, VisiHash(VH))
import PPrint
import Debug.Trace


main :: IO ()

main =
  let charlie = hash "Charlie" in
    let (block, [receipt]) = mineTransactions charlie (hash block1) [tx1] in
          --receipt
          runShows $ pprListWith pprBlock [block0, block1, block2]

