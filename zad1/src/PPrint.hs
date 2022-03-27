module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = undefined

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS undefined
pprH = intercalateS undefined

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS sep (x:xs) = foldl (.) x (prependToAll sep xs) where  
  prependToAll :: ShowS -> [ShowS] -> [ShowS]
  prependToAll _ [] = []
  prependToAll s ys = map (s.) ys

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith = undefined

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")