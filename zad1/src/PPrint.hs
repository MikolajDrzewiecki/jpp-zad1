-- Mikołaj Drzewiecki 406134


module PPrint where

  writeln :: String -> IO ()
  writeln = putStrLn

  showsPair :: Show a => (String, a) -> ShowS
  showsPair (k,v) = showString k . showString ": " . shows v

  pprH, pprV :: [ShowS] -> ShowS
  pprV = intercalateS (showChar '\n')
  pprH = intercalateS (showChar ' ')

  intercalateS :: ShowS -> [ShowS] -> ShowS
  intercalateS sep (x:xs) = foldl (.) x (prependToAll sep xs) where
    prependToAll :: ShowS -> [ShowS] -> [ShowS]
    prependToAll _ [] = []
    prependToAll s ys = map (s.) ys

  pprListWith :: (a -> ShowS) -> [a] -> ShowS
  pprListWith fn xs = concat' ( map fn xs) where
    concat' :: [ShowS] -> ShowS
    concat' [] = showString ""
    concat' [y] = y
    concat' (y:ys) = y . showChar '\n' . concat' ys

  runShows :: ShowS -> IO ()
  runShows = putStrLn . ($"")