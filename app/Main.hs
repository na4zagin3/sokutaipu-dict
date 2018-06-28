module Main where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Stenotype.Sokutype.Prim
import Stenotype.Sokutype.Generate

main :: IO ()
main = do
  let duplicatedStrokes = M.filter (\xs -> length xs > 1) strokeTable
  print $ length strokeTable
  mapM_ putStrLn $ map (\(s, sps) -> showStroke s ++ ": " ++ L.intercalate ", " (map fst $ S.toList sps)) $ M.toList duplicatedStrokes
