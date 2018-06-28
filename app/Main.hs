module Main where

import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import System.IO

import Stenotype.Sokutype.Prim
import Stenotype.Sokutype.Generate

main :: IO ()
main = do
  let duplicatedStrokes = M.filter (\xs -> length xs > 1) strokeTable
  hPutStrLn stderr $ "Total keystrokes"
  hPutStrLn stderr . show $ length strokeTable
  hPutStrLn stderr $ "Duplicated keystrokes"
  hPutStrLn stderr . show $ length duplicatedStrokes
  mapM_ (hPutStrLn stderr) $ map (\(s, sps) -> showStroke s ++ ": " ++ L.intercalate ", " (map fst $ S.toList sps)) $ M.toList duplicatedStrokes
  let f s | S.null s = Nothing
          | otherwise = Just . fst $ S.elemAt 0 s
  BL8.putStrLn $ J.encode $ M.mapKeys showStroke $ M.mapMaybe f strokeTable
