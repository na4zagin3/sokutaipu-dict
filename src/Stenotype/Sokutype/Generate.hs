module Stenotype.Sokutype.Generate where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Stenotype.Sokutype.Prim
import Stenotype.Sokutype.Stroke


generateStroke1 :: (StenoWord, StrokeType) -> [(String, Stroke, Priority)]
generateStroke1 (Syllables s, EitherHand f) = [(s, stroke (f, S.empty, S.empty), Normal)]
generateStroke1 (Syllables s, OnlyLeft f) = [(s, stroke (f, S.empty, S.empty), Normal)]
generateStroke1 (Syllables s, Middle n) = [(s, stroke (S.empty, n, S.empty), Normal)]
generateStroke1 (Syllables s, LeftMiddle f n) = [(s, stroke (f, n, S.empty), Normal)]
generateStroke1 (Syllables s, OnlyLeftMiddle f n) = [(s, stroke (f, n, S.empty), Normal)]
generateStroke1 (Syllables s, OnlyRightMiddle nr fr) = [(s, stroke (S.empty, nr, fr), Normal)]
generateStroke1 (Syllables s, LeftRightMiddle f n fr) = [(s, stroke (f, n, fr), Normal)]


generateStroke2 :: (StenoWord, StrokeType) -> (StenoWord, StrokeType) -> [(String, Stroke, Priority)]
generateStroke2 _ (Syllables _, OnlyLeft _) = []
generateStroke2 _ (Syllables _, OnlyLeftMiddle _ _) = []
generateStroke2 (Syllables _, Middle _) _ = []
generateStroke2 _ (Syllables _, Middle _) = []
generateStroke2 (Syllables _, OnlyRightMiddle _ _) _ = []
generateStroke2 _ (Syllables _, OnlyRightMiddle _ _) = [] -- これ許されるか不明だが、衝突を防ぐ為に禁止してみる
generateStroke2 (Syllables _, LeftRightMiddle _ _ _) _ = []
generateStroke2 _ (Syllables _, LeftRightMiddle _ _ _) = []
generateStroke2 (s1, OnlyLeft f1) r = generateStroke2 (s1, LeftMiddle f1 S.empty) r -- 衝突を防ぐ為に、中群を禁止してみる
generateStroke2 (Syllables s1, EitherHand f1) (Syllables s2, EitherHand f2)
  | head s2 `elem` "いんくちつき" = strokesWithNaka
  | otherwise = [(s1 ++ s2, stroke (f1, S.empty, f2), Normal)] ++ strokesWithNaka
  where
    strokesWithNaka = do
      (sn, n) <- strokeNaka
      return (s1 ++ sn ++ s2, stroke (f1, n, f2), Normal)
generateStroke2 (Syllables s1, EitherHand f1) (Syllables s2, LeftMiddle f2 n2) =
  [(s1 ++ s2, stroke (f1, mirrorThumbs n2, f2), Permitted)]
-- generateStroke2 (Syllables s1, EitherHand f1) (Syllables s2, OnlyRightMiddle nr2 f2) = [(s1 ++ s2, (f1, nr2, f2), Normal)] -- ?
generateStroke2 (Syllables s1, LeftMiddle f1 n1) (Syllables s2, EitherHand f2) =
  [(s1 ++ s2, stroke (f1, n1, f2), Normal)]
generateStroke2 (Syllables s1, LeftMiddle f1 n1) (Syllables s2, LeftMiddle f2 n2) =
  [(s1 ++ s2, stroke (f1, n1 `S.union` mirrorThumbs n2, f2), Permitted)]
-- generateStroke2 (Syllables s1, LeftMiddle f1 n1) (Syllables s2, OnlyRightMiddle nr2 f2) = [(s1 ++ s2, (f1, n1 `S.union` nr2, f2), Normal)] -- ?
generateStroke2 (Syllables s1, OnlyLeftMiddle f1 n1) (Syllables s2, EitherHand f2) =
  [(s1 ++ s2, stroke (f1, n1, f2), Normal)]
generateStroke2 (Syllables s1, OnlyLeftMiddle f1 n1) (Syllables s2, LeftMiddle f2 n2) =
  [(s1 ++ s2, stroke (f1, n1 `S.union` mirrorThumbs n2, f2), Permitted)]

basicStrokes :: [(String, Stroke, Priority)]
basicStrokes = concatMap generateStroke1 basicStrokeTemplates ++ complexStrokes
  where
    complexStrokes = concat $ do
      t1 <- basicStrokeTemplates
      t2 <- basicStrokeTemplates
      return $ generateStroke2 t1 t2

strokeTable :: M.Map Stroke (Set (String, Priority))
strokeTable = M.fromListWith S.union $ map (\(t, s, p) -> (s, S.singleton (t, p))) basicStrokes
