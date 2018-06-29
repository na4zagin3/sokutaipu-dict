module Stenotype.Sokutype.Prim where

import Data.Set (Set)
import qualified Data.Set as S

data FingerKey = Y | T | H | K | S | A | I | O
  deriving (Eq, Ord, Show, Read)

data ThumbKey = TT | TK | TI | TN
  deriving (Eq, Ord, Show, Read)

mirrorThumb :: ThumbKey -> ThumbKey
mirrorThumb TT = TN
mirrorThumb TK = TI
mirrorThumb TI = TK
mirrorThumb TN = TT

mirrorThumbs :: Set ThumbKey -> Set ThumbKey
mirrorThumbs = S.map mirrorThumb

data StrokeType = EitherHand (Set FingerKey)
  | OnlyLeft (Set FingerKey)
  | LeftMiddle (Set FingerKey) (Set ThumbKey)
  | OnlyLeftMiddle (Set FingerKey) (Set ThumbKey)
  | Middle (Set ThumbKey)
  | RightMiddle (Set ThumbKey) (Set FingerKey)
  | OnlyRightMiddle (Set ThumbKey) (Set FingerKey)
  | LeftRightMiddle (Set FingerKey) (Set ThumbKey) (Set FingerKey)
  deriving (Eq, Ord, Show, Read)

data StenoWord = Syllables String
  deriving (Eq, Ord, Show, Read)


data Priority = Special | Normal | Permitted
  deriving (Eq, Ord, Read, Show)

data Stroke = Stroke
  { hidariNoMure :: Set FingerKey
  , nakaNoMure :: Set ThumbKey
  , migiNoMure :: Set FingerKey
  }
  deriving (Eq, Ord, Read, Show)

emptyStroke :: Stroke
emptyStroke = Stroke
  { hidariNoMure = S.empty
  , nakaNoMure = S.empty
  , migiNoMure = S.empty
  }

stroke :: (Set FingerKey, Set ThumbKey, Set FingerKey) -> Stroke
stroke (fl, n, fr) = Stroke
  { hidariNoMure = fl
  , nakaNoMure = n
  , migiNoMure = fr
  }

showStroke :: Stroke -> String
showStroke s = concat
  [ concatMap show $ S.toAscList f1
  , "|"
  , concatMap (tail . show) $ S.toAscList n
  , "|"
  , concatMap show $ S.toDescList f2
  ]
  where
    f1 = hidariNoMure s
    n = nakaNoMure s
    f2 = migiNoMure s
