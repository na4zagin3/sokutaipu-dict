module Main where

import Control.Monad
import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Lib

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

strokeNaka :: [(String, Set ThumbKey)]
strokeNaka = [ ("つ", S.fromList [TT         ])
             , ("く", S.fromList [   TK      ])
             , ("い", S.fromList [      TI   ])
             , ("ん", S.fromList [         TN])
             , ("ち", S.fromList [TT,   TI   ])
             , ("き", S.fromList [   TK,TI   ])
             , ("い", S.fromList [      TI   ])
             ]


strokeKiso :: [(StenoWord, StrokeType)]
strokeKiso = map (\(w, s) -> (Syllables w, EitherHand $ S.fromList s)) ks
  where
    -- tot. 175
    ks = [ ("あ",     [          A    ])
         , ("い",     [            I  ])
         , ("う",     [            I,O])
         , ("え",     [          A,  O])
         , ("おー",   [          A,I  ])
         , ("あい",   [Y,        A,I  ])
         , ("えい",   [Y,        A,  O])
         , ("か",     [      K,  A    ])
         , ("き",     [      K,    I  ])
         , ("く",     [      K        ])
         , ("くー",   [      K,    I,O])
         , ("け",     [      K,  A,  O])
         , ("こ",     [      K,      O])
         , ("こー",   [      K,  A,I  ])
         , ("かい",   [Y,    K,  A,I  ])
         , ("けい",   [Y,    K,  A,  O])
         , ("きゃ",   [Y,    K,  A    ])
         , ("きゅ",   [Y,    K,    I  ])
         , ("きゅー", [Y,    K,    I,O])
         , ("きょ",   [Y,    K,      O])
         , ("きょー", [Y,    K        ])
         , ("が",     [    H,K,  A    ])
         , ("ぎ",     [    H,K,    I  ])
         , ("ぐ",     [    H,K        ])
         , ("ぐー",   [    H,K,    I,O])
         , ("げ",     [    H,K,  A,  O])
         , ("ご",     [    H,K,      O])
         , ("ごー",   [    H,K,  A,I  ])
         , ("がい",   [Y,  H,K,  A,I  ])
         , ("げい",   [Y,  H,K,  A,  O])
         , ("ぎゃ",   [Y,  H,K,  A    ])
         , ("ぎゅ",   [Y,  H,K,    I  ])
         , ("ぎゅー", [Y,  H,K,    I,O])
         , ("ぎょ",   [Y,  H,K,      O])
         , ("ぎょー", [Y,  H,K        ])
         , ("さ",     [        S,A    ])
         , ("し",     [        S,  I  ])
         , ("す",     [        S      ])
         , ("すー",   [        S,  I,O])
         , ("せ",     [        S,A,  O])
         , ("そ",     [        S,    O])
         , ("そー",   [        S,A,I  ])
         , ("さい",   [Y,      S,A,I  ])
         , ("せい",   [Y,      S,A,  O])
         , ("しゃ",   [Y,      S,A    ])
         , ("しゅ",   [Y,      S,  I  ])
         , ("しゅー", [Y,      S,  I,O])
         , ("しょ",   [Y,      S,    O])
         , ("しょー", [Y,      S      ])
         , ("ざ",     [  T,    S,A    ])
         , ("じ",     [  T,    S,  I  ])
         , ("ず",     [  T,    S      ])
         , ("ずー",   [  T,    S,  I,O])
         , ("ぜ",     [  T,    S,A,  O])
         , ("ぞ",     [  T,    S,    O])
         , ("ぞー",   [  T,    S,A,I  ])
         , ("ざい",   [Y,T,    S,A,I  ])
         , ("ぜい",   [Y,T,    S,A,  O])
         , ("じゃ",   [Y,T,    S,A    ])
         , ("じゅ",   [Y,T,    S,  I  ])
         , ("じゅー", [Y,T,    S,  I,O])
         , ("じょ",   [Y,T,    S,    O])
         , ("じょー", [Y,T,    S      ])
         , ("た",     [  T,      A    ])
         , ("ち",     [  T,        I  ])
         , ("つ",     [  T            ])
         , ("つー",   [  T,        I,O])
         , ("て",     [  T,      A,  O])
         , ("と",     [  T,          O])
         , ("とー",   [  T,      A,I  ])
         , ("たい",   [Y,T,      A,I  ])
         , ("てい",   [Y,T,      A,  O])
         , ("ちゃ",   [Y,T,      A    ])
         , ("ちゅ",   [Y,T,        I  ])
         , ("ちゅー", [Y,T,        I,O])
         , ("ちょ",   [Y,T,          O])
         , ("ちょー", [Y,T            ])
         , ("だ",     [  T,H,K,S,A    ])
         , ("で",     [  T,H,K,S,A,  O])
         , ("ど",     [  T,H,K,S      ]) -- Irregular
         , ("だい",   [Y,T,H,K,S,A,I  ])
         , ("でい",   [Y,T,H,K,S,A,  O])
         , ("な",     [    H,  S,A    ])
         , ("に",     [    H,  S,  I  ])
         , ("ぬ",     [    H,  S      ])
         , ("ぬー",   [    H,  S,  I,O])
         , ("ね",     [    H,  S,A,  O])
         , ("の",     [    H,  S,    O])
         , ("のー",   [    H,  S,A,I  ])
         , ("ない",   [Y,  H,  S,A,I  ])
         , ("ねい",   [Y,  H,  S,A,  O])
         , ("にゃ",   [Y,  H,  S,A    ])
         , ("にゅ",   [Y,  H,  S,  I  ])
         , ("にゅー", [Y,  H,  S,  I,O])
         , ("にょ",   [Y,  H,  S,    O])
         , ("にょー", [Y,  H,  S      ])
         , ("は",     [    H,    A    ])
         , ("ひ",     [    H,      I  ])
         , ("ふ",     [    H          ])
         , ("ふー",   [    H,      I,O])
         , ("へ",     [    H,    A,  O])
         , ("ほ",     [    H,        O])
         , ("ほー",   [    H,    A,I  ])
         , ("はい",   [Y,  H,    A,I  ])
         , ("れい",   [Y,  H,    A,  O])
         , ("ひゃ",   [Y,  H,    A    ])
         , ("ひゅ",   [Y,  H,      I  ])
         , ("ひゅー", [Y,  H,      I,O])
         , ("ひょ",   [Y,  H,        O])
         , ("ひょー", [Y,  H          ])
         , ("ば",     [  T,H,    A    ])
         , ("び",     [  T,  K,S,A,  O]) -- Irregular
         , ("ぶ",     [  T,H          ])
         , ("ぶー",   [  T,H,      I,O]) -- It looks regular.
         , ("べ",     [  T,H,    A,  O])
         , ("ぼ",     [  T,H,        O])
         , ("ぼー",   [  T,H,    A,I  ])
         , ("ばい",   [Y,T,H,    A,I  ])
         , ("べい",   [Y,T,H,    A,  O])
         , ("びゃ",   [Y,T,H,    A    ])
         , ("びゅ",   [Y,T,  K,S,A,  O]) -- Irregular
         , ("びゅー", [Y,T,H,      I,O])
         , ("びょ",   [Y,T,H,        O])
         , ("びょー", [Y,T,H          ])
         , ("ぱ",     [  T,H,K,  A    ])
         , ("ぴ",     [  T,  K,S,    O]) -- Irregular
         , ("ぷ",     [  T,H,K        ])
         , ("ぷー",   [  T,H,K,    I,O])
         , ("ぺ",     [  T,H,K,  A,  O])
         , ("ぽ",     [  T,H,K,      O])
         , ("ぽー",   [  T,H,K,  A,I  ])
         , ("ぱい",   [Y,T,H,K,  A,I  ])
         , ("ぺい",   [Y,T,H,K,  A,  O])
         , ("ぴゃ",   [Y,T,H,K,  A    ])
         , ("ぴゅ",   [Y,T,  K,S,    O]) -- Irregular
         , ("ぴゅー", [Y,T,H,K,    I,O])
         , ("ぴょ",   [Y,T,H,K,      O])
         , ("ぴょー", [Y,T,H,K        ])
         , ("ま",     [      K,S,A    ])
         , ("み",     [  T,  K,S      ]) -- Irregular
         , ("む",     [      K,S      ])
         , ("むー",   [      K,S,  I,O])
         , ("め",     [      K,S,A,  O])
         , ("も",     [      K,S,    O])
         , ("もー",   [      K,S,A,I  ])
         , ("まい",   [Y,    K,S,A,I  ])
         , ("めい",   [Y,    K,S,A,  O])
         , ("みゃ",   [Y,    K,S,A    ])
         , ("みゅ",   [Y,T,  K,S      ]) -- Irregular. Followed http://hoangmoc123.d.dooo.jp/sokutaipu.html.
         , ("みゅー", [Y,    K,S,  I,O])
         , ("みょ",   [Y,    K,S,    O])
         , ("みょー", [Y,    K,S      ])
         , ("や",     [Y,        A    ])
         , ("ゆ",     [Y,          I  ]) -- Irregular
         , ("ゆー",   [Y,          I,O])
         , ("よ",     [Y,            O]) -- Corrected from Y. Followed http://hoangmoc123.d.dooo.jp/sokutaipu.html.
         , ("よー",   [Y              ])
         , ("ら",     [  T,  K,  A    ])
         , ("り",     [  T,  K,    I  ])
         , ("る",     [  T,  K        ])
         , ("るー",   [  T,  K,    I,O])
         , ("れ",     [  T,  K,  A,  O])
         , ("ろ",     [  T,  K,      O])
         , ("ろー",   [  T,  K,  A,I  ])
         , ("らい",   [Y,T,  K,  A,I  ])
         , ("れい",   [Y,T,  K,  A,  O])
         , ("りゃ",   [Y,T,  K,  A    ])
         , ("りゅ",   [Y,T,  K,    I  ]) -- Irregular
         , ("りゅー", [Y,T,  K,    I,O])
         , ("りょ",   [Y,T,  K,      O])
         , ("りょー", [Y,T,  K        ])
         , ("わ",     [  T,  K,S,A    ])
         , ("わい",   [Y,T,  K,S,A,I  ])
         , ("ん",     [          A,I,O])
         , ("っ",     [  T,      A,I,O])
         ]

strokeA :: [(StenoWord, StrokeType)]
strokeA = map (\(w, s) -> (Syllables w, Middle $ S.fromList s)) ks
  where
    -- tot. 11
    ks = [ ("に",     [TT            ])
         , ("が",     [    TK        ])
         , ("で",     [TT, TK        ])
         , ("は",     [        TI    ])
         , ("の",     [            TN])
         , ("も",     [        TI, TN])
         , ("には",   [TT,         TN])
         , ("にも",   [TT,     TI, TN])
         , ("では",   [TT, TK, TI    ])
         , ("での",   [TT, TK,     TN])
         , ("でも",   [TT, TK, TI, TN])
         ]

strokeB1 :: [(StenoWord, StrokeType)]
strokeB1 = map (\(w, s) -> (Syllables w, EitherHand $ S.fromList s)) ks ++ [nano]
  where
    ks = [ ("もし",   [  T,  K,S,A,I  ])
         , ("ども",   [  T,H,K,S,    O])
         , ("わけ",   [Y,T,  K,S,A    ])
         , ("など",   [Y,T,H,K,S,    O])
         , ("だけ",   [Y,T,H,K,S,A    ])
         ]
    -- LeftMiddle なのか、 OnlyLeftMiddle なのか不明
    nano = (Syllables "なの", LeftMiddle (S.fromList [Y,T,H,K,S      ]) (S.fromList [TT]))

-- 後置詞が多い
strokeB2 :: [(StenoWord, StrokeType)]
strokeB2 = map (\(s, w) -> (Syllables w, EitherHand $ S.fromList s)) ks ++ [koto]
  where
    koto = (Syllables "（の）こと", LeftMiddle (S.fromList [  T,  K,S,A,I,O]) (S.fromList [TT]))
    ks = [ ([  T,  K,  A,I,O], "から")
         , ([Y,T,  K,  A,I,O], "より")
         , ([      K,S,A,I,O], "まで")
         , ([  T,H,    A,I,O], "ばかり")
         , ([    H,K,  A,I,O], "かぎり")
         , ([        S,A,I,O], "して")
         , ([Y,      S,A,I,O], "（し）まして")
         , ([Y,T,    S,A,I,O], "として")
         , ([Y,T,  K,S,A,I,O], "のでございまして")
         , ([    H,K,S,A,I,O], "もって") -- HKSAI|| なのか、KSAIOなのか、はたまた HKSAIO なのか不明
         , ([Y,T,H,K,S,A,I,O], "になって")
         , ([    H,    A,I,O], "ほど")
         , ([Y,  H,    A,I,O], "ほか")
         , ([Y,    K,  A,I,O], "くらい")
         , ([Y,T,H,    A,I,O], "のばあい")
         , ([      K,  A,I,O], "かどうか")
         , ([  T,H,K,S,A,I,O], "のではない")
--       , ([Y,T,H,K,S,A,I,O], "より")
         ]

-- 後置詞か？ 格助詞+ハ
strokeB3 :: [(StenoWord, StrokeType)]
strokeB3 = map (\(n, s, w) -> (Syllables w, OnlyRightMiddle (S.fromList n) (S.fromList s))) ks ++ [koto]
  where
    koto = (Syllables "（の）こと", LeftMiddle (S.fromList [  T,  K,S,A,I,O]) (S.fromList [TT]))
    ks = [ ([TI   ], [O,I,A,  K      ], "かわ")
         , ([TI   ], [O,I,A,        Y], "えわ")
         , ([TI   ], [O,I,A,      T,Y], "てわ")
         , ([TI   ], [O,I,A,      T  ], "とわ")
         , ([TI   ], [O,I,A,    H    ], "のわ")
         , ([TI   ], [O,I,A,    H,  Y], "にわ")
         , ([TI   ], [O,I,A,S,K,H,T,Y], "でわ")
         ]

strokeB4 :: [(StenoWord, StrokeType)]
strokeB4 = map (\(s, w) -> (Syllables w, OnlyLeft (S.fromList s))) ks ++ [koto]
  where
    koto = (Syllables "（の）こと", LeftMiddle (S.fromList [  T,  K,S,A,I,O]) (S.fromList [TT]))
    ks = [ ([Y,        A,I,O], "いずれ")
         , ([Y,T,H,K,S      ], "なぜ")
         , ([    H,  S,A,I,O], "のぞむ")
         , ([Y,T,      A,I,O], "たのむ")
         ]

-- 基本音節と衝突する略語
strokeB5 :: [(StenoWord, StrokeType)]
strokeB5 = map (\(s, w) -> (Syllables w, EitherHand (S.fromList s))) ks
  where
    ks = [ ([Y,  H,K,  A    ], "がわ")
         , ([Y,  H,        O], "ごしょうち")
         , ([Y,T,    S,    O], "ごぞんじ")
         , ([Y,    K,    I  ], "いけない")
         , ([Y,    K,  A    ], "かつて")
         , ([Y,  H,  S,A    ], "ならば")
         , ([Y,T,  K,  A    ], "（せ）られる")
         , ([Y,      S,  I  ], "される")
         , ([Y,T,  K,S,    O], "（の）ことを")
         , ([Y,      S,A    ], "（し）ますと")
         , ([Y,  H,  S,    O], "のが")
         , ([Y,  H,  S,  I  ], "のに")
         , ([Y,T,          O], "とする")
         , ([Y,T,      A    ], "ただ")
         ]

strokeC1 :: [(StenoWord, StrokeType)]
strokeC1 = map (\(s, n, w) -> (Syllables w, LeftMiddle (S.fromList s) (S.fromList n))) ks
  where
    ks = [ ([          A    ], [TT,TK], "ある")
         , ([            I  ], [TT,TK], "いる")
         , ([              O], [TT,TK], "おる")
         , ([          A,  O], [TT,TK], "える")
         , ([      K        ], [TT,TK], "くる")
         , ([        S,A,  O], [TT,TK], "せる")
         , ([  T,  K,  A,  O], [TT,TK], "れる")
         , ([      K,S,A,  O], [TT,TK], "める")
         , ([    H,K,  A,  O], [TT,TK], "げる")
         , ([  T,    S,A    ], [TT,TK], "ざる")
         , ([        S      ], [TT,TK], "する")
         , ([    H          ], [TT,TK], "ふる")
         , ([  T,    S      ], [TT,TK], "（ん）ずる")
         , ([      K,  A,  O], [TT,TK], "ける")
         , ([  T,  K,S      ], [TT,TK], "（して）みる")
         , ([Y,            O], [TT,TK], "（に）よる")
         , ([Y,  H,    A,I  ], [TT,TK], "（に）はいる")
         ]

strokeC2 :: [(StenoWord, StrokeType)]
strokeC2 = map (\(s, n, w) -> (Syllables w, LeftMiddle (S.fromList s) (S.fromList n))) ks
  where
    ks = [ ([            I,O], [TT,TK], "（の）うえ")
         , ([Y,        A,I  ], [TT,TK], "（の）あいだ")
         , ([  T            ], [TT,TK], "つぎ")
         , ([      K,      O], [TT,TK], "これ")
         , ([        S,    O], [TT,TK], "それ")
         , ([  T,H,K,S      ], [TT,TK], "どこ")
         , ([        S,A    ], [TT,TK], "され")
         , ([  T,  K,  A    ], [TT,TK], "（せ）られ")
         , ([  T,      A    ], [TT,TK], "（し）たら")
         , ([    H,  S,A    ], [TT,TK], "（に）なら")
         , ([  T,      A,I  ], [TT,TK], "（の）とおり")
         , ([  T,H,K,S,A,I  ], [TT,TK], "どおり")
         , ([  T,      A,  O], [TT,TK], "てき")
         , ([    H,  S,    O], [TT,TK], "ので")
         , ([    H,  S,A,  O], [TT,TK], "（せ）ねば")
         , ([  T,H,    A,  O], [TT,TK], "（す）べく")
         , ([  T,    S,  I  ], [TT,TK], "（ん）じて")
         , ([      K,  A    ], [TT,TK], "かた")
         , ([      K,    I  ], [TT,TK], "きり")
         , ([        S,  I  ], [TT,TK], "しか")
         , ([    H,    A    ], [TT,TK], "はな")
         , ([    H,      I  ], [TT,TK], "ひと")
         , ([      K,S,    O], [TT,TK], "もの")
         , ([  T,    S,A,  O], [TT,TK], "（ん）ぜん")
         , ([  T,    S,A,I,O], [TT,TK], "とも") -- TSAIO?!
         , ([  T,          O], [TT,TK], "ところ")
         , ([    H,        O], [TT,TK], "ほんとう")
         , ([  T,  K,S,A    ], [TT,TK], "わたし")
         , ([  T,    S,    O], [TT,TK], "ぞんずる")
         , ([      K,S,A,I  ], [TT,TK], "もうしあげる")
         , ([  T,H,K,S,A    ], [TT,TK], "だろう")
         , ([  T,H,K,S,A,  O], [TT,TK], "です")
         , ([      K,S,A    ], [TT,TK], "ます")
         , ([    H,K,      O], [TT,TK], "のでございます")
         , ([Y,T,    S,A    ], [TT,TK], "じゃない")
         , ([      K,  A,I,O], [TT,TK], "かもしれ（ない）")
         ]

strokeC3 :: [(StenoWord, StrokeType)]
strokeC3 = map (\(s, n, w) -> (Syllables w, LeftMiddle (S.fromList s) (S.fromList n))) ks
  where
    ks = [ ([Y,        A    ], [TT,TK], "（ので）あった")
         , ([Y,          I  ], [TT,TK], "（して）いった")
         , ([Y,    K,  A    ], [TT,TK], "かった")
         , ([Y,  H,  S,A    ], [TT,TK], "（に）なった")
         , ([Y,    K,    I  ], [TT,TK], "（して）きた")
         , ([Y,      S,  I  ], [TT,TK], "した")
         , ([Y,T,  K,S      ], [TT,TK], "（して）みた")
         , ([Y,T,    S,  I  ], [TT,TK], "（ん）じた")
         , ([Y,T,      A    ], [TT,TK], "（の）ため")
         , ([Y,    K,  A,  O], [TT,TK], "けれ（ば）")
         , ([Y,T,H,    A,  O], [TT,TK], "（す）べき")
         , ([Y,  H,    A    ], [TT,TK], "はや")

         , ([Y,T,  K,  A,  O], [TT,TK], "れた")
         , ([Y,  H,K,  A,  O], [TT,TK], "げた")
         , ([Y,T,H,K,S,A,  O], [TT,TK], "でした")
         , ([Y,    K,S,A    ], [TT,TK], "（し）ました")
         , ([Y,  H,K,      O], [TT,TK], "（ので）ございました")
         , ([Y,    K,      O], [TT,TK], "ここ")
         , ([Y,      S,    O], [TT,TK], "そこ")
         , ([Y,      S,A    ], [TT,TK], "させ")
         , ([Y,    K,S,    O], [TT,TK], "もと")
         , ([Y,  H,  S,    O], [TT,TK], "のみ")
         , ([Y,T,          O], [TT,TK], "とり")
         ]

strokeC4 :: [(StenoWord, StrokeType)]
strokeC4 = map (\(s, n, w) -> (Syllables w, LeftMiddle (S.fromList s) (S.fromList n))) ks
  where
    ks = [ ([  T,H,    A    ], [TT,TK], "えば")
         , ([  T,  K,S,A,  O], [TT,TK], "たび")
         , ([    H,K,  A    ], [TT,TK], "（し）ながら")
         , ([    H,K,    I  ], [TT,TK], "（し）すぎる")
         , ([  T,H          ], [TT,TK], "じぶん")
         , ([  T,  K,      O], [TT,TK], "ころ")
         , ([  T,  K,    I  ], [TT,TK], "（ので）あり")
         , ([  T,  K,  A,I  ], [TT,TK], "（で）あろう")
         , ([Y,      S      ], [TT,TK], "でしょう")
         , ([Y,T,  K,  A,I  ], [TT,TK], "いらい")
         , ([Y,  H,K,  A,I  ], [TT,TK], "いがい")

         , ([Y,T,    S      ], [TT,TK], "いじょう")
         , ([Y,          I,O], [TT,TK], "という")
         , ([Y,      S,A,I  ], [TT,TK], "なさる")
         , ([    H,K,  A,I  ], [TT,TK], "うかがう")
         , ([Y,T,H,    A    ], [TT,TK], "せば")
         , ([Y,T,        I  ], [TT,TK], "たち")
         , ([Y,T,      A,  O], [TT,TK], "れて")
         , ([Y,T,  K,    I  ], [TT,TK], "（ので）ありまし")
         , ([Y,T,  K,      O], [TT,TK], "よろこぶ")
         , ([Y,  H,K,  A    ], [TT,TK], "かんがえる")
         ]

strokeC5 :: [(StenoWord, StrokeType)]
strokeC5 = map (\(s, n, w) -> (Syllables w, LeftMiddle (S.fromList s) (S.fromList n))) ks
  where
    ks = [ ([Y,    K        ], [TT,TK], "かろう")
         , ([Y,T            ], [TT,TK], "（し）たろう")
         , ([Y,    K,S      ], [TT,TK], "（し）ましょう")
         , ([Y,T,  K        ], [TT,TK], "（で）ありましょう")
         , ([Y,  H,K        ], [TT,TK], "（で）ございましょう")
         , ([  T,        I  ], [TT,TK], "（し）たり")
         , ([    H,  S      ], [TT,TK], "（に）なり")
         , ([  T,        I,O], [TT,TK], "（し）たる")
         , ([    H,  S,  I  ], [TT,TK], "（に）なる") -- TODO 資料では HS|TK なのであるが、HSO|I としてみた
         , ([    H,  S,  I,O], [TT,TK], "（し）なく")
         , ([        S,  I,O], [TT,TK], "しく")

         , ([    H,      I,O], [TT,TK], "はず")
         , ([      K,    I,O], [TT,TK], "こたえる")
         , ([          A,I  ], [TT,TK], "（と）おもう")
         , ([    H,K,    I,O], [TT,TK], "ねがう")
         , ([Y,    K,S,A,  O], [TT,TK], "まえ")
         , ([Y,      S,A,  O], [TT,TK], "（せ）しめ")
         , ([Y,T,  K,  A    ], [TT,TK], "（す）れば")
         , ([Y,  H,  S,A,  O], [TT,TK], "（し）なければ")
         , ([Y,  H,      I,O], [TT,TK], "ひらく")
         , ([Y,    K,    I,O], [TT,TK], "（して）ください")
         , ([Y,T,    S,A,  O], [TT,TK], "（せ）ざるを（えない）")
         ]

strokeC6 :: [(StenoWord, StrokeType)]
strokeC6 = map (\(s, n, w) -> (Syllables w, LeftMiddle (S.fromList s) (S.fromList n))) ks
  where
    ks = [ ([        S,A,I  ], [TT,TK], "さん")
         , ([  T,    S,A,I  ], [TT,TK], "ざん")
         , ([    H,    A,I  ], [TT,TK], "はん")
         , ([  T,H,    A,I  ], [TT,TK], "ばん")
         , ([  T,H,K,  A,I  ], [TT,TK], "（ん）ぱん")
         , ([Y,    K,  A,I  ], [TT,TK], "いか")
         , ([Y,T,      A,I  ], [TT,TK], "（して）いた")
         , ([Y,    K,S,A,I  ], [TT,TK], "いま")
         , ([  T,  K,    I,O], [TT,TK], "（し）うる")
         , ([Y,T,        I,O], [TT,TK], "（の）うち")
         , ([      K,  A,I  ], [TT,TK], "おこなう")
         , ([          A,I,O], [TT,TK], "（のでは）ありません")
         , ([      K,S      ], [TT,TK], "（し）ません")
         , ([    H,K        ], [TT,TK], "（のでは）ございません")
         , ([  T,  K        ], [TT,TK], "（して）もらう")
         , ([Y,T,  K,    I,O], [TT,TK], "（して）やる")

         , ([Y,  H,  S,  I,O], [TT,TK], "なに")
         , ([Y,      S,  I,O], [TT,TK], "しい")
         , ([  T,      A,I,O], [TT,TK], "（の）とき")
         , ([Y,  H,  S,A,I  ], [TT,TK], "（の）なか")
         , ([Y,  H,  S,  I  ], [TT,TK], "なかった")
         , ([    H,  S,A,I  ], [TT,TK], "（しなければ）ならない")
         , ([  T,H,        O], [TT,TK], "のべる")
         , ([Y,T,  K,S,A,I  ], [TT,TK], "わかる")
         , ([Y,T,H,K,S      ], [TT,TK], "（ことが）できる")
         , ([Y,T,H,K,S,A,I  ], [TT,TK], "（ことが）できない")
         , ([Y,T,    S,A,I  ], [TT,TK], "（して）いらっしゃる")
         , ([Y,T,    S,    O], [TT,TK], "おっしゃる")
         , ([Y,T,    S,  I,O], [TT,TK], "（して）しまう")
         , ([Y,  H,K,    I  ], [TT,TK], "きめる")
         , ([Y,  H,K,    I,O], [TT,TK], "こまる")
         ]

strokeC7 :: [(StenoWord, StrokeType)]
strokeC7 = map (\(s, n, w) -> (Syllables w, LeftMiddle (S.fromList s) (S.fromList n))) ks
  where
    ks = [ ([    H,    A,  O], [TT,TK], "さえ")
         , ([Y,  H,        O], [TT,TK], "あと")
         , ([Y,T,H,        O], [TT,TK], "あなた")
         , ([Y,        A,  O], [TT,TK], "（ので）あります")
         , ([Y,  H,    A,  O], [TT,TK], "（ので）ありまして")
         , ([Y,  H          ], [TT,TK], "ような")
         , ([Y,T,H          ], [TT,TK], "てきな")

         , ([Y,T,  K,S,A,  O], [TT,TK], "てきに")
         , ([Y              ], [TT,TK], "いたす")
         , ([Y,T,H,    A,I  ], [TT,TK], "（して）いただく")
         , ([  T,H,K        ], [TT,TK], "（ので）あって")
         , ([Y,T,H,K        ], [TT,TK], "（して）いって")
         ]

strokeC8 :: [(StenoWord, StrokeType)]
strokeC8 = map (\(s, n, w) -> (Syllables w, OnlyLeftMiddle (S.fromList s) (S.fromList n))) ks
  where
    ks = [ ([Y,T,    S,A,I,O], [TT,TK], "としている")
         , ([Y,    K,S,A,I,O], [TT,TK], "もっている")
         , ([Y,T,H,K,S,A,I,O], [TT,TK], "（に）なっている")
         , ([Y,T,H,K,S,    O], [TT,TK], "だれ")
         , ([  T,H,K,S,    O], [TT,TK], "どれ")
         , ([    H,K,  A,I,O], [TT,TK], "かぎる")

         , ([  T,  K,S,A,I  ], [TT,TK], "もうす")
         , ([Y,T,  K,S,A    ], [TT,TK], "わたす")
         , ([Y,  H,    A,I,O], [TT,TK], "はじめる")
         , ([  T,  K,S,A,I,O], [TT,TK], "ことわる")
         , ([Y,T,      A,I,O], [TT,TK], "たずねる")
         , ([Y,        A,I,O], [TT,TK], "やめる")
         ]

strokeD1 :: [(StenoWord, StrokeType)]
strokeD1 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([            I  ], [           ], [      S,K,H,T,Y], "いろいろな")
         , ([  T,      A    ], [           ], [      S,K,H,T,Y], "たしかな")
         , ([      K,  A    ], [           ], [      S,K,H,T,Y], "かんたんな")
         , ([    H,      I  ], [           ], [      S,K,H,T,Y], "ひじょうな")
         , ([    H,K        ], [           ], [      S,K,H,T,Y], "ぐあいな")

         , ([      K,  A,I  ], [           ], [      S,K,H,T,Y], "こういうふうな")
         , ([        S,A,I  ], [           ], [      S,K,H,T,Y], "そういうふうな")
         , ([  T,H,K,S,A,I  ], [           ], [      S,K,H,T,Y], "どういうふうな")
         , ([Y,          I,O], [TT,TK      ], [      S,K,H,T,Y], "というふうな")
         ]

strokeD2 :: [(StenoWord, StrokeType)]
strokeD2 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([        S,A,  O], [      TI,TN], [O,  A,      T  ], "せっきょくてき")
         , ([Y,      S      ], [      TI,TN], [O,  A,      T  ], "しょうきょうてき")
         , ([    H,K        ], [      TI,TN], [O,  A,      T  ], "ぐたいてき")
         , ([Y,T,        I,O], [      TI,TN], [O,  A,      T  ], "ちゅうしょうてき")
         , ([    H,      I  ], [      TI,TN], [O,  A,      T  ], "ひかくてき")
         , ([    H,    A    ], [           ], [O,I,    K,  T,Y], "はたらく")
         ]

strokeD3 :: [(StenoWord, StrokeType)]
strokeD3 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([          A    ], [           ], [O,I,    K,  T,Y], "あらわれる")
         , ([  T,H,K,S,A,  O], [           ], [O,I,    K,  T,Y], "できあがる")
         , ([    H,  S,    O], [           ], [O,I,    K,  T,Y], "のこる")
         , ([  T,        I  ], [           ], [O,I,    K,  T,Y], "ちがう")
         , ([              O], [   TK      ], [O,I,    K,  T,Y], "おこる")
         , ([              O], [           ], [O,I,    K,  T,Y], "おわる")
         , ([    H,      I  ], [           ], [O,I,    K,  T,Y], "ひきつづく")
         , ([      K,  A    ], [           ], [O,I,    K,  T,Y], "かわる")
         , ([      K,    I  ], [           ], [O,I,    K,  T,Y], "きまる")
         , ([Y,            O], [           ], [O,I,    K,  T,Y], "およぶ")
         , ([        S,A    ], [TT         ], [O,I,    K,  T,Y], "さしつかえる")
         , ([  T            ], [           ], [O,I,    K,  T,Y], "つとめる")
         , ([            I,O], [TT         ], [O,I,    K,  T,Y], "うったえる")
         , ([      K        ], [TT,TK      ], [O,I,    K,  T,Y], "くらべる")
         , ([      K,S,A    ], [TT         ], [O,I,    K,  T,Y], "まちがう")
         , ([Y,        A    ], [           ], [O,I,    K,  T,Y], "あやまる")
         , ([      K        ], [           ], [O,I,    K,  T,Y], "くわえる")
         , ([  T,  K,S      ], [           ], [O,I,    K,  T,Y], "みとめる")
         , ([      K,S,    O], [           ], [O,I,    K,  T,Y], "もちいる")
         , ([    H,  S      ], [           ], [O,I,    K,  T,Y], "ぬすむ")
         , ([        S,  I  ], [           ], [O,I,    K,  T,Y], "しらべる")
         , ([  T,          O], [           ], [O,I,    K,  T,Y], "とりしらべる")
         , ([Y,T,          O], [           ], [O,I,    K,  T,Y], "とりしまる")
         , ([  T            ], [   TK      ], [O,I,    K,  T,Y], "つくる")
         , ([Y,T,          O], [TT,TK      ], [O,I,    K,  T,Y], "とりあつかう")
         , ([            I,O], [           ], [O,I,    K,  T,Y], "うけとる")
         , ([            I,O], [   TK      ], [O,I,    K,  T,Y], "うけたまわる")
         , ([  T,  K,S,A    ], [           ], [O,I,    K,  T,Y], "わすれる")
         , ([  T,H,        O], [           ], [O,I,    K,  T,Y], "おぼえる")
         ]

strokeD4 :: [(StenoWord, StrokeType)]
strokeD4 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([          A    ], [           ], [  I,    K,  T,Y], "あまり")
         , ([          A    ], [         TN], [  I,    K,  T,Y], "あんまり")
         , ([Y,        A    ], [           ], [  I,    K,  T,Y], "やはり")
         , ([Y,        A    ], [TT,      TN], [  I,    K,  T,Y], "やっぱり")
         , ([  T            ], [           ], [  I,    K,  T,Y], "つまり")
         , ([Y,T            ], [           ], [  I,    K,  T,Y], "つもり")
         ]

strokeD5 :: [(StenoWord, StrokeType)]
strokeD5 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([Y,  H,  S,  I,O], [TT,TK,   TN], [O,I,  S        ], "なんとか")
         , ([              O], [         TN], [O,I,  S        ], "おだやか")
         , ([          A    ], [         TN], [O,I,  S        ], "あきらか")
         ]

-- TODO The document says it is LeftMiddle type.
strokeD6 :: [(StenoWord, StrokeType)]
strokeD6 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([          A    ], [      TI   ], [               ], "あるいは")
         , ([      K,S,A    ], [      TI   ], [               ], "または")
         , ([  T,  K,S,A    ], [      TI   ], [               ], "いわば")
         , ([  T,      A    ], [      TI   ], [               ], "たとえば")
         ]

-- TODO The document says it is LeftMiddle type.
strokeD7 :: [(StenoWord, StrokeType)]
strokeD7 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([      K,  A,  O], [      TI,TN], [               ], "けれども")
         , ([      K,S,    O], [      TI,TN], [               ], "もっとも")
         ]

strokeD8 :: [(StenoWord, StrokeType)]
strokeD8 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([          A    ], [           ], [        K,  T  ], "あらゆる")
         , ([            I  ], [           ], [        K,  T  ], "いわゆる")
         , ([        S,  I  ], [           ], [O,  A,      T  ], "(に)したがって")
         , ([Y,      S,  I  ], [           ], [O,  A,      T  ], "(に)したがいまして")
         ]

strokeD9 :: [(StenoWord, StrokeType)]
strokeD9 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([Y,    K,  A    ], [           ], [      S,    T  ], "かならず")
         , ([Y,    K,  A    ], [           ], [O,    S,K      ], "かならずしも")
         ]

strokeE1 :: [(StenoWord, StrokeType)]
strokeE1 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([          A    ], [           ], [  I            ], "あたらしい")
         , ([    H,  S,A    ], [           ], [  I            ], "ながい")
         , ([  T,  K,S      ], [           ], [  I            ], "みじかい")
         , ([Y,        A    ], [           ], [  I            ], "やすい")
         , ([      K,S      ], [           ], [  I            ], "むずかしい")
         , ([Y,            O], [           ], [  I            ], "よろしい")
         , ([  T,  K,S,A    ], [           ], [  I            ], "わるい")
         , ([      K,      O], [           ], [  I            ], "こまかい")
         , ([      K        ], [           ], [  I            ], "くわしい")
         , ([  T,        I  ], [           ], [  I            ], "ちいさい")
         , ([        S      ], [           ], [  I            ], "すくない")
         , ([            I  ], [           ], [  I            ], "いちじるしい")
         , ([    H,      I  ], [           ], [  I            ], "ひどい")
         , ([  T            ], [           ], [  I            ], "つよい")
         , ([            I,O], [           ], [  I            ], "うつくしい")
         , ([  T,  K,  A    ], [           ], [  I            ], "らしい")
         , ([              O], [           ], [  I            ], "おなじ")
         , ([Y,          I,O], [           ], [  I            ], "いうまでもない")
         , ([  T,          O], [           ], [  I            ], "とんでも(ない)")
         ]

strokeE2 :: [(StenoWord, StrokeType)]
strokeE2 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([      K,  A,  O], [           ], [        K      ], "けっきょく")
         , ([      K,S,A    ], [           ], [        K      ], "まったく")
         , ([        S,A    ], [           ], [        K      ], "さっそく")
         , ([        S,A,  O], [           ], [        K      ], "せっかく")
         , ([        S,  I  ], [           ], [        K      ], "しばらく")
         , ([  T,          O], [           ], [        K      ], "とにかく")
         , ([    H,    A,I  ], [           ], [        K      ], "ほうこく")
         , ([    H,K,  A,  O], [           ], [        K      ], "げんそく")
         , ([Y,    K,  A,I  ], [           ], [        K      ], "かいしゃく")
         , ([Y,    K,  A,  O], [           ], [        K      ], "けいやく")
         , ([Y,    K        ], [           ], [        K      ], "きょうりょく")
         , ([Y,    K,S,A,  O], [           ], [        K      ], "めいわく")
         ]

strokeE3 :: [(StenoWord, StrokeType)]
strokeE3 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([            I  ], [           ], [  I,    K      ], "いしき")
         , ([    H,  S,  I  ], [           ], [  I,    K      ], "にんしき")
         , ([        S,    O], [           ], [  I,    K      ], "そうしき")
         , ([  T,        I  ], [           ], [  I,    K      ], "ちしき")
         , ([Y,T,    S      ], [           ], [  I,    K      ], "じょうしき")
         , ([      K,  A    ], [           ], [  I,    K      ], "かぶしき")
         , ([Y,      S,  I  ], [           ], [  I,    K      ], "しゅっせき")
         , ([  T,      A,  O], [           ], [  I,    K      ], "てつづき")
         , ([Y,T,          O], [           ], [  I,    K      ], "とりひき")
         , ([Y,      S,A,  O], [           ], [  I,    K      ], "せいしき")
         , ([Y,    K,  A,  O], [           ], [  I,    K      ], "けいしき")
         , ([  T,  K,    I  ], [           ], [  I,    K      ], "りえき")
         , ([            I,O], [           ], [  I,    K      ], "うらがき")
         , ([  T,  K,S,A    ], [           ], [  I,    K      ], "わりびき")
         ]

strokeE4 :: [(StenoWord, StrokeType)]
strokeE4 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([    H,    A    ], [           ], [            T  ], "ぎじゅつ")
         , ([Y,    K,  A,I  ], [           ], [            T  ], "かいけつ")
         , ([      K        ], [           ], [            T  ], "くべつ")
         , ([Y,      S,A,  O], [           ], [            T  ], "せいりつ")
         , ([Y,T,      A,  O], [           ], [            T  ], "ていしょく")
         , ([  T,    S,  I  ], [           ], [            T  ], "じじつ")
         , ([    H          ], [           ], [            T  ], "ふくざつ")
         , ([  T,          O], [           ], [            T  ], "とくべつ")
         , ([Y,T,          O], [           ], [            T  ], "ちょくせつ")
         , ([    H,K,  A,  O], [           ], [            T  ], "げんじつ")
         , ([      K,    I  ], [           ], [            T  ], "きじつ")
         , ([Y,    K        ], [           ], [            T  ], "きょうじゅつ")
         ]

strokeE5 :: [(StenoWord, StrokeType)]
strokeE5 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([  T,H,K,S,A,I  ], [           ], [            T  ], "どうじに")
         , ([    H,  S,A    ], [           ], [            T  ], "ならびに")
         , ([Y,    K,  A    ], [           ], [            T  ], "かりに")
         , ([        S,A    ], [           ], [            T  ], "さらに")
         , ([        S      ], [           ], [            T  ], "すでに")
         , ([Y              ], [           ], [            T  ], "ようするに")
         ]

strokeE6 :: [(StenoWord, StrokeType)]
strokeE6 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([        S      ], [           ], [  I,        T  ], "すなわち")
         , ([      K,    I  ], [           ], [  I,        T  ], "きもち")
         , ([    H,      I  ], [           ], [  I,        T  ], "ひにち")
         ]

strokeE7 :: [(StenoWord, StrokeType)]
strokeE7 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([      K,S,    O], [           ], [O,I,A          ], "もちろん")
         , ([      K,S      ], [           ], [O,I,A          ], "むろん")
         , ([  T,    S,  I  ], [           ], [O,I,A          ], "じんもん")
         , ([        S,A,  O], [   TK      ], [O,I,A          ], "げんいん")
         , ([  T,      A    ], [           ], [O,I,A          ], "たぶん")
         , ([  T,    S      ], [           ], [O,I,A          ], "ずいぶん")
         , ([Y,    K,      O], [           ], [O,I,A          ], "きょねん")
         , ([          A,I  ], [           ], [O,I,A          ], "さくねん")
         , ([    H,K,      O], [           ], [O,I,A          ], "ごらん")
         , ([    H,    A    ], [           ], [O,I,A          ], "はんだん")
         , ([    H          ], [           ], [O,I,A          ], "ふたん")
         , ([Y,    K,  A,  O], [           ], [O,I,A          ], "けいけん")
         , ([      K,  A,  O], [TT         ], [O,I,A          ], "けつろん")
         , ([Y,      S,    O], [           ], [O,I,A          ], "しょぶん")
         , ([      K,    I  ], [           ], [O,I,A          ], "きかん")
         , ([        S,A,  O], [   TK      ], [O,I,A          ], "せきにん")
         , ([    H,  S,  I  ], [           ], [O,I,A          ], "にんげん")
         , ([        S,  I  ], [           ], [O,I,A          ], "しんぶん")
         , ([            I  ], [           ], [O,I,A          ], "いっぱん")
         , ([Y,T,      A,I  ], [           ], [O,I,A          ], "たいへん")
         , ([Y,T,    S,  I,O], [           ], [O,I,A          ], "じゅうぶん")
         , ([        S,A,  O], [           ], [O,I,A          ], "せんもん")
         , ([      K,      O], [           ], [O,I,A          ], "こんぽん")
         , ([  T,  K,    I  ], [           ], [O,I,A          ], "りろん")
         ]

-- TODO The document says f1 if RightMiddle Pattern.
strokeF1 :: [(StenoWord, StrokeType)]
strokeF1 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([               ], [           ], [  I,A,        Y], "あいかわらず")
         , ([               ], [           ], [    A          ], "あらかじめ")
         , ([               ], [           ], [  I,A,S,K,H,T,Y], "だいたい")
         , ([               ], [           ], [    A,    H    ], "はたして")
         , ([               ], [           ], [O,        H    ], "ほとんど")
         , ([               ], [           ], [          H    ], "ふたたび")
         , ([               ], [           ], [  I            ], "いちばん")
         , ([               ], [           ], [    A,  K,    Y], "かえって")
         , ([               ], [           ], [O,  A,  K      ], "けっして")
         , ([               ], [           ], [  I,    K      ], "きわめて")
         , ([               ], [           ], [O,      K      ], "このまま")
         , ([               ], [           ], [    A,S,K      ], "まるで")
         , ([               ], [           ], [      S,K      ], "むしろ")
         , ([               ], [           ], [O,    S,  H    ], "のちほど")
         , ([               ], [           ], [  I,A,S        ], "おそらく")
         , ([               ], [           ], [    A,S        ], "さきほど")
         , ([               ], [           ], [O,    S        ], "そのまま")
         , ([               ], [           ], [      S        ], "すべて")
         , ([               ], [           ], [    A,S,K,  T  ], "わりあい")
         , ([               ], [           ], [  I,  S,    T  ], "じしん")
         ]

-- TODO The document says f2 if RightMiddle Pattern.
strokeF2 :: [(StenoWord, StrokeType)]
strokeF2 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([               ], [TT         ], [    A          ], "にあたって")
         , ([               ], [TT         ], [    A,    H    ], "にはんして")
         , ([               ], [TT         ], [    A,  K      ], "にかんして")
         , ([               ], [TT         ], [O,    S,K      ], "にもとづいて")
         , ([               ], [TT         ], [O              ], "において")
         , ([               ], [TT         ], [    A,      T  ], "にたいして")
         , ([               ], [TT         ], [O,          T  ], "にとって")
         , ([               ], [TT         ], [            T  ], "について")
         , ([               ], [TT         ], [    A,S,K,  T  ], "にわたって")
         , ([               ], [TT         ], [              Y], "によって")
         , ([               ], [TT         ], [    A,        Y], "にあたりまして")
         , ([               ], [TT         ], [    A,    H,  Y], "にはんしまして")
         , ([               ], [TT         ], [    A,  K,    Y], "にかんしまして")
         , ([               ], [TT         ], [O,    S,K,    Y], "にもとづきまして")
         , ([               ], [TT         ], [O,            Y], "におきまして")
         , ([               ], [TT         ], [    A,      T,Y], "にたいしまして")
         , ([               ], [TT         ], [O,          T,Y], "にとりまして")
         , ([               ], [TT         ], [            T,Y], "につきまして")
         , ([               ], [TT         ], [    A,S,K,  T,Y], "にわたりまして")
         , ([               ], [TT         ], [  I,A,    H    ], "にはんする")
         , ([               ], [TT         ], [  I,A,  K      ], "にかんする")
         , ([               ], [TT         ], [  I,A,S,K      ], "にもとづく")
         , ([               ], [TT         ], [  I,A          ], "における")
         , ([               ], [TT         ], [  I,A,      T  ], "にたいする")
         , ([               ], [TT         ], [  I,A,S,K,  T  ], "にわたる")
         , ([               ], [TT         ], [        K      ], "にかかわらず")
         , ([               ], [TT,   TI,TN], [        K      ], "にもかわらず")
         , ([               ], [TT         ], [        K,    Y], "にかかわりませず")
         , ([               ], [TT,   TI,TN], [        K,    Y], "にもかかわりませず")
         ]

-- TODO The document says f3 if RightMiddle Pattern.
strokeF3 :: [(StenoWord, StrokeType)]
strokeF3 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([               ], [TT,TK      ], [  I,A,        Y], "あいさつ")
         , ([               ], [TT,TK      ], [    A,    H    ], "はなし")
         , ([               ], [TT,TK      ], [    A,    H,  Y], "おはなし")
         , ([               ], [TT,TK      ], [  I            ], "いけん")
         , ([               ], [TT,TK      ], [  I,          Y], "ごいけん")
         , ([               ], [TT,TK      ], [    A,  K      ], "かんけい")
         , ([               ], [TT,TK      ], [    A,  K,    Y], "ごかんけい")
         , ([               ], [TT,TK      ], [    A,S,  H    ], "なまえ")
         , ([               ], [TT,TK      ], [    A,S,  H,  Y], "おなまえ")
         , ([               ], [TT,TK      ], [  I,  S        ], "しつもん")
         , ([               ], [TT,TK      ], [O,    S        ], "そのた")
         , ([               ], [TT,TK      ], [  I,A,    H,T  ], "ぼうえき")
         , ([               ], [TT,TK      ], [      S,K,H,T  ], "どりょく")
         , ([               ], [TT,TK      ], [O,  A,  K,  T  ], "れんらく")
         , ([               ], [TT,TK      ], [O,  A,S        ], "せいかつ")
         , ([               ], [TT,TK      ], [          H,T  ], "ぶぶん")
         , ([               ], [TT,TK      ], [          H,  Y], "ひょうじゅん")
         , ([               ], [TT,TK      ], [  I,A,  K,    Y], "かいしゃ")
         , ([               ], [TT,TK      ], [  I,A,  K,H,  Y], "かぶしきがいしゃ")
         , ([               ], [TT,TK      ], [        K      ], "くみあい")
         , ([               ], [TT,TK      ], [  I,  S,      Y], "ごしつもん")
         , ([               ], [TT,TK      ], [  I,A,S        ], "そうだん")
         , ([               ], [TT,TK      ], [  I,A,S,      Y], "ごそうだん")
         , ([               ], [TT,TK      ], [      S,K,  T  ], "みなさん")
         , ([               ], [TT,TK      ], [O,I,A,S        ], "さいきん")
         , ([               ], [TT,TK      ], [O,  A,  K,H    ], "げんざい")
         , ([               ], [TT,TK      ], [  I,A,S,K,    Y], "まいにち")
         , ([               ], [TT,TK      ], [      S        ], "すこし")
         , ([               ], [TT,TK      ], [O,    S,K      ], "もんだい")
         , ([               ], [TT,TK      ], [    A,S,K,  T  ], "われわれ")
         , ([               ], [TT,TK      ], [O,  A,S,    T,Y], "ぜいきん")
         , ([               ], [TT,TK      ], [  I,A,S,    T,Y], "ざいさん")
         , ([               ], [TT,TK      ], [O,  A,S,    T  ], "ぜんぶ")
         , ([               ], [TT,TK      ], [  I,  S,    T  ], "じかん")
         , ([               ], [TT,TK      ], [      S,    T,Y], "じょうけん")
         , ([               ], [TT,TK      ], [    A,        Y], "やくそく")
         ]

-- TODO The document says f4 if RightMiddle Pattern.
strokeF4 :: [(StenoWord, StrokeType)]
strokeF4 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([               ], [   TK      ], [  I,  S,  H    ], "にほん")
         , ([               ], [   TK      ], [  I,A,    H    ], "ほうりつ")
         , ([               ], [   TK      ], [  I,    K      ], "きそく")
         , ([               ], [   TK      ], [    A,S        ], "さいばん")
         , ([               ], [   TK      ], [O,I,A,S        ], "さいばんかん")
         , ([               ], [   TK      ], [  I,A,S,      Y], "さいばんしょ")
         , ([               ], [   TK      ], [O,I,A,S,      Y], "さいばんちょう")
         , ([               ], [   TK      ], [O,  A,  K,    Y], "けいさつ")
         , ([               ], [   TK      ], [O,I,A,  K,    Y], "けいさつかん")
         , ([               ], [   TK      ], [O,  A,  K      ], "けんさつ")
         , ([               ], [   TK      ], [O,I,A,  K      ], "けんさつかん")
         , ([               ], [   TK      ], [  I,      H    ], "ひこく")
         , ([               ], [   TK      ], [O,I,A,    H    ], "ひこくにん")
         , ([               ], [   TK      ], [O,I,A,    H,T  ], "べんごにん")
         , ([               ], [   TK      ], [O,  A,  K,H    ], "げんこく")
         , ([               ], [   TK      ], [  I,  S,    T  ], "じけん")
         , ([               ], [   TK      ], [      S,      Y], "しょうにん")
         , ([               ], [   TK      ], [O,I,A,      T  ], "とうじしゃ")
         , ([               ], [   TK      ], [  I,A,S,K      ], "もうしたて")
         , ([               ], [   TK      ], [O,        H    ], "ほんにん")
         , ([               ], [   TK      ], [O,I,A,S,K,H,T,Y], "だいりにん")
         , ([               ], [   TK      ], [    A,      T  ], "たてもの")
         , ([               ], [   TK      ], [O,    S,      Y], "しょるい")
         , ([               ], [   TK      ], [          H    ], "ふわたり")
         ]

-- TODO I|TN|IAKY does not belong to g1*
strokeG1S :: [(StenoWord, StrokeType)]
strokeG1S = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([Y,    K,  A,I  ], [      TI,TN], [O,  A,  K,  T  ], "かいいれる")
         , ([Y,    K,  A,I  ], [      TI,TN], [O,  A,      T,Y], "かいいれて")
         , ([Y,    K,  A,I  ], [      TI,TN], [O,  A,  K,  T,Y], "かいいれた")
         , ([Y,T,          O], [TT,TK,TI,TN], [O,  A,  K,  T  ], "とりいれる")
         , ([Y,T,          O], [TT,TK,TI,TN], [O,  A,      T,Y], "とりいれて")
         , ([Y,T,          O], [TT,TK,TI,TN], [O,  A,  K,  T,Y], "とりいれた")
         , ([            I  ], [         TN], [  I,A,  K,    Y], "いいんかい")
         ]

-- Conflict with kihon
strokeG1L :: [(StenoWord, StrokeType)]
strokeG1L = map (\(s, w) -> (Syllables w, EitherHand (S.fromList s))) ks
  where
    ks = [ ([Y,      S      ], "しよう")
         , ([Y,    K,S      ], "みよう")
         , ([Y,T,  K        ], "りよう")
         , ([Y,T,  K,    I,O], "りゆう")
         , ([Y,T,    S,  I,O], "じゆう")
         ]

strokeG2 :: [(StenoWord, StrokeType)]
strokeG2 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([Y,    K,S,A,I  ], [      TI,TN], [    A,S,K      ], "(して)おります")
         , ([      K,S,A,I  ], [           ], [  I,      H    ], "もうひとつ")
         , ([      K,S,A,I  ], [           ], [      S        ], "もうすこし")
         , ([      K,  A,I  ], [      TI,TN], [          H,  Y], "こういうような")
         , ([      K,  A,I  ], [      TI,TN], [  I,      H,  Y], "こういうように")
         , ([      K,  A,I  ], [      TI,TN], [  I,      H,  Y], "そういうような")
         , ([        S,A,I  ], [      TI,TN], [          H,  Y], "そういうように")
         , ([              O], [      TI,TN], [          H,  Y], "おなじような")
         , ([              O], [      TI,TN], [  I,      H,  Y], "おなじように")
         , ([Y,T,          O], [TT,TK      ], [O,  A,  K,H    ], "とりあげ")
         , ([Y,T,          O], [TT,TK,TI,TN], [O,  A,  K,H    ], "とりあげる")
         , ([Y,T,          O], [TT,TK,TI,TN], [O,  A,  K,H,  Y], "とりあげた")
         , ([Y,        A    ], [           ], [  I,A,S,K      ], "やむをえ")
         ]

-- I am not sure which is stared
strokeG3S :: [(StenoWord, StrokeType)]
strokeG3S = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([    H          ], [TT         ], [               ], "ふつう")
         , ([Y,    K        ], [   TK      ], [               ], "きおく")
         , ([      K,      O], [   TK,TI   ], [O,  A,      T  ], "こぎって")
         , ([  T,        I,O], [           ], [               ], "つづ")
         , ([          A,I  ], [TT,TK      ], [               ], "をも おも")
         ]

strokeP :: [(StenoWord, StrokeType)]
strokeP = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([               ], [         TN], [        K      ], "、。")
         , ([               ], [         TN], [    A,  K      ], "「")
         , ([               ], [         TN], [    A,  K,H    ], "」")
         , ([               ], [         TN], [            T  ], "つなぎのしるし")
         ]
{-
strokeC2 :: [(StenoWord, StrokeType)]
strokeC2 = map (\(s, n, w) -> (Syllables w, LeftMiddle (S.fromList s) (S.fromList n))) ks
  where
    ks = [ ([Y,T,H,K,S,A,I,O], [TT,TK], "（）")
         , ([Y,T,H,K,S,A,I,O], [TT,TK], "（）")
         , ([Y,T,H,K,S,A,I,O], [TT,TK], "（）")
         ]

strokeD1 :: [(StenoWord, StrokeType)]
strokeD1 = map (\(s, n, sr, w) -> (Syllables w, LeftRightMiddle (S.fromList s) (S.fromList n) (S.fromList sr))) ks
  where
    ks = [ ([Y,T,H,K,S,A,I,O], [TT,TK,TI,TN], [O,I,A,S,K,H,T,Y], "（）")
         , ([Y,T,H,K,S,A,I,O], [TT,TK,TI,TN], [O,I,A,S,K,H,T,Y], "（）")
         , ([Y,T,H,K,S,A,I,O], [TT,TK,TI,TN], [O,I,A,S,K,H,T,Y], "（）")
         ]

-}

data Priority = Special | Normal | Permitted
  deriving (Eq, Ord, Read, Show)

type Stroke = (Set FingerKey, Set ThumbKey, Set FingerKey)

generateStroke1 :: (StenoWord, StrokeType) -> [(String, Stroke, Priority)]
generateStroke1 (Syllables s, EitherHand f) = [(s, (f, S.empty, S.empty), Normal)]
generateStroke1 (Syllables s, OnlyLeft f) = [(s, (f, S.empty, S.empty), Normal)]
generateStroke1 (Syllables s, Middle n) = [(s, (S.empty, n, S.empty), Normal)]
generateStroke1 (Syllables s, LeftMiddle f n) = [(s, (f, n, S.empty), Normal)]
generateStroke1 (Syllables s, OnlyLeftMiddle f n) = [(s, (f, n, S.empty), Normal)]
generateStroke1 (Syllables s, OnlyRightMiddle nr fr) = [(s, (S.empty, nr, fr), Normal)]
generateStroke1 (Syllables s, LeftRightMiddle f n fr) = [(s, (f, n, fr), Normal)]


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
generateStroke2 (Syllables s1, EitherHand f1) (Syllables s2, EitherHand f2) | head s2 `elem` "いんくちつき" = strokesWithNaka
                                                                            | otherwise = [(s1 ++ s2, (f1, S.empty, f2), Normal)] ++ strokesWithNaka
  where
    strokesWithNaka = do
      (sn, n) <- strokeNaka
      return (s1 ++ sn ++ s2, (f1, n, f2), Normal)
generateStroke2 (Syllables s1, EitherHand f1) (Syllables s2, LeftMiddle f2 n2) = [(s1 ++ s2, (f1, mirrorThumbs n2, f2), Permitted)]
-- generateStroke2 (Syllables s1, EitherHand f1) (Syllables s2, OnlyRightMiddle nr2 f2) = [(s1 ++ s2, (f1, nr2, f2), Normal)] -- ?
generateStroke2 (Syllables s1, LeftMiddle f1 n1) (Syllables s2, EitherHand f2) = [(s1 ++ s2, (f1, n1, f2), Normal)]
generateStroke2 (Syllables s1, LeftMiddle f1 n1) (Syllables s2, LeftMiddle f2 n2) = [(s1 ++ s2, (f1, n1 `S.union` mirrorThumbs n2, f2), Permitted)]
-- generateStroke2 (Syllables s1, LeftMiddle f1 n1) (Syllables s2, OnlyRightMiddle nr2 f2) = [(s1 ++ s2, (f1, n1 `S.union` nr2, f2), Normal)] -- ?
generateStroke2 (Syllables s1, OnlyLeftMiddle f1 n1) (Syllables s2, EitherHand f2) = [(s1 ++ s2, (f1, n1, f2), Normal)]
generateStroke2 (Syllables s1, OnlyLeftMiddle f1 n1) (Syllables s2, LeftMiddle f2 n2) = [(s1 ++ s2, (f1, n1 `S.union` mirrorThumbs n2, f2), Permitted)]

basicStrokeTemplates = concat [ strokeKiso
                              , strokeA
                              , strokeB1
                              , strokeB2
                              , strokeB3
                              , strokeB4
                              -- , strokeB5
                              , strokeC1
                              , strokeC2
                              , strokeC3
                              , strokeC4
                              , strokeC5
                              , strokeC6
                              , strokeC7
                              , strokeC8
                              , strokeD1
                              , strokeD2
                              , strokeD3
                              , strokeD4
                              , strokeD5
                              , strokeD6
                              , strokeD7
                              , strokeD8
                              , strokeD9
                              , strokeE1
                              , strokeE2
                              , strokeE3
                              , strokeE4
                              , strokeE5
                              , strokeE6
                              , strokeE7
                              , strokeF1
                              , strokeF2
                              , strokeF3
                              , strokeF4
                              , strokeG1S
                              -- , strokeG1L
                              , strokeG2
                              , strokeG3S
                              ]

basicStrokes = concatMap generateStroke1 basicStrokeTemplates ++ complexStrokes
  where
    complexStrokes = concat $ do
      t1 <- basicStrokeTemplates
      t2 <- basicStrokeTemplates
      return $ generateStroke2 t1 t2

strokeTable = M.fromListWith S.union $ map (\(t, s, p) -> (s, S.singleton (t, p))) basicStrokes

showStroke :: Stroke -> String
showStroke (f1, n, f2) = concat [ concatMap show $ S.toAscList f1
                                , "|"
                                , concatMap (tail . show) $ S.toAscList n
                                , "|"
                                , concatMap show $ S.toDescList f2
                                ]

main :: IO ()
main = do
  let duplicatedStrokes = M.filter (\xs -> length xs > 1) strokeTable
  print $ length strokeTable
  _ <- mapM_ putStrLn $ map (\(s, sps) -> showStroke s ++ ": " ++ L.intercalate ", " (map fst $ S.toList sps)) $ M.toList duplicatedStrokes
  someFunc
