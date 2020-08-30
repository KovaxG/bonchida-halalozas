module Table (
  Alignment(..),
  latexTable1, latexTable2, latexTable3,
  latexTable4, latexTable5, latexTable6,
  latexTable7, latexTable8, latexTable9
  ) where

import Data.List

data Alignment = LeftAlign | MiddleAlign | RightAlign

instance Show Alignment where
  show LeftAlign = "l"
  show MiddleAlign = "m"
  show RightAlign = "r"

latexTable1 :: Show a
             => String
             -> [a]
             -> Alignment
             -> String
latexTable1 label as al =
  latexTable [label]
              (fmap (\a -> [show a]) as)
              [al]

latexTable2 :: (Show a, Show b)
             => (String, String)
             -> [(a, b)]
             -> (Alignment, Alignment)
             -> String
latexTable2 (l1,l2) as (a1,a2) =
  latexTable [l1, l2]
              (fmap (\(a,b) -> [show a, show b]) as)
              [a1, a2]

latexTable3 :: (Show a, Show b, Show c)
             => (String, String, String)
             -> [(a, b, c)]
             -> (Alignment, Alignment, Alignment)
             -> String
latexTable3 (l1,l2,l3) as (a1,a2,a3) =
  latexTable [l1, l2, l3]
              (fmap (\(a,b,c) -> [show a, show b, show c]) as)
              [a1, a2, a3]

latexTable4 :: (Show a, Show b, Show c, Show d)
             => (String, String, String, String)
             -> [(a, b, c, d)]
             -> (Alignment, Alignment, Alignment, Alignment)
             -> String
latexTable4 (l1,l2,l3,l4) as (a1,a2,a3,a4) =
  latexTable [l1, l2, l3, l4]
              (fmap (\(a,b,c,d) -> [show a, show b, show c, show d]) as)
              [a1, a2, a3, a4]

latexTable5 :: (Show a, Show b, Show c, Show d, Show e)
             => (String, String, String, String, String)
             -> [(a, b, c, d, e)]
             -> (Alignment, Alignment, Alignment, Alignment, Alignment)
             -> String
latexTable5 (l1,l2,l3,l4,l5) as (a1,a2,a3,a4,a5) =
  latexTable [l1, l2, l3, l4, l5]
              (fmap (\(a,b,c,d,e) -> [show a, show b, show c, show d, show e]) as)
              [a1, a2, a3, a4, a5]

latexTable6 :: (Show a, Show b, Show c, Show d, Show e, Show f)
             => (String, String, String, String, String, String)
             -> [(a, b, c, d, e, f)]
             -> (Alignment, Alignment, Alignment, Alignment, Alignment, Alignment)
             -> String
latexTable6 (l1,l2,l3,l4,l5,l6) as (a1,a2,a3,a4,a5,a6) =
  latexTable [l1, l2, l3, l4, l5, l6]
              (fmap (\(a,b,c,d,e,f) -> [show a, show b, show c, show d, show e, show f]) as)
              [a1, a2, a3, a4, a5, a6]

latexTable7 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g)
             => (String, String, String, String, String, String, String)
             -> [(a, b, c, d, e, f, g)]
             -> (Alignment, Alignment, Alignment, Alignment, Alignment, Alignment, Alignment)
             -> String
latexTable7 (l1,l2,l3,l4,l5,l6,l7) as (a1,a2,a3,a4,a5,a6,a7) =
  latexTable [l1, l2, l3, l4, l5, l6, l7]
              (fmap (\(a,b,c,d,e,f,g) -> [show a, show b, show c, show d, show e, show f, show g]) as)
              [a1, a2, a3, a4, a5, a6, a7]

latexTable8 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h)
             => (String, String, String, String, String, String, String, String)
             -> [(a, b, c, d, e, f, g, h)]
             -> (Alignment, Alignment, Alignment, Alignment, Alignment, Alignment, Alignment, Alignment)
             -> String
latexTable8 (l1,l2,l3,l4,l5,l6,l7,l8) as (a1,a2,a3,a4,a5,a6,a7,a8) =
  latexTable [l1, l2, l3, l4, l5, l6, l7, l8]
              (fmap (\(a,b,c,d,e,f,g,h) -> [show a, show b, show c, show d, show e, show f, show g, show h]) as)
              [a1, a2, a3, a4, a5, a6, a7, a8]

latexTable9 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i)
             => (String, String, String, String, String, String, String, String, String)
             -> [(a, b, c, d, e, f, g, h, i)]
             -> (Alignment, Alignment, Alignment, Alignment, Alignment, Alignment, Alignment, Alignment, Alignment)
             -> String
latexTable9 (l1,l2,l3,l4,l5,l6,l7,l8,l9) as (a1,a2,a3,a4,a5,a6,a7,a8,a9) =
  latexTable [l1, l2, l3, l4, l5, l6, l7, l8, l9]
              (fmap (\(a,b,c,d,e,f,g,h,i) -> [show a, show b, show c, show d, show e, show f, show g, show h, show i]) as)
              [a1, a2, a3, a4, a5, a6, a7, a8, a9]

latexTable :: [String] -> [[String]] -> [Alignment] -> String
latexTable labels as alignments =
  filter (\c -> notElem c "\"\'")
  $ unlines
  $ beginTabular (alingments alignments)
  : hline
  : row labels
  : []
  ++ fmap row as
  ++ [endTabular]

beginTabular :: String -> String
beginTabular thing = "\\begin{tabular}{" ++ thing ++ "}"

endTabular :: String
endTabular = "\\end{tabular}"

alingments :: [Alignment] -> String
alingments as = concat $ [sep] ++ intersperse sep (show <$> as) ++ [sep]
  where sep = " | "

row :: [String] -> String
row vs = concat (intersperse " & " vs) ++ unwords [newline, hline]

hline :: String
hline = "\\hline"

newline :: String
newline = "\\\\"
