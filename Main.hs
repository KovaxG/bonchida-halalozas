import           Data.Function ((&))
import qualified Data.List as List
import qualified Text.Printf as Text

import Common
import Table
import Row (Row, Sex(..))
import qualified Row

main :: IO ()
main = do
  let filePath = "adatok.csv"
  Text.printf "Reading from \"%s\"\n" filePath
  contents <- readFile filePath
  either putStrLn generateOutput $ Row.parseData (removeFirstLine contents)

generateOutput :: [Row] -> IO ()
generateOutput rows = do
  putStrLn "\n[General Data]"
  printInfo "Total people (#): " length
  printInfo "Total men (#):" men
  printInfo "Total women (#):" women
  printInfo "Average life for men (years):" (averageLife ((== Male) . Row.sex))
  printInfo "Average life for women (years):" (averageLife ((== Female) . Row.sex))
  printInfo "Average life length (years): " (averageLife everything)
  printInfo "Shortest life (days): " shortestLife
  printInfo "Longest life (years): " longestLife

  putStrLn "\n[Plots]"
  printInfo "Age/Deaths for men (Years/#): " (deathsPerNumber ((== Male) . Row.sex))
  printInfo "Age/Deaths for women (Years/#): " (deathsPerNumber ((== Female) . Row.sex))
  printInfo "Age/Deaths (Years/#): " (deathsPerNumber everything)

  putStrLn "\n[Occupations]"
  printInfo "Occupations: (Occupation/#): " occupations
  where
    printInfo :: Show a => String -> ([Row] -> a) -> IO ()
    printInfo msg f = putStrLn $ msg ++ show (f rows)

men :: [Row] -> Int
men = count Male . map Row.sex

women :: [Row] -> Int
women = count Female . map Row.sex

averageLife :: (Row -> Bool) -> [Row] -> Double
averageLife p rows = (fromIntegral $ sum $ map Row.years filtered) / (List.genericLength filtered)
  where filtered = filter p rows

shortestLife :: [Row] -> Int
shortestLife rows = (Row.years shortest * 365) + (maybe 0 (*30) $ Row.months shortest) + (maybe 0 id $ Row.days shortest)
  where
    shortest :: Row
    shortest = List.minimumBy rowComparator rows

longestLife :: [Row] -> Int
longestLife = maximum . fmap Row.years

deathsPerNumber :: (Row -> Bool) -> [Row] -> [(Int, Int)]
deathsPerNumber p rows = map (\y -> (y, count y $ map Row.years $ filter p rows)) [0 .. 100]

occupations :: [Row] -> [(String, Int)]
occupations = reverse . List.sortOn snd . map (\os -> (occupationString $ head os, length os)) . List.group . List.sort . map Row.occupation
  where
    occupationString :: Maybe String -> String
    occupationString Nothing = "Not Specified"
    occupationString (Just occ) = occ

rowComparator :: Row -> Row -> Ordering
rowComparator a b
  | Row.years a > Row.years b = GT
  | Row.years a < Row.years b = LT
  | otherwise = if res == EQ then maybeComparator (Row.days a) (Row.days b) else res
  where
    res :: Ordering
    res = maybeComparator (Row.months a) (Row.months b)

    maybeComparator :: Ord a => Maybe a -> Maybe a -> Ordering
    maybeComparator ma mb = case (ma, mb) of
      (Nothing, Nothing) -> EQ
      (Just _, Nothing) -> GT
      (Nothing, Just _) -> LT
      (Just a, Just b) -> compare a b

printColumn :: Show a => [Row] -> (Row -> a) -> IO ()
printColumn rows f = mapM_ (\(i, r) -> Text.printf "%03d: %s\n" i (show $ f r)) (zip index rows)
  where index = [1..] :: [Int]

printColumnValues :: (Show a, Ord a) => [Row] -> (Row -> a) -> IO ()
printColumnValues rows accesser = do
  let values = histogram (fmap accesser rows)
  Text.printf "Unique values: %d\n" (length values)
  mapM_ (\(a, f) -> Text.printf "%s - %d\n" (show a) f) values

histogram :: Ord a => [a] -> [(a, Int)]
histogram = fmap valAndLength . List.group . List.sort
  where
    valAndLength :: [a] -> (a, Int)
    valAndLength as@(a:_) = (a, length as)
    valAndLength _ = error "This should never happen (histogram)"
