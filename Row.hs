module Row where

import qualified Data.Bifunctor as Bifunctor
import qualified Text.ParserCombinators.Parsec as Parsec
import           Text.ParserCombinators.Parsec ((<|>))
import qualified Text.Printf as Text

import Common

data Row = Row
  { index :: Int
  , death :: Date
  , burial :: Date
  , name :: String
  , maidenName :: Maybe String
  , religion :: String
  , occupation :: Maybe String
  , address :: String
  , birthplace :: String
  , years :: Int
  , months :: Maybe Int
  , days :: Maybe Int
  , sex :: Sex
  , maritalStatus :: Maybe MaritalStatus
  , cause :: String
  , priest :: String
  , notes :: String
  } deriving (Show, Eq, Ord)

data Date = Date Int Int Int deriving (Eq, Ord)

instance Show Date where
  show (Date y m d) = Text.printf "%d-%02d-%02d" y m d

data Sex = Male | Female deriving (Show, Eq, Ord)
data MaritalStatus = Unmarried | Married | Divorced deriving (Show, Eq, Ord)

parseData :: String -> Either String [Row]
parseData = Bifunctor.first show . Parsec.parse (Parsec.many row) "Parsing rows"
  where
    check s = if s == "-" then Nothing else Just s

    row = do
      index <- read <$> Parsec.many1 Parsec.digit
      Parsec.char ','
      death <- date
      Parsec.char ','
      burial <- date
      Parsec.char ','
      name <- string
      Parsec.char ','
      maidenName <- check <$> string
      Parsec.char ','
      religion <- string
      Parsec.char ','
      occupation <- check <$> string
      Parsec.char ','
      address <- string
      Parsec.char ','
      birthplace <- string
      Parsec.char ','
      years <- read <$> Parsec.many1 Parsec.digit
      Parsec.char ','
      months <- safeRead <$> string
      Parsec.char ','
      days <- safeRead <$> string
      Parsec.char ','
      sex <- sex
      Parsec.char ','
      maritalStatus <- maritalStatus
      Parsec.char ','
      cause <- string
      Parsec.char ','
      priest <- string
      Parsec.char ','
      notes <- Parsec.many1 $ Parsec.noneOf "\n"
      Parsec.char '\n'
      return $ Row index death burial name maidenName religion occupation address
                birthplace years months days sex maritalStatus cause priest notes

    date = do
      y <- read <$> Parsec.many1 Parsec.digit
      Parsec.char '/'
      m <- read <$> Parsec.many1 Parsec.digit
      Parsec.char '/'
      d <- read <$> Parsec.many1 Parsec.digit
      return $ Date y m d

    sex = do
      s <- Parsec.many1 Parsec.alphaNum
      return $ if safeHead s == Just 'f' then Male else Female

    maritalStatus = do
      s <- Parsec.many1 $ Parsec.noneOf ","
      return $ case s of
        "nőtlen/hajadon" -> Just Unmarried
        "nős/férjezett"  -> Just Married
        "özvegy/elvállt" -> Just Divorced
        _ -> Nothing

    string = quoted <|> Parsec.many1 (Parsec.noneOf ",")

    quoted = do
      Parsec.char '\"'
      s <- Parsec.many $ Parsec.noneOf "\""
      Parsec.char '\"'
      return s
