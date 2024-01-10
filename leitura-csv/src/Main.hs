module Main (main) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

loadCSV :: FilePath -> IO (Either String (Vector (Vector String)))
loadCSV filePath = do
  csvData <- BL.readFile filePath
  return $ decode NoHeader csvData

getColumn :: Int -> Vector (Vector String) -> Vector String
getColumn colIndex csv = fmap (\row -> row ! colIndex) csv

main :: IO ()
main = do
  result <- loadCSV "data/breast-cancer.csv"
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right csv -> do 
      let column = getColumn 0 csv
      print (V.length column)