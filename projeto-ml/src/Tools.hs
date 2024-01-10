module Tools (
  loadCSV, 
  getColumn, 
  parsePonto, 
  Ponto,
  calculaAcuracia,
  calculaPrecisao,
  calculaRecall,
  calculaF1) where 

import Data.Csv ( decode, HasHeader(NoHeader) )
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

loadCSV :: FilePath -> IO (Either String (Vector (Vector String)))
loadCSV filePath = do
  csvData <- BL.readFile filePath
  return $ decode NoHeader csvData

getColumn :: Int -> Vector (Vector String) -> Vector String
getColumn colIndex = fmap (! colIndex)

type Ponto = Vector Double

--transforma o vetor de string em vetor de pontos double
parsePonto :: Vector String -> Ponto
parsePonto = V.map read

-- Metricas de desempenho 
calculaAcuracia :: (Eq a) => [a] -> [a] -> Double 
calculaAcuracia y_true y_pred = fromIntegral (length acertos) / fromIntegral (length y_true)
  where 
    acertos = filter (\(y1, y2) -> y1 == y2) (zip y_true y_pred)

--verificar porcentagem de class positiva corretas em relacao a todas as class positivas classificadas
calculaPrecisao :: [String] -> [String] -> Double 
calculaPrecisao y_true y_pred = fromIntegral (length verdadeiroPositivo) / fromIntegral (length (verdadeiroPositivo ++ falsoPositivo))
  where 
    verdadeiroPositivo = filter (\(y, y') -> y == "M" && y' == "M") (zip y_true y_pred)
    falsoPositivo = filter (\(y, y') -> y == "B" && y' == "M") (zip y_true y_pred)

calculaRecall :: [String] -> [String] -> Double 
calculaRecall y_true y_pred = fromIntegral (length verdadeiroPositivo) / fromIntegral (length (verdadeiroPositivo ++ falsoNegativo)) --verificar porcentagem de class positiva corretas em relacao a todas as class que deveriam ser positivas
  where 
    verdadeiroPositivo = filter (\(y, y') -> y == "M" && y' == "M") (zip y_true y_pred)
    falsoNegativo = filter (\(y, y') -> y == "M" && y' == "B") (zip y_true y_pred)

calculaF1:: Double -> Double -> Double 
calculaF1 taxaPrecisao taxaRecall = 2 * (taxaPrecisao * taxaRecall) / (taxaPrecisao + taxaRecall) --media harmonica entre recall e precisão