{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module NaiveBayes (fitBayes, predictBayes) where

import Data.Foldable (maximumBy)
import Data.List (nub)
import Data.Ord (comparing)
import Data.Vector (Vector, (!))
import qualified Data.Vector  as V
import Tools (Ponto)

data Model = Model
  { probPorFeat :: [(Ponto, Ponto)],
    cls :: [(Int, String)],
    probPorClasse :: [Double]
  }


media :: (Fractional a) => [a] -> a
media xs = sum xs / fromIntegral (length xs)

std :: (Floating a) => [a] -> a
std xs = sqrt (sum [(x - media xs) * (x - media xs) | x <- xs] / fromIntegral (length xs))

-- Padroniza todos os valores
padronizaValPontos :: Vector Double -> Vector Double
padronizaValPontos vec = V.map (\x -> (x - mean) / stdDev) vec
  where
    mean = media (V.toList vec)
    stdDev = std (V.toList vec)

-- Padroniza Todos os valores de uma Lista de Pontos
padronizaValPontoss :: [Vector Double] -> [Vector Double]
padronizaValPontoss = map padronizaValPontos

getColuna :: [Ponto] -> Int -> [Double]
getColuna [] _ = []
getColuna (pt : pts) idx = (pt ! idx) : getColuna pts idx

valoresXMedia :: [Ponto] -> Ponto
valoresXMedia base = V.fromList (valoresXMedia' (padronizaValPontoss base) (V.length (head base) - 1))
  where
    valoresXMedia' _ (-1) = []
    valoresXMedia' baseU n = media (getColuna baseU n) : valoresXMedia' baseU (n - 1)

valoresXStd :: [Ponto] -> Ponto
valoresXStd base = V.fromList (valoresXStd' (padronizaValPontoss base) (V.length (head base) - 1))
  where
    valoresXStd' _ (-1) = []
    valoresXStd' baseU n = std (getColuna base n) : valoresXStd' baseU (n - 1)

filtraDadosPorClasse :: (b -> Bool) -> [Ponto] -> [b] -> [Ponto]
filtraDadosPorClasse condition matrix series =
  let filteredRows = filter (\(_, element) -> condition element) (zip matrix series)
   in map fst filteredRows


-- Utiliza a função Auxiliar fitBayes' para treinar o modelo, re
fitBayes :: [Ponto] -> [String] -> Model
fitBayes xss ys =
  Model
    { probPorFeat = [fitBayes' xss i | i <- classes],
      cls = zip [0 ..] classes,
      probPorClasse = probabilidadesClasses ys classes
    }
  where
    fitBayes' zss cl = (valoresXMedia (filtraDadosPorClasse (== cl) zss ys), valoresXStd (filtraDadosPorClasse (== cl) zss ys))

    classes = nub ys

-- Usa uma gaussiana para o calculo da probabilidade - Utilizando o Epsilon como parametro de Suavização
calculaProb :: Floating a => a -> a -> a -> a
calculaProb val mean stdev = (1 / (sqrt (2 * pi) * stdev)) * exp (-((val - mean) ** 2) / (2 * stdev ** 2))
                            + epsilon
  where
    epsilon = 0.00001

probabilidadesClasses :: [String] -> [String] -> [Double]
probabilidadesClasses [] _ = []
probabilidadesClasses _ [] = []
probabilidadesClasses todos (cl : cls) = fromIntegral (Prelude.length $ Prelude.filter (== cl) todos) / fromIntegral (Prelude.length todos) : probabilidadesClasses todos cls

predictBayes :: Model -> [Ponto] -> [String]
predictBayes model xs = map (predictBayesX model) (padronizaValPontoss xs)

-- Faz a predição dos valores para uma Amostra X
predictBayesX :: Model -> Ponto -> String
predictBayesX model x =
  let n = V.length x - 1
      logProb = [(clsIndex, predictBayes' (clsIndex, clsName) model x (log $ probPorClasse model !! clsIndex) n) | (clsIndex, clsName) <- cls model]
      (maxIndex, _) = maximumBy (comparing snd) logProb
   in snd $ cls model !! maxIndex

-- Utiliza recursão para se Analisar o valor de cada Feature do Modelo
predictBayes' :: (Int, String) -> Model -> Ponto -> Double -> Int -> Double
predictBayes' (clsIndex, _) model x antes n
  | n == -1 = antes
  | otherwise = predictBayes' (clsIndex, clsName) model x (predictBayes'' (clsIndex, clsName) model x antes n) (n - 1)
  where
    (_, clsName) = cls model !! clsIndex

-- Calcula a chance de um Ponto pertencer a uma classe
predictBayes'' :: (Int, String) -> Model -> Ponto -> Double -> Int -> Double
predictBayes'' (clsIndex, _) model x antes n =
  let (mean, stdev) = probPorFeat model !! clsIndex
      probs = [calculaProb (x ! i) (mean ! i) (stdev ! i) | i <- [0 .. n]]
      chance = product probs
   in antes + log chance
