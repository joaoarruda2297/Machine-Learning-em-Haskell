module KNN (distancia, knnClassificar, mostFrequent) where

import qualified Data.Vector as V
import Data.List (sortBy, group, sort, maximumBy)
import Tools ( Ponto )

--distancia euclidiana
distancia :: Ponto -> Ponto -> Double
distancia p1 p2 = sqrt $ V.sum (V.zipWith (\x1 x2 -> (x2 - x1) * (x2 - x1) ) p1 p2)

--classificador KNN
knnClassificar :: Int -> [(Ponto, String)] -> Ponto -> String
knnClassificar k pontosTreinamento pontoTeste = 
  let pontosOrdenados = sortBy (\(p1, _) (p2, _) -> compare (distancia p1 pontoTeste) (distancia p2 pontoTeste)) pontosTreinamento
      kPontosMaisProximos = take k pontosOrdenados
      classesKPontos = map snd kPontosMaisProximos
  in mostFrequent classesKPontos
  
--função que encontra o mais frequente dentre os 5 vizinhos
mostFrequent :: (Ord a) => [a] -> a
mostFrequent = head . maximumBy (\x y -> compare (length x) (length y)) . group . sort