{-# LANGUAGE InstanceSigs #-} -- Permite assinatura em instancias
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use second" #-}

module DecisionTree (ArvoreClassificacao, criaArvore, predictArvore) where
import Tools ( Ponto )
import Data.List (sortBy, minimumBy)
import Data.Vector as V ( (!), length)
import Data.Ord (comparing)

data ArvoreClassificacao =
    Classe String | Decisao (Ponto -> ArvoreClassificacao)

instance Show ArvoreClassificacao where
    show :: ArvoreClassificacao -> String
    show (Classe str) = str
    show (Decisao _) = "Decisao"

{-AUXILIARES PARA CALCULO DA IMPUREZA DE GINI-}
-- Filtra valores unicos da lista
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x:unique (Prelude.filter (x /=) xs)

probabilidadesClasses :: [String] -> [String] -> [Double]
probabilidadesClasses [] _ = []
probabilidadesClasses _ [] = []
probabilidadesClasses todos (cl:cls) = fromIntegral (Prelude.length $ Prelude.filter (==cl) todos) / fromIntegral (Prelude.length todos) : probabilidadesClasses todos cls

impurezaGini :: [Double] -> Double
impurezaGini = Prelude.foldr (\ p -> (+) (1 - p * p)) 0

calculaImpurezaGiniIndividual :: [String] -> Double
calculaImpurezaGiniIndividual strs = impurezaGini probabilidades
    where
        classes = unique strs
        probabilidades = probabilidadesClasses strs classes

calculaImpurezaGiniNo :: [String] -> [String] -> Double
calculaImpurezaGiniNo pontosEsquerda pontosDireita =
    ((calculaImpurezaGiniIndividual pontosEsquerda) * tamanhoEsquerda + (calculaImpurezaGiniIndividual pontosDireita) * tamanhoDireita) / (tamanhoDireita + tamanhoEsquerda)
    where
        tamanhoEsquerda = fromIntegral $ Prelude.length pontosEsquerda
        tamanhoDireita = fromIntegral $ Prelude.length pontosDireita
{-------------------------------------------------------}
encontraIndicies :: (a -> Bool) -> [a] -> Int -> [Int]
encontraIndicies _ [] _ = []
encontraIndicies f (x:xs) contador  | f x       = contador : encontraIndicies f xs (contador + 1)
                                    | otherwise = encontraIndicies f xs (contador + 1)

filtraIndices :: [a] -> [Int] -> [a]
filtraIndices _ [] = []
filtraIndices xs (idx:idxs) = (xs !! idx) : filtraIndices xs idxs

giniDivisao :: [Double] -> [String] -> Double -> Double
giniDivisao valores y threshold = calculaImpurezaGiniNo menores maiores
    where
        indicesMenores = encontraIndicies (< threshold) valores 0
        indicesMaiores = encontraIndicies (> threshold) valores 0
        menores = filtraIndices y indicesMenores
        maiores = filtraIndices y indicesMaiores

-- Encontra valores medios (DEVE ESTAR ORDENADO)
divisoesColuna :: [(Double, String)] -> [Double]
divisoesColuna [] = []
divisoesColuna [(_, _)] = []
divisoesColuna ((v,_):vs) = (v + fst (Prelude.head vs)) / 2 : divisoesColuna vs

-- Auxiliar: Separa um conjunto pontos e um indice e retona lista de Doubles referentes a coluna do indice
getColumn :: [Ponto] -> Int -> [Double]
getColumn [] _ = []
getColumn (pt:pts) idx = (pt ! idx) : getColumn pts idx

-- Retorna a tupla (Gini, Threshold de uma coluna de fatures)
melhorDivisaoGini :: [(Double, String)] -> [Double] -> (Double, Double)
melhorDivisaoGini pts lims = minimumBy (comparing fst) ginis
    where
        (xs, ys) = unzip pts
        ginis = [(giniDivisao xs ys lim, lim) | lim <- lims]

-- Retorna uma decisao de acordo com um conjunto de pontos e strings, retornando o indice da coluna junto ao seu threshold
melhorDivisao :: [Ponto] -> [String] -> (Int, Double)
melhorDivisao [_] _ = (-1, 1)
melhorDivisao xs ys = (idxMelhor, thresholdMelhor)
    where
        qtd_features = V.length $ Prelude.head xs
        features = [getColumn xs i | i <- [0..(qtd_features - 1)]]
        pares = [zip feat ys | feat <- features]
        paresOrdenados = [sortBy (\(f1, _) (f2, _) -> compare f1 f2) par | par <- pares]
        divisoesPossiveis = [divisoesColuna parOrdenado | parOrdenado <- paresOrdenados]
        ginisColunas = [melhorDivisaoGini col divisoes | (col, divisoes) <- (zip paresOrdenados divisoesPossiveis)]
        idxginisColunas = zip [0..(qtd_features - 1)] ginisColunas
        melhor = minimumBy (comparing $ fst . snd) idxginisColunas
        idxMelhor = fst melhor
        thresholdMelhor = snd . snd $ melhor

criaArvore :: [Ponto] -> [String] -> ArvoreClassificacao
criaArvore [] _ = error "Must Pass Data"
criaArvore _ [] = error "Must Pass Data"
criaArvore xs ys | colidx == -1             = Classe (head ys)
                 | Prelude.length ys == 1   = Classe (head ys)
                 | otherwise                = Decisao (\pt -> 
                    if pt ! colidx <= threshold then arvoreEsquerda
                    else arvoreDireita)
    where 
        (colidx, threshold) = melhorDivisao xs ys
        zipPontos = zip xs ys
        pontosEsquerda = [pts | pts <- zipPontos, ((fst pts) ! colidx) <= threshold]
        pontosDireita = [pts | pts <- zipPontos, ((fst pts) ! colidx) > threshold]
        (xsE, ysE) = unzip pontosEsquerda
        (xsD, ysD) = unzip pontosDireita
        arvoreEsquerda = criaArvore xsE ysE
        arvoreDireita = criaArvore xsD ysD

runArvore :: Ponto -> ArvoreClassificacao -> String
runArvore _ (Classe str) = str
runArvore pt (Decisao f) = runArvore pt (f pt)

predictArvore :: [Ponto] -> ArvoreClassificacao -> [String]
predictArvore [] _ = []
predictArvore (x:xs) tree = runArvore x tree : predictArvore xs tree