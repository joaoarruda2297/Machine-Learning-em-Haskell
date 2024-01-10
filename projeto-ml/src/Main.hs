module Main (main) where

import qualified Data.Vector as V
import Tools ( loadCSV, parsePonto, calculaAcuracia, calculaPrecisao, calculaRecall, calculaF1 ) 
import KNN ( knnClassificar )
import DecisionTree (criaArvore, predictArvore)
import NaiveBayes (fitBayes, predictBayes)

main :: IO ()
main = do
  result_X_train <- loadCSV "data/X_train.csv"
  result_Y_train <- loadCSV "data/y_train.csv"
  result_X_test <- loadCSV "data/X_test.csv"
  result_Y_test <- loadCSV "data/y_test.csv"

  case (result_X_train, result_Y_train, result_X_test, result_Y_test) of
    (Right csv_X_train, Right csv_Y_train, Right csv_X_test, Right csv_Y_test) -> do
      let pontos_X_train = tail $ V.toList $ V.map parsePonto csv_X_train
          pontos_Y_train = tail $ map (head . V.toList) (V.toList csv_Y_train)
          pontos_X_test = tail $ V.toList $ V.map parsePonto csv_X_test
          pontos_Y_test = tail $ map (head . V.toList) (V.toList csv_Y_test)
          pontosTreinamento = zip pontos_X_train pontos_Y_train

          -- Classificador KNN
          k = 5 --numero de vizinhos
          y_pred_knn = fmap (knnClassificar k pontosTreinamento) pontos_X_test
          
          acc_knn = calculaAcuracia pontos_Y_test y_pred_knn
          prec_knn = calculaPrecisao pontos_Y_test y_pred_knn
          recall_knn = calculaRecall pontos_Y_test y_pred_knn
          f1_knn = calculaF1 prec_knn recall_knn

          -- Arvore classifiacao
          arvore_classificacao = criaArvore pontos_X_train pontos_Y_train
          y_pred_arvore = predictArvore pontos_X_test arvore_classificacao

          acc_arvore = calculaAcuracia pontos_Y_test y_pred_arvore
          prec_arvore = calculaPrecisao pontos_Y_test y_pred_arvore
          recall_arvore = calculaRecall pontos_Y_test y_pred_arvore
          f1_arvore = calculaF1 prec_arvore recall_arvore

          -- Naive Bayes
          naive_bayes = fitBayes pontos_X_train pontos_Y_train
          y_pred_bayes = predictBayes naive_bayes pontos_X_test

          acc_bayes = calculaAcuracia pontos_Y_test y_pred_bayes
          prec_bayes = calculaPrecisao pontos_Y_test y_pred_bayes
          recall_bayes = calculaRecall pontos_Y_test y_pred_bayes
          f1_bayes = calculaF1 prec_bayes recall_bayes

      putStrLn "Classificador KNN"
      putStrLn $ "Taxa de Acurácia: " ++ show acc_knn
      putStrLn $ "Taxa de Precisão: " ++ show prec_knn
      putStrLn $ "Taxa de Recall: " ++ show recall_knn
      putStrLn $ "Taxa de F1: " ++ show f1_knn
      putStrLn ""
      putStrLn "Classificador Arvore de Classificacao"  
      putStrLn $ "Taxa de Acurácia: " ++ show acc_arvore
      putStrLn $ "Taxa de Precisão: " ++ show prec_arvore
      putStrLn $ "Taxa de Recall: " ++ show recall_arvore
      putStrLn $ "Taxa de F1: " ++ show f1_arvore
      putStrLn ""
      putStrLn "Classificador Gaussian Naive Bayes"  
      putStrLn $ "Taxa de Acurácia: " ++ show acc_bayes
      putStrLn $ "Taxa de Precisão: " ++ show prec_bayes
      putStrLn $ "Taxa de Recall: " ++ show recall_bayes
      putStrLn $ "Taxa de F1: " ++ show f1_bayes
      putStrLn ""
    _ -> putStrLn "Erro ao carregar dados."