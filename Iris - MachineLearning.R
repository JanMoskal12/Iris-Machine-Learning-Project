#Wczytanie potrzebnych bibiotek
library(tidyverse)
library(caret)
library(randomForest)

#Ustawienie jądra generatora liczb losowych
set.seed(1234)

#Przyjrzenie się datasetowi
View(iris)
str(iris)
summary(iris)
Data <- iris

#Szukam korelacji między Sepal.Width i Sepal.Length
ggplot(Data,
       aes(x = Sepal.Length,
           y = Sepal.Width, 
           color = Species))+
  geom_point()

#Szukam korelacji między Petal.Width i Petal.Length
ggplot(Data,
       aes(x = Petal.Length,
           y = Petal.Width,
           color = Species))+
  geom_point()

#Sprawdzam jaki procent danych gatunków występuje w zbiorze danych
 prop.table(table(iris$Species))*100 
 
#Podział danych na traing data i test data za pomocą library(caret)
 TrainingDataSize <- createDataPartition(Data$Species,
                                       p = 0.75,
                                       list = FALSE)
 TrainingData <- Data[TrainingDataSize,]
 TestData <- Data[-TrainingDataSize,]
 
 #Tworzenie modelu korzystając z algorytmu k-nearest neighbour (knn)
 knnmodel <- train(Species ~ .,
                data = TrainingData,
                method = "knn",
                trControl =  trainControl(method = "cv"),
                tuneGrid = data.frame(k = 7))
 
 #Teraz przetestuje ten model na 'test data'
 best_model.test <- predict(knnmodel, TestData)
 round(print(best_model.test), digits = 0)
 
 #Ocena wydajności modelu predykcyjnego przy użyciu macierzy pomyłek
 best_model.test.confusion <- confusionMatrix(best_model.test, TestData$Species)
 best_model.test.confusion
 
 

 
 