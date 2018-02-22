rm(list=ls()) #Remueve los objetos de espacio de trabajo.

#install.packages("ISLR")
#install.packages("tree")

library(ISLR) #Provide the collection of data-sets used in the book 'An Introduction to Statistical Learning with Applications in R'.
library(tree) #Classification and Regression Trees.

# Informacion a ser usada, conjunto de datos simulado de ventas de asientos de niños para autos en 400 tiendas.
data("Carseats")
attach(Carseats) 
head(Carseats)

#Se empieza con la manipulacion de la informacion.
Sales #Unidades vendidas (miles) en cada tienda.
range(Sales) #  Rango de 0 a 16

#Crear variables categoricas basadas en las ventas.
High=ifelse(Sales>=8, "Yes", "No")
High

#Agrega la columna de High al data set que se tenia.
 Carseats=data.frame(Carseats,High) 
 head(Carseats)
 names(Carseats)

#Elimina la variable Sales del data-set Carseats  
 Carseats=Carseats[,-1] #Carseats$High <- NULL   #Comando para eliminar una variable del dataframe. 
 head(Carseats)
 names(Carseats)
 
 
 set.seed(2)
 
 #Genera un vector de 200 elementos con valores entre el 0:400. 
 train=sample(1:nrow(Carseats),nrow(Carseats)/2)
 train
 test=-train
 test
 
 #Divide la informacion para prueba y entrenamiento del arbol de desiciones.
 training_data=Carseats[train,] #Asigna los elementos del data-set del vector train.
 testing_data=Carseats[test,] #Asigna el resto de los elementos para las pruebas. 
 
 dim(training_data)
 dim(testing_data)
 
 testing_High=High[test]
 testing_High
 dim(testing_High)
 
 #Ajuste del modelo con los datos de entrenamiento. 

 tree_model=tree(High~.,training_data)
 plot(tree_model)
 text(tree_model,pretty=0)
 
 
 #Validar el modelo con la informacion de prueba. 
 
 tree_pred=predict(tree_model,testing_data,type="class")
 mean(tree_pred != testing_High) #0.285 =28.5%
 
 #Prune the tree
 #Cross validation to check where to stop pruning
 
 set.seed(3)
 cv_tree=cv.tree(tree_model,FUN=prune.misclass)
 names(cv_tree)
 
 plot(cv_tree$size,cv_tree$dev,type="b")
 
 #prune the tree
 
 pruned_model=prune.misclass(tree_model,best=9)
 plot(pruned_model)
 text(pruned_model,pretty=0)
 #check how is doing
 
 tree_pred1=predict(pruned_model,testing_data,type="class")
 mean(tree_pred1 != testing_High)
 