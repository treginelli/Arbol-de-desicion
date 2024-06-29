library(dplyr)
library(rpart)
library(rpart.plot)

#librerías para mejorar el gráfico
library(rattle)
library(RColorBrewer)

#Objetivo: predecir qué personas tienen más probabilidades de sobrevivir después de la colisión con el iceberg.



#Variables:

#pclass: Clase de pasajeros, factor no ordenado: 1º 2º 3º
#survived: Factor de supervivencia: murió (0) o sobrevivió (1)
#sex: Factor sexo: Masculino (male) Femenino (female)
#age:	Edad en años, Min 0.167 Max 80.0
#sibsp:	Número de hermanos o cónyuges a bordo, entero: 0…8
#parch:	Número de padres o hijos a bordo, entero: 0…6
#ticket:	Número de pasaje
#cabin:	Número de camarote
#embarked:	Factor puerto embarque: (C=Cherbourg, Q=Queenstown, S=Southampton )

path<-"titanic_data.csv"
titanic <-read.csv(path)

head(titanic)

#Resumen de los datos donde se indican estadísticas de las 13 variables
summary(titanic)

#Seteo semilla
set.seed(2024)

#Mezclo aleatoriamente los datos que originalmente vienen ordenados por la clase de pasaje
shuffle_index <- sample(1:nrow(titanic))
head(shuffle_index)

titanic.shuffle <- titanic[shuffle_index, ]
head(titanic.shuffle)

#Elimino variables que no me interesan
clean_titanic= select(titanic.shuffle,-c (home.dest, cabin, name, x, ticket))
head(clean_titanic)

#Convierto la edad y tarifa de texto a numerico
clean_titanic$age<-as.numeric(clean_titanic$age)
clean_titanic$fare<-as.numeric(clean_titanic$fare)

summary(clean_titanic)

#Transformamos las variables de clase (pclass) y supervivencia (survived) a texto
clean_titanic=mutate(clean_titanic, pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Alta', 'Media', 'Baja' )))
clean_titanic=mutate(clean_titanic, survived = factor(survived, levels = c(0, 1), labels = c('No', 'Si')))
clean_titanic=mutate(clean_titanic, sex = factor(sex, levels = c('male','female'), labels = c('Masculino', 'Femenino')))

head(clean_titanic)

#Eliminamos ahora los registros que no contienen todas las variables.
nrow(clean_titanic)
clean_titanic=na.omit(clean_titanic)
nrow(clean_titanic)

#Defino una función para separar los datos en un conjunto de entrenamiento (train) del modelo, y un conjunto para verificación (test) del modelo.
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

#Separo los datos en dos conjuntos
data_train <- create_train_test(clean_titanic, 0.8, train = TRUE)
data_test <- create_train_test(clean_titanic, 0.8, train = FALSE)
dim(data_train)

#Chequeo que ambos conjuntos de datos sean similares
prop.table(table(data_train$survived))
prop.table(table(data_test$survived))

#Ajustamos el modelo con los datos de entrenamiento dejando que se seleccionen automáticamente las variables
fit <- rpart(survived~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)

#Mejoramos el gráfico
fancyRpartPlot(fit)

#Calculamos la predicción
predict_unseen <-predict(fit, data_test, type = 'class')
table_mat <- table(data_test$survived, predict_unseen)
table_mat

#Construimos tabla de porcentajes
100.0*table_mat/nrow(data_test)

#Medición de Performance
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test (%):', 100.0*accuracy_Test))

#Ajuste del modelo
#rpart.control() permite ajustar los parámetros utilizados para particionar y eventualmente mejorar la performance

