library(dplyr)
library(keras)
library(tensorflow)
library(yardstick)

options(scipen = 999)
#importo i dati - train set dalla libreria KERAS

mnist <- dataset_mnist()
X_train <- mnist$train$x
X_test <- mnist$test$x
y_train <- mnist$train$y
y_test <- mnist$test$y

print (X_train, 1)

# È un buon inizio, ma non abbiamo ancora finito. Utilizzaremo solo livelli l
# lineari (senza convoluzioni), quindi dovrai rimodellare le immagini di input da 28×28 a 1×784 ciascuna. 
# Si può fare con la funzione array_reshape() di Keras. 
# Inoltre, divideremo anche ogni valore della matrice dell'immagine per 255, quindi tutte le immagini sono nell'intervallo [0, 1].

#Questo gestirà le immagini di input, ma dobbiamo anche convertire le etichette. 
#Questi sono memorizzati come numeri interi per impostazione predefinita e li convertiremo in categorie con la funzione to_categorical().


X_train <- array_reshape(X_train, c(nrow(X_train), 784))
X_train <- X_train / 255

X_test <- array_reshape(X_test, c(nrow(X_test), 784))
X_test <- X_test / 255

y_train <- to_categorical(y_train, num_classes = 10)
y_test <- to_categorical(y_test, num_classes = 10)

# MNIST è un set di dati ampio e semplice, quindi un'architettura di modello semplice dovrebbe risultare in un modello quasi perfetto.
# Avremo tre livelli nascosti rispettivamente con 256, 128 e 64 neuroni 
# e un livello di output con dieci neuroni poiché ci sono dieci classi distinte nel set di dati MNIST.

# Ogni strato lineare è seguito da dropout per evitare l'overfitting.

# Una volta dichiarato il modello, puoi utilizzare la funzione summary() per stampare la sua architettura

model <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = "relu", input_shape = c(784)) %>%
  layer_dropout(rate = 0.25) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.25) %>% 
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 10, activation = "softmax")
summary(model)
#ottimizzo il modello
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

# Ora puoi chiamare la funzione fit() per addestrare il modello. 
# Il seguente frammento addestra il modello per 50 epoche, alimentando 128 immagini alla volta:

history <- model %>% 
  fit(X_train, y_train, epochs = 50, batch_size = 128, validation_split = 0.15)

model %>%
  evaluate(X_test, y_test)

predictions <- model %>%
  predict(X_test) %>%
  k_argmax()

predictions$numpy()

# let's go fashion!
fashion_data <- dataset_fashion_mnist()
head(fashion_data$train, 1)
