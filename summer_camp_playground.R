# Questo è il file di accompagnamento all'introduzione al linguaggio R del corso "Summer Data Camp 2023" di PLIN + FEM
# ci sono le procedure di cui abbiamo parlato durante il corso

# creo dati
variabile_nome <- c("Anna", "Antonio", "Maria", "Giulio", "Anna")
variabile_anni <- c(14, 17, 18, 15, 17)
variabile_altezza <- c (164, 170,174, 166, 172)

ilMioPrimoDF <- data.frame(variabile_nome, variabile_anni, variabile_altezza)

# Sommario del dataframe
summary (ilMioPrimoDF)


# Posso caricare un file da una repository (GitHub in questo caso
# carico solo le prime 100 righe 
ilMioSecondoDF <- read.csv ("https://raw.githubusercontent.com/giancarlomirmillo/dataCampOpen/main/dfschool.csv", nrows =100)


# Seleziono le variabili numeriche
ilMioPrimoDFNumerico <- select_if(ilMioPrimoDF, is.numeric)
ilMioPrimoDFNumerico

#un pò di statistica di base
mean (ilMioPrimoDF$variabile_anni)
median (ilMioPrimoDF$variabile_altezza)
cor (ilMioPrimoDFNumerico)
sd (ilMioPrimoDF$variabile_altezza)

# creo la funzione Moda
Moda <- function(x) {                                                     # funzione moda
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Moda (ilMioPrimoDF$variabile_nome)

# EDA
library (DataExplorer)
create_report(ilMioSecondoDF)

# grafici
library (ggplot2)

ggplot(ilMioPrimoDF, aes(x= variabile_anni)) + 
  geom_histogram(color="steelblue", fill="white") +
  geom_vline(aes(xintercept=mean(variabile_anni)),
             color="blue", linetype="dashed", linewith=1)


ggplot(ilMioPrimoDF, aes(x=variabile_anni, y=variabile_altezza)) +
  geom_point() + # Show dots
  geom_text(
    label= ilMioPrimoDF$variabile_nome, 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T
  )

pie (ilMioPrimoDF$variabile_altezza, labels = variabile_altezza)

# modello: rappresentazione smplificata della realtà che può capire un computer


# il mio primo modello regressione lineare

regressione_modello <- lm  (media.voti ~ coscienziosità +apertura + stabilità.emotiva + estroversione + 
                               amicalità + altezza + ore.studio + BMI, data = ilMioSecondoDFNumerico)

library (jtools)
print (regressione_modello)
plot_summs (regressione_modello)

# un modello non basta! Devo provarne molti per vedere quale performa meglio
library (glmulti)
library (dplyr)

# seleziono solo le variabili numeriche
ilMioSecondoDFNumerico <- as.data.frame(
  select_if (ilMioSecondoDF, is.numeric)
)

# cerco il modello migliore
optimal_model <- glmulti (media.voti ~ coscienziosità +apertura + stabilità.emotiva + estroversione + 
                 amicalità + altezza + ore.studio + BMI, 
                 method = "g",
                 crit = aic,
                 level = 2,
                 family = gaussian,
                 fitfunction = glm,
                 data = ilMioSecondoDFNumerico, 
                 confsetsize = 100 )
print (optimal_model)
plot (optimal_model, type = "s")

# visualizzo i 3 modelli migliori
library(flextable) 
weightable(optimal_model)[1:3,] %>% 
  regulartable() %>%       # beautifying tables
  autofit()

# parametri del modello migliore 
optimal_model@objects[[2]]

#####  Programmato da ChatGPT
# il prompt : scrivi in R uno script per selezionare dal dataframe dfschool solo le variabili numeriche, 
# escludere la variabile "id", e fare un clustering con kmeans e 3 cluster e 
# visualizzare i grafici dei risultati 

# Installa e carica il pacchetto per il clustering K-means
install.packages("stats")
library(stats)

# Seleziona solo le variabili numeriche dal dataframe dfschool
numeric_vars <- ilMioSecondoDF[, sapply(ilMioSecondoDF, is.numeric)]

# Escludi la variabile "id"
numeric_vars <- numeric_vars[, !(colnames(numeric_vars) %in% c("id"))]

# Esegui il clustering K-means con 3 cluster
set.seed(123)  # Imposta un seme casuale per riproducibilità
k <- 3  # Numero di cluster
kmeans_result <- kmeans(numeric_vars, centers = k)

# Assegna le etichette di cluster ai dati
cluster_labels <- kmeans_result$cluster

# Aggiungi le etichette di cluster come variabile al dataframe originale
ilMioSecondoDF$cluster <- cluster_labels
head (ilMioSecondoDF, 10)
# Visualizza i grafici dei risultati del clustering
plot(numeric_vars, col = cluster_labels, pch = 19, main = "Clustering K-means con 3 cluster")

# Per grafici bidimensionali, puoi specificare le variabili che vuoi visualizzare
# Ad esempio, per visualizzare il clustering solo per le variabili "var1" e "var2"
plot(numeric_vars$ore.studio, numeric_vars$media.voti, col = cluster_labels, pch = 19, main = "Clustering K-means con 3 cluster")

