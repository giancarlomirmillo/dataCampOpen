# Creo un data frame
nome <- c ("Anna", "Edoardo", "Maria" ,"Anna" ,"Pietro",  "Luca" )
anni <- c (15, 17, 16, 18, 17, 15)
altezza <- c (167, 172, 169, 177, 172, 168)

ilMioPrimoDF <- data.frame(nome, anni, altezza)

ilMioPrimoDF
mean (ilMioPrimoDF$altezza)

#Sommario
summary (ilMioPrimoDF)


# Creo la funzione moda
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

moda (ilMioPrimoDF$nome)

#leggo un dataframe 

ilMioSecondoDF <- read.csv("/Users/giancarlo/Documents/FEM-PLIN/dfschool.csv", nrows = 100 )
head (ilMioSecondoDF, 3)
ilMioSecondoDF <-  ilMioSecondoDF %>%  mutate_if(is.numeric, round, 0)

# seleziono solo le variabili numeriche
library(dplyr)
ilMioPrimoDFNumerico <- select_if(ilMioPrimoDF, is.numeric)
ilMioPrimoDFNumerico
cor (ilMioPrimoDFNumerico)

library (DataExplorer)
create_report(ilMioPrimoDF)

# Regressione (il mio primo modello)
set.seed(999)
modello.voti  <- lm(media.voti ~ ore.studio + amicalità+ coscienziosità + apertura + stabilità.emotiva+ estroversione + classe + altezza + BMI +factor (squadra), data = ilMioSecondoDF)
modello.voti


# CONTROLLO I PARAMETRI DEL MODELLO 
library (easystats)
plot(parameters(modello.voti))
model_parameters(modello.voti)

# Decision tree
library (rpart)
library (rpart.plot)
library (maptree)
set.seed(2001)
decision.tree.model <-  rpart (media.voti ~ ., ilMioSecondoDF)
#dfschool.tree.model 
printcp (decision.tree.model)
prp (decision.tree.model)

# Grafici
library(ggstatsplot)
library (ggcorrplot)

# correlazioni
ggcorrmat (ilMioSecondoDF)

#differenze fra gruppi
ggbetweenstats(ilMioSecondoDF,  BMI, media.voti,  outlier.tagging = TRUE)

 ggscatterstats(
  ilMioSecondoDF,
  x = coscienziosità,
  y = media.voti,
  label.var = classe,
) 

#grafico a torta 
ggpiestats(ilMioSecondoDF, BMI, media.voti) 

# cluster algoritmo NON supervisionato
library(ClusterR)
library(cluster)
set.seed(240) # Setting seed
kmeans.re <- kmeans(ilMioPrimoDFNumerico, centers = 3, nstart = 20)
kmeans.re$size

cm <- table(ilMioPrimoDFNumerico$altezza, kmeans.re$cluster)
cm

## Visualizing clusters

y_kmeans <- kmeans.re$cluster
clusplot(ilMioPrimoDFNumerico[, c("anni", "altezza")],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster") ,
         xlab = 'Anni',
         ylab = 'Altezza')
         
