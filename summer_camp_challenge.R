#creo i dati 
library (simstudy)
library (dplyr)
#defNew <- NULL
defNew <- defData (varname= "area", formula = "0.3; 0.27; 0.23; 0.20",variance = "centro; corso; borgo; viale" , dist = "categorical", link = "identity")
defNew <- defData (defNew, varname= "consumo.energia", formula = "145", dist = "normal", variance = 40)
defNew <- defData (defNew, varname= "consumo.acqua", formula = "324", dist = "normal", variance = 120)
dfambiente <- genData(320, defNew)
dfambiente <- dfambiente %>% 
  mutate(
    distanza_isole = recode (area, "corso" = 800,
                     "viale" = 500,
                     "centro" = 1200,
                     "borgo" = 300)
  )

dfambiente <- dfambiente %>% 
  mutate(
    peso_rifiuti = recode (area, "corso" = 800,
                         "viale" = 700,
                         "centro" = 500,
                         "borgo" = 1000)
  )

dfambiente <-mutate(dfambiente,
       distanza.isole = rnorm(320, distanza_isole, sd = 100)
)

dfambiente <-mutate(dfambiente,
                    peso.rifiuti = rnorm(320, peso_rifiuti, sd = 30)
)

dfambiente$distanza_isole <- NULL
dfambiente$peso_rifiuti <- NULL



cor(dfambiente$peso.rifiuti, dfambiente$distanza.isole)


dfambiente %>%
  group_by(area) %>%
  summarise(media.distanza = mean(distanza.isole), 
            media.peso.rifiuti = mean (peso.rifiuti) ,
            media.energia = mean (consumo.energia),
            media.acqua = mean (consumo.acqua))

library (DataExplorer)
create_report (dfambiente)
#salvo i dati 
write.csv(dfambiente,"/Users/giancarlo/Documents/FEM-PLIN/dfambiente.csv", row.names = FALSE)

dfambiente <- read.csv ("https://raw.githubusercontent.com/giancarlomirmillo/dataCampOpen/main/dfambiente.csv")
