

###############################################################################
############################## momios.R #######################################

#Instalación de paqueterías pendientes
install.packages("remotes") 
install_github("cran/fbRanks")

#Llamar librerías
library(remotes)
library(fbRanks)
library(dplyr)
library(ggplot2)


############### inicia procesamiento data

# Colocar el directorio de trabajo según corresponda

#setwd("~/Documents/CURSOS/DATA SCIENCE/BEDU - SANTANDER/FASE 2 - ESTADÍSTICA Y R/PROYECTO FINAL/datasets")
#getwd()

# Descarga de archivos
# https://www.football-data.co.uk/spainm.php

#mejora descargando la data completa en repositorio

u1011 <- "https://www.football-data.co.uk/mmz4281/1011/SP1.csv"
u1112 <- "https://www.football-data.co.uk/mmz4281/1112/SP1.csv"
u1213 <- "https://www.football-data.co.uk/mmz4281/1213/SP1.csv"
u1314 <- "https://www.football-data.co.uk/mmz4281/1314/SP1.csv"
u1415 <- "https://www.football-data.co.uk/mmz4281/1415/SP1.csv"
u1516 <- "https://www.football-data.co.uk/mmz4281/1516/SP1.csv"
u1617 <- "https://www.football-data.co.uk/mmz4281/1617/SP1.csv"
u1718 <- "https://www.football-data.co.uk/mmz4281/1718/SP1.csv"
u1819 <- "https://www.football-data.co.uk/mmz4281/1819/SP1.csv"
u1920 <- "https://www.football-data.co.uk/mmz4281/1920/SP1.csv"



download.file(url = u1011, destfile ="SP1-1011.csv", mode = "wb")
download.file(url = u1112, destfile ="SP1-1112.csv", mode = "wb")
download.file(url = u1213, destfile ="SP1-1213.csv", mode = "wb")
download.file(url = u1314, destfile ="SP1-1314.csv", mode = "wb")
download.file(url = u1415, destfile ="SP1-1415.csv", mode = "wb")
download.file(url = u1516, destfile ="SP1-1516.csv", mode = "wb")
download.file(url = u1617, destfile ="SP1-1617.csv", mode = "wb")
download.file(url = u1718, destfile ="SP1-1718.csv", mode = "wb")

# Lectura de datos


lista <- lapply(list.files(path = "/datasets"), read.csv)

# Procesamiento de datos

lista <- lapply(lista, select, Date:FTAG, BbMx.2.5:BbAv.2.5.1)
d1018 <- do.call(rbind, lista)


download.file(url = u1819, destfile ="SP1-1819.csv", mode = "wb")
d1819 <- read.csv("SP1-1819.csv")
d1819S <- select(d1819, Date:FTAG, BbMx.2.5:BbAv.2.5.1)


download.file(url = u1920, destfile ="SP1-1920.csv", mode = "wb")
d1920 <- read.csv("SP1-1920.csv")
d1920S <- select(d1920, Date:FTAG, Max.2.5:Avg.2.5.1, -Time)

#Renombrar columnas

d1018S <- rename(d1018,  Max.2.5.O = BbMx.2.5, 
                 Avg.2.5.O = BbAv.2.5, 
                 Max.2.5.U = BbMx.2.5.1,
                 Avg.2.5.U = BbAv.2.5.1)

d1819S <- rename(d1819S,  Max.2.5.O = BbMx.2.5, 
                 Avg.2.5.O = BbAv.2.5, 
                 Max.2.5.U = BbMx.2.5.1,
                 Avg.2.5.U = BbAv.2.5.1)

d1920S <- rename(d1920S,  Max.2.5.O = Max.2.5, 
                 Avg.2.5.O = Avg.2.5, 
                 Max.2.5.U = Max.2.5.1,
                 Avg.2.5.U = Avg.2.5.1)

# Ordenamos las columnas, para que correspondan con d1920S
d1018S <- select(d1018S, colnames(d1920S))
d1018S <- mutate(d1018S, Date = as.Date(Date, format = "%d/%m/%y"))

# Se agrega la temporada 1820 y 1920
d1820S <- rbind(d1819S,d1920S)
d1820S <- mutate(d1820S, Date = as.Date(Date, format = "%d/%m/%Y"))

d1020S <- rbind(d1018S,d1820S)

# Arreglamos las fechas


# Renombramos
d1020S <- rename(d1020S, date = Date, home.team = HomeTeam, home.score = FTHG, away.team = AwayTeam, away.score = FTAG)
#Se cambia el orden de las columnas
d1020S <- select(d1020S, date, home.team, home.score, away.team, away.score:Avg.2.5.U)
data<-d1020S
############### termina procesamiento data

# Data frames de partidos y equipos

#escribir un csv, solo se hace con las primeras 5 cols de data, las últimas 4 solo se usan para los fors de momios al final

write.csv(select(data, date:away.score), "match.data.csv", row.names = FALSE)
df <- create.fbRanks.dataframes(scores.file = "match.data.csv")
#crea 2 df nuevas a partir de df con los equipos por un lado, y por otro las primeras 5 de data
teams <- df$teams; scores <- df$scores


head(teams, n = 2L); dim(teams); head(scores, n = 2L); dim(scores)

# Conjuntos iniciales de entrenamiento y de prueba

f <- scores$date # Fechas de partidos
fu <- unique(f) # Fechas sin repetición
Ym <- format(fu, "%Y-%m") # Meses y años
Ym <- unique(Ym) # Meses y años sin repetir
#Ym = meses en los que hubo partidos, lenght 101
#Ym[15] = 2011-12
places <- which(Ym[15]==format(scores$date, "%Y-%m")) # Consideramos partidos de 15 meses para comenzar a ajustar el modelo
#places entrega secuencia del 511 al 540 ¿? length = 30
#places se pregunta cuáles fechas en el df scores (que contienen día) son parte del mes "2011-12" y entrega esos registros
ffe <- scores$date[max(places)] # Fecha final conjunto de entrenamiento
#ffe = 2011-12-18, entrega la última fecha del mes 2011-12

# Consideraremos partidos de 15 meses para comenzar a ajustar el modelo. Así, nuestro primer conjunto de entrenamiento consiste de datos de partidos hasta el `r ffe` 
#se separará la data en 2: conjunto entrenamiento será hasta el ffe, lo demás será para probar
#dim(train)=540 ; dim(test) =3260

train <- scores %>% filter(date <= ffe)
test <- scores %>% filter(date > ffe)
#train da los datos del inicio hasta ffe 
head(train, n = 1); tail(train, n = 1)
#test da los datos después de ffe (2012-01-07) hasta el final
head(test, n = 1); tail(test, n = 1)

# Primer ajuste del modelo

traindate <- unique(train$date) #entrega las fechas únicas del conjunto train
testdate <- unique(test$date) #entrega las fechas únicas del conjunto test

#traindate[1] = 2010-08-28
#traindate[length(traindate)] = 2011-12-18

#ranks.teams parte de la librería fbRanks, crea el ranking necesita scores de la manera que está (5 cols), teams contiene lo que se creó en teams
ranks <- rank.teams(scores = scores, teams = teams, 
                    min.date = traindate[1], 
                    max.date = traindate[length(traindate)])

# Primera predicción con la primera fecha del conjunto de prueba (test)
pred <- predict(ranks, date = testdate[1])
phs <- pred$scores$pred.home.score # predicted home score
pas <- pred$scores$pred.away.score # predicted away score
pht <- pred$scores$home.team # home team in predictions
pat <- pred$scores$away.team # away team in predictions

#entrega la predicción de la primera fecha en el df scores
#pred$scores


# Continuar ajustando y prediciendo
#una vez que se ve que si jala predict con la primera test date se vacían las variables phs,pas,pht,pat 
phs <- NULL; pas <- NULL; pht <- NULL; pat <- NULL

#for para crear predicciones con un ranking variable que toma el date que se entrega de scores y agarra las siguientes 169 fechas
#en cada ciclo se predice con los rankings cambiantes los partidos de la fecha inicial hasta 170 después
# i va de 1 a 1066, 1236-170
for(i in 1:(length(unique(scores$date))-170)){
  ranks <- rank.teams(scores = scores, teams = teams, 
                      min.date = unique(scores$date)[i], 
                      max.date = unique(scores$date)[i+170-1], 
                      silent = TRUE,
                      time.weight.eta = 0.0005)
  pred <- predict(ranks, date = unique(scores$date)[i+170],
                  silent = TRUE)
#se crea un vector que contendrá todas las iteraciones de predicciones
  phs <- c(phs, pred$scores$pred.home.score) # predicted home score
  pas <- c(pas, pred$scores$pred.away.score) # predicted away score
  pht <- c(pht, pred$scores$home.team) # home team in predictions
  pat <- c(pat, pred$scores$away.team) # away team in predictions
}
#head(phs)
#head(pas)
#head(pht)
#head(pat)

# Eliminamos NA's

#checar si hay alguna predicción que sea error
buenos <- !(is.na(phs) | is.na(pas)) # 
#actualizar las predicciones tomando solo los registros buenos
phs <- phs[buenos] # predicted home score
pas <- pas[buenos] # predicted away score
pht <- pht[buenos] # home team in predictions
pat <- pat[buenos] # away team in predictions

#unique(scores$date)[171] = 2012-03-05
#toma la data real de fechas mayores a 2012-03-05, creo que esta fecha es la última que no se creó predicción
momio <- data %>% filter(date >= unique(scores$date)[171]) # momios conjunto de prueba
#¿porqué toma lo de buenos si buenos viene de las predicciones (scores) y no de data?
momio <- momio[buenos,]
#se asegura que se hayan tomado exactamente los mismo equipos en ese orden (mean es para revisar que todo sea 1)
mean(pht == momio$home.team); mean(pat == momio$away.team)

#proporción de partidos con más de 2.5 goles tanto en predicción como en los momios   
mean(phs + pas > 2.5 & momio$home.score + momio$away.score > 2.5)
#proporción de partidos con menos de 2.5 goles tanto en predicción como en los momios 
mean(phs + pas < 2.5 & momio$home.score + momio$away.score < 2.5)

#vectores de goles
hs <- momio$home.score
as <- momio$away.score


# Probabilidades condicionales

mean(phs + pas > 3) # proporción de partidos con más de tres goles según el modelo
#proporción de partidos con más de 3 goles en tanto en predicciones como momios entre proporción de predicciones con más de 3 goles 
mean(phs + pas > 3 & hs + as > 2.5)/mean(phs + pas > 3) 
# probabilidad condicional estimada de ganar en over 2.5
mean(phs + pas < 2.1) # proporción de partidos con menos de 2.1 goles según el modelo
mean(phs + pas < 2.1 & hs + as < 2.5)/mean(phs + pas < 2.1) 
# probabilidad condicional estimada de ganar en under 2.5

# Juegos con momios máximos

cap <- 50000; g <- NULL

#For que simula un serie de apuestas, se apuesta 1000 por partido dependiendo 
#se decide over o under en función de los goles de las predicciones y luego se compara contra goles reales
for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64*(momio$Max.2.5.O[j]) > 1)){   #.64 prob cond de ganar en over 2.5
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Max.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58*(momio$Max.2.5.U[j]) > 1)){    #.58 prob cond de ganar en under 2.5 
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Max.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

#cap termina en 53090, terminamos con 3090 de ganancia
(cap/50000-1)*100
#se obtiene una ganancia de 6.18% en el escenario de apostar en el momio máximo en cada apuesta

# Escenario con momios máximos

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Realizando una secuencia de juegos") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas 
p

# Escenario con momios promedio

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Avg.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Avg.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Avg.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Avg.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

(cap/50000-1)*100
#cap termina en 30310, terminamos perdiendo 39.38%
#se obtiene una pérdida de 39.38% en el escenario de apostar en el momio promedio (a todos) en cada apuesta

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Realizando una secuencia de juegos") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas 
p


###############################################################################
############################## momios.R #######################################
