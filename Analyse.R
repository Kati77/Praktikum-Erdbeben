###########################################Erdbeben######################################################
install.packages("readxl")
library(readxl)
library(tidyverse)
library(ggplot2)
############## Daten einlesen #########################################################################
pfad <- setwd("C:/Users/FR/Documents/Erdbeben/StatPra2021")
#### Tabellen einlesen 
japan_erdbeben <- read_xlsx("Japan_Earthquakes_210228.xlsx", sheet = 1)
japan_triggered <- read_xlsx("Japan_triggerRelations_210228.xlsx", sheet = 1)

### Tabellen zusammenfügen
japan_triggered$eventID <- japan_triggered$evID 
japan_triggered <- japan_triggered[,-1]
japan_daten <- left_join(japan_erdbeben, japan_triggered, by = "eventID")


### Spalte erstellen, wo man festlegt welches Erdbeben welches triggert
japan_daten <- mutate(japan_daten, Triggering = -1)
for (i in 1:length(japan_daten$eventID)) {
  if(japan_daten$triggeredFrom[i] != -1){
    evID1 <- japan_daten$triggeredFrom[i]
    evID2 <- japan_daten$eventID[i]
    if(japan_daten$Triggering[evID1] == -1){
    japan_daten$Triggering[japan_daten$eventID == evID1] <- evID2
    }
    else japan_daten <- 
      rbind(japan_daten, 
            data.frame(eventID = evID1, date = japan_daten$date[evID1],
                       t = japan_daten$t[evID1], lon = japan_daten$lon[evID1],
                       lat = japan_daten$lat[evID1], mag = japan_daten$mag[evID1],
                       depth = japan_daten$depth[evID1], strike = japan_daten$strike[evID1],
                       dip = japan_daten$dip[evID1], rake = japan_daten$rake[evID1], 
                       strainRate = japan_daten$strainRate[evID1], heatFlow = japan_daten$heatFlow[evID1],
                       crustalThick = japan_daten$crustalThick[evID1], 
                       mantleThick = japan_daten$mantleThick[evID1], elevation = japan_daten$elevation[evID1],
                       isBlind = japan_daten$isBlind[evID1], distanceMeasure = japan_daten$distanceMeasure[evID1],
                       triggeredFrom = japan_daten$triggeredFrom[evID1], 
                       Triggering = evID2))
  }
}

### Einzelbeben aus Datensatz rausnehmen
japan_triggering <- filter(japan_daten, Triggering != -1)
japan_triggered <- filter(japan_daten, triggeredFrom != -1)

japan_ohneEB <- union(japan_triggered, japan_triggering) %>%
  distinct()

### sortieren der Daten nach EventID
japan_ohneEB <- japan_ohneEB[order(japan_ohneEB$eventID), ]

remove(japan_erdbeben, japan_triggered, japan_triggering, evID1, evID2, i)

### evtl. extra Spalte mit Magnitude dem triggerendem Erdbeben
japan_ohneEB <- mutate(japan_ohneEB,triggeringMag = 0)

for(i in 1:length(japan_ohneEB$triggeredFrom)){
  if(japan_ohneEB$triggeredFrom[i] == -1)  japan_ohneEB$triggeringMag[i] <- -1
  else {
    evID <- japan_ohneEB$triggeredFrom[i]
    japan_ohneEB$triggeringMag[i] <- japan_daten$mag[evID]
  }
}

remove(evID,i)

