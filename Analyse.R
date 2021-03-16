###########################################Erdbeben######################################################
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


### extra Spalte mit Magnitude des triggerenden Erdbeben
japan_daten <- mutate(japan_daten,triggeringMag = 0)

for(i in 1:length(japan_daten$triggeredFrom)){
  if(japan_daten$triggeredFrom[i] == -1)  japan_daten$triggeringMag[i] <- -1
  else {
    evID <- japan_daten$triggeredFrom[i]
    japan_daten$triggeringMag[i] <- japan_daten$mag[evID]
  }
}
remove(evID,i, japan_erdbeben, japan_triggered)

### Clustervariable
japan_daten <- mutate(japan_daten, Cluster = -1)
j <- 0
for (i in 1:length(japan_daten$eventID)){
 if(japan_daten$triggeredFrom[i] != -1){
   IDtriggering <- japan_daten$triggeredFrom[i]
   
    if(japan_daten$Cluster[IDtriggering] != -1){
      Clusternr <- japan_daten$Cluster[IDtriggering]
      japan_daten$Cluster[i] <- Clusternr
    }
    if(japan_daten$Cluster[IDtriggering] == -1) {
      j <- j + 1
      japan_daten$Cluster[IDtriggering] <- j
      japan_daten$Cluster[i] <- j
    }
 }
}

### Spalte umbennen
japan_daten <- japan_daten %>%
  rename(triggeredMag = mag)

### Erstellen von Datensatz mit nur Beben die auch getriggert werden
japan_reg <- filter(japan_daten, triggeringMag != -1)

remove(Clusternr, i, IDtriggering, j)

