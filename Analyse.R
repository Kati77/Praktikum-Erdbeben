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

### extra Spalte mit Magnitude dem triggerendem Erdbeben
japan_daten <- mutate(japan_daten,triggeringMag = 0)

for(i in 1:length(japan_daten$triggeredFrom)){
  if(japan_daten$triggeredFrom[i] == -1)  japan_daten$triggeringMag[i] <- -1
  else {
    evID <- japan_daten$triggeredFrom[i]
    japan_daten$triggeringMag[i] <- japan_daten$mag[evID]
  }
}
remove(evID,i)
### Einzelbeben aus Datensatz rausnehmen
japan_triggering <- filter(japan_daten, Triggering != -1)
japan_triggered <- filter(japan_daten, triggeredFrom != -1)

japan_ohneEB <- union(japan_triggered, japan_triggering) %>%
  distinct()

### sortieren der Daten nach EventID
japan_ohneEB <- japan_ohneEB[order(japan_ohneEB$eventID), ]

remove(japan_erdbeben, japan_triggered, japan_triggering, evID1, evID2)
### Erdbeben die nichts triggeren oder nicht getriggertet werden gleich NA setzen
japan_daten$triggeredFrom[japan_daten$triggeredFrom == -1] <- NA 
japan_daten$triggeringMag[japan_daten$triggeringMag == -1] <- NA

### Differnenz der Magnituden Werte erstellen
japan_daten <- mutate(japan_daten, DifferenzMag = triggeringMag - mag)
japan_reg <- rename( japan_daten,triggeredMag = mag) 

### Differnenz der Magnituden Werte erstellen
japan_daten <- mutate(japan_daten, DifferenzMag = triggeringMag - mag)
