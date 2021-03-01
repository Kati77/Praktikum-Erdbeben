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

### deskriptive Analyse der Daten 
ggplot(japan_daten, aes(date, mag)) +
  geom_point()+
  theme_bw()

ggplot(japan_daten, aes(depth,mag))+
  geom_point()+
  theme_bw()
### Achtung 0 sind evtl fehlende Daten?

