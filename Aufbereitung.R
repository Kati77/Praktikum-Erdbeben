######################################################################################################
################################### Daten Aufbereitung ###############################################
######################################################################################################

library(readxl)
library(tidyverse)
library(ggplot2)

setwd("C:/Users/FR/Documents/Erdbeben/StatPra2021")

######################################################################################################
################################### Japan Daten ######################################################
######################################################################################################
{
### Tabellen einlesen 
japan_erdbeben <- read_xlsx("Japan_Earthquakes_210228.xlsx", sheet = 1)
japan_triggered <- read_xlsx("Japan_triggerRelations_210318.xlsx", sheet = 1)

### Tabellen zusammenfügen
japan_triggered$eventID <- japan_triggered$evID 
japan_triggered <- japan_triggered[,-1]
japan_daten <- left_join(japan_erdbeben, japan_triggered, by = "eventID")

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
remove(Clusternr, i, IDtriggering, j)

### Datensatz umstellen
japan_final <- japan_daten %>%
  mutate(triggeringMag = 0) %>%
  mutate(triggeringT = 0)


for(i in 1:length(japan_final$triggeredFrom)){
  if(japan_final$triggeredFrom[i] != -1){
    evID <- japan_final$triggeredFrom[i]
    japan_final$triggeringT[i] <- japan_daten$t[evID]
    japan_final$lon[i] <- japan_daten$lon[evID]
    japan_final$lat[i] <- japan_daten$lat[evID]
    japan_final$depth[i] <- japan_daten$depth[evID]
    japan_final$strike[i] <- japan_daten$strike[evID]
    japan_final$dip[i] <- japan_daten$dip[evID]
    japan_final$rake[i] <- japan_daten$rake[evID]
    japan_final$strainRate[i] <- japan_daten$strainRate[evID]
    japan_final$heatFlow[i] <- japan_final$heatFlow[evID]
    japan_final$crustalThick[i] <- japan_daten$crustalThick[evID]
    japan_final$mantleThick[i] <- japan_daten$mantleThick[evID]
    japan_final$elevation[i] <- japan_daten$elevation[evID]
    japan_final$triggeringMag[i] <- japan_daten$mag[evID]
  } 
}

japan_reg <- japan_final%>%
  rename(triggeredMag = mag) %>%
  rename(triggeringBeben = triggeredFrom) %>%
  rename(triggeredBeben = eventID) %>% 
  rename(triggeredDate = date) %>%
  rename(triggeredT = t) %>%
  rename(triggeredIsBlind = isBlind) %>%
  rename(triggeredDistanceMeasure = distanceMeasure) %>%
  mutate(timediff = triggeredT - triggeringT) %>%
  filter(triggeringBeben != -1)

remove(evID,i, japan_erdbeben, japan_triggered)

}

#######################################################################################################
################################ California Daten #####################################################
#######################################################################################################

{
cali_erdbeben <- read_xlsx("California_Earthquakes_210318.xlsx", sheet = 1)
cali_triggered <- read_xlsx("California_triggerRelations_210318.xlsx", sheet = 1)

### Tabellen zusammenfügen
cali_daten <- left_join(cali_erdbeben, cali_triggered, by = "eventID")

remove(cali_erdbeben, cali_triggered)

### Clustervariable
cali_daten<- mutate(cali_daten, Cluster = -1)
j <- 0
for (i in 1:length(ccali_daten$eventID)){
  if(cali_daten$triggeredFrom[i] != -1){
    IDtriggering <- cali_daten$triggeredFrom[i]
    
    if(cali_daten$Cluster[IDtriggering] != -1){
      Clusternr <- cali_daten$Cluster[IDtriggering]
      cali_daten$Cluster[i] <- Clusternr
    }
    if(cali_daten$Cluster[IDtriggering] == -1) {
      j <- j + 1
      cali_daten$Cluster[IDtriggering] <- j
      cali_daten$Cluster[i] <- j
    }
  }
}
remove(Clusternr, i, IDtriggering, j)

### Datensatz umstellen
cali_final <- cali_daten %>%
  mutate(triggeringMag = 0) %>%
  mutate(triggeringT = 0)


for(i in 1:length(cali_final$triggeredFrom)){
  if(cali_final$triggeredFrom[i] != -1){
    evID <- cali_final$triggeredFrom[i]
    cali_final$triggeringT[i] <- cali_daten$t[evID]
    cali_final$lon[i] <- cali_daten$lon[evID]
    cali_final$lat[i] <- cali_daten$lat[evID]
    cali_final$depth[i] <- cali_daten$depth[evID]
    cali_final$strike[i] <- cali_daten$strike[evID]
    cali_final$dip[i] <- cali_daten$dip[evID]
    cali_final$rake[i] <- cali_daten$rake[evID]
    cali_final$strainRate[i] <- cali_daten$strainRate[evID]
    cali_final$heatFlow[i] <- cali_final$heatFlow[evID]
    cali_final$crustalThick[i] <- cali_daten$crustalThick[evID]
    cali_final$mantleThick[i] <- cali_daten$mantleThick[evID]
    cali_final$elevation[i] <- cali_daten$elevation[evID]
    cali_final$triggeringMag[i] <- cali_daten$mag[evID]
  } 
}

cali_reg <- cali_final%>%
  rename(triggeredMag = mag) %>%
  rename(triggeringBeben = triggeredFrom) %>%
  rename(triggeredBeben = eventID) %>% 
  rename(triggeredDate = date) %>%
  rename(triggeredT = t) %>%
  rename(triggeredIsBlind = isBlind) %>%
  rename(triggeredDistanceMeasure = distanceMeasure) %>%
  mutate(timediff = triggeredT - triggeringT) %>%
  filter(triggeringBeben != -1)

remove(evID,i, cali_erdbeben, cali_triggered)

}
