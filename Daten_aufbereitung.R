# Packages einlesen

library(readxl)
library(tidyverse)
library(ggplot2)
library(gamlss)
library(quantreg)


######################################################################################################
################################### Daten Aufbereitung ###############################################
######################################################################################################

# eigenes directory angeben

setwd("C:/Users/cfisc/Documents/Statistisches Praktikum")

######################################################################################################
################################### Japan Daten ######################################################
######################################################################################################

{
  ### Tabellen einlesen 
  japan_erdbeben <- read_xlsx("Japan_Earthquakes_210515.xlsx", sheet = 1)
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
  
  
  for(i in 1:length(japan_final$eventID)){
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
    mutate(rake_modified = 0) %>%
    filter(triggeringBeben != -1) %>%
    mutate(timediffReal = triggeredT - triggeringT)
  
  ### TimediffReal stellt wahre Zeitdifferenz dar, da max timediff nachfolgend 
  ### auf 10 gesetzt
  
  japan_reg$depth <- japan_reg$depth * (-1)
  
  
  for(j in seq_along(japan_reg$rake)){
    if(japan_reg$rake[j] >= 0)  japan_reg$rake_modified[j] = 90 - abs(japan_reg$rake[j] - 90)
    if(japan_reg$rake[j] < 0) japan_reg$rake_modified[j] = - 90 + abs(japan_reg$rake[j] + 90) 
    if(japan_reg$complMagn[j] == 4) japan_reg$triggeredIsBlind[j] <- FALSE
    if(japan_reg$complMagn[j] > 4) japan_reg$triggeredIsBlind[j] <- TRUE
    if(japan_reg$timediff[j] > 10) japan_reg$timediff[j] <- 10
  }
  
  
  
  
  remove(evID, i, j,japan_erdbeben, japan_triggered)
  
  japan_regOS <- filter(japan_reg, triggeredIsBlind == FALSE)
  
  
}

#######################################################################################################
################################ California Daten #####################################################
#######################################################################################################

{
  cali_erdbeben <- read_xlsx("California_Earthquakes_210515.xlsx", sheet = 1)
  cali_triggered <- read_xlsx("California_triggerRelations_210318.xlsx", sheet = 1)
  
  ### Tabellen zusammenfügen
  cali_daten <- left_join(cali_erdbeben, cali_triggered, by = "eventID")
  
  remove(cali_erdbeben, cali_triggered)
  
  ### Clustervariable
  cali_daten<- mutate(cali_daten, Cluster = -1)
  j <- 0
  for (i in 1:length(cali_daten$eventID)){
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
    mutate(rake_modified = 0) %>%
    filter(triggeringBeben != -1) %>%
    mutate(timediffReal = triggeredT - triggeringT)
  
  cali_reg$depth <- cali_reg$depth * (-1)
  remove(evID,i)
  
  
  for(j in seq_along(cali_reg$rake)){
    if(cali_reg$rake[j] >= 0)  cali_reg$rake_modified[j] = 90 - abs(cali_reg$rake[j] - 90)
    if(cali_reg$rake[j] < 0) cali_reg$rake_modified[j] = - 90 + abs(cali_reg$rake[j] + 90) 
    if(cali_reg$complMagn[j] == 4) cali_reg$triggeredIsBlind[j] <- FALSE
    if(cali_reg$complMagn[j] > 4) cali_reg$triggeredIsBlind[j] <- TRUE
    if(cali_reg$timediff[j] > 10) cali_reg$timediff[j] <- 10
  }
  
  cali_regOS <- filter(cali_reg, triggeredIsBlind == FALSE)
}



###########################################################################################
############################# deskriptive Auswertung ######################################
###########################################################################################

######################### Verteilungsplots getriggerte Magnitude ##########################

### Japan
### Minimum auf 0 gesetzt

japan_reg$standardisierteMag <- japan_reg$triggeredMag - 4.0

ggplot() +
  geom_histogram(data = japan_reg, aes(x = standardisierteMag, y = ..density..),
                 binwidth = 0.1, color = "black", fill = "#00AD9A", alpha = 0.8) +
  geom_function(fun = dexp, args = list(rate = 1/mean(japan_reg$standardisierteMag)),
                col = "red", size = 0.75) +
  theme_bw() +
  xlab("Magnitude des getriggerten Bebens") +
  ylab("Dichte") +
  scale_y_continuous(limits = c(0, 2.6), 
                     breaks = c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5))


### Kalifornien

cali_reg$standardisierteMag <- cali_reg$triggeredMag - 2.8

ggplot() +
  geom_histogram(data = cali_reg, aes(x = standardisierteMag, y = ..density..),
                 binwidth = 0.1, color = "black", fill = "orange2", alpha = 1)+
  geom_function(fun = dexp, args = list(rate = 1/mean(cali_reg$standardisierteMag)), 
                col = "red", size = 0.75) +
  theme_bw() +
  xlab("Magnitude des getriggerten Bebens") +
  ylab("Dichte") +
  scale_y_continuous(limits = c(0, 2.6),
                     breaks = c(0.0, 0.5, 1.0, 1.5, 2.0, 2.5))



######################## boxplots triggernden und getriggerte Magnitude ##############

### Kalifornien

stack_cali <- stack(data.frame(cali_reg$triggeringMag, cali_reg$triggeredMag))
stack_cali$ind <- factor(stack_cali$ind, 
                         levels = c("cali_reg.triggeringMag", "cali_reg.triggeredMag"),
                         labels = c("triggernde Beben", "getriggerte Beben"))


ggplot(stack_cali) +
  geom_boxplot(aes(y = values, x = ind), fill = "orange2", na.rm = TRUE) +
  theme_bw() +
  coord_cartesian(ylim = c(2.5, 9)) +
  scale_y_continuous(breaks = c(3,4,5,6,7,8,9)) +
  ylab("Magnitude") +
  xlab(" ") +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 24))+
  ggtitle("Kalifornien")


### Japan

stack_japan <- stack(data.frame(japan_reg$triggeringMag, japan_reg$triggeredMag))
stack_japan$ind <- factor(stack_japan$ind, 
                          levels = c("japan_reg.triggeringMag", "japan_reg.triggeredMag"),
                          labels = c("triggernde Beben", "getriggerte Beben"))


ggplot(stack_japan) +
  geom_boxplot(aes(y = values, x = ind), fill = "cadetblue", na.rm = TRUE) +
  theme_bw() +
  coord_cartesian(ylim = c(2.5, 9)) +
  scale_y_continuous(breaks = c(3,4,5,6,7,8,9)) +
  ylab("Magnitude") +
  xlab(" ") +
  theme(axis.title = element_text(size = 22),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 24)) +
  ggtitle("Japan")


########################### Verteilung timediff ########################################

### Japan

ggplot(japan_reg) +
  geom_histogram(aes(x = timediffReal), binwidth = 1, fill = "cadetblue") +
  theme_bw() +
  xlim(0, 30) +
  ylim(0, 1000) + 
  geom_vline(xintercept = 10, color = "red") +
  ylab("Anzahl der Erdbeben") +
  xlab("Tagesdifferenz")

### Kalifornien

ggplot(cali_reg) +
  geom_histogram(aes(x = timediffReal), binwidth = 1, fill = "orange2") +
  theme_bw() +
  xlim(0, 30) +
  ylim(0, 1000) + 
  geom_vline(xintercept = 10, color = "red") +
  ylab("Anzahl der Erdbeben") +
  xlab("Tagesdifferenz")


########## Boxplots erdbeben die während Blindheitsphase stattgefunden haben ##########

### Japan

japan_raus <- filter(japan_reg, japan_reg$triggeredIsBlind == TRUE)
df_japan_raus <- data.frame(japan_raus$triggeringMag, japan_raus$triggeredMag)
sdf_japan_raus <- stack(df_japan_raus)


ggplot(data = sdf_japan_raus, aes(x = ind, y = values)) +
  geom_boxplot( fill = "cadetblue") +
  ylim(c(2.8, 9)) + 
  scale_x_discrete(labels = c("triggernde Beben", "getriggerte Beben")) +
  labs(x = " ", y = "Magnitude") + 
  theme_bw() +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 16))


### Kalifornien

cali_raus <- filter(cali_reg, cali_reg$triggeredIsBlind == TRUE)
df_cali_raus <- data.frame(cali_raus$triggeringMag, cali_raus$triggeredMag)
sdf_cali_raus <- stack(df_cali_raus)

ggplot(data = sdf_cali_raus, aes(x = ind, y = values)) +
  geom_boxplot( fill = "orange2") +
  ylim(c(2.8, 9)) + 
  scale_x_discrete(labels = c("triggernde Beben", "getriggerte Beben")) +
  labs(x = " ", y = "Magnitude") + 
  theme_bw() +
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 16))


#########################################################################################
############################### Regression ##############################################
#########################################################################################

########################### Korrelation ####################################################

### Japan 

data_korrelation_japan <- data.frame(japan_reg$depth, japan_reg$strike, japan_reg$dip, japan_reg$rake, japan_reg$strainRate,
                                     japan_reg$heatFlow, japan_reg$crustalThick, japan_reg$mantleThick, japan_reg$elevation,
                                     japan_reg$triggeringMag)

cor(data_korrelation_japan, method = "spearman")


### Japan ohne short-term incompleteness

data_korrelation_japanOS <- data.frame(japan_regOS$depth, japan_regOS$strike, japan_regOS$dip, japan_regOS$rake, japan_regOS$strainRate,
                                       japan_regOS$heatFlow, japan_regOS$crustalThick, japan_regOS$mantleThick, japan_regOS$elevation,
                                       japan_regOS$triggeringMag)

cor(data_korrelation_japanOS, method = "spearman")

### Kalifornien

data_korrelation_cali <- data.frame(cali_reg$depth, cali_reg$strike, cali_reg$dip, cali_reg$rake, cali_reg$strainRate,
                                    cali_reg$heatFlow, cali_reg$crustalThick, cali_reg$mantleThick, cali_reg$elevation,
                                    cali_reg$triggeringMag)

cor(data_korrelation_cali, method = "spearman")


### Kalifornien ohne short-term incompleteness

data_korrelation_caliOS <- data.frame(cali_regOS$depth, cali_regOS$strike, cali_regOS$dip, cali_regOS$rake, cali_regOS$strainRate,
                                      cali_regOS$heatFlow, cali_regOS$crustalThick, cali_regOS$mantleThick, cali_regOS$elevation,
                                      cali_regOS$triggeringMag)

cor(data_korrelation_caliOS, method = "spearman")


### man erkennt dass elevation teilweise eine sehr hohe Korrelation mit 
### crustal bzw. mantle thickness hat -> aus Modellen entfernt


############################### Modelle ##################################################

### Japan mit short-term incompleteness

model_gamlss_japan <- gamlss(formula = triggeredMag ~ pb(triggeringMag) + 
                               pb(heatFlow) + 
                               pb(strainRate) + pb(dip) + pb(depth) +
                               cy(rake_modified) + pb(mantleThick) + pb(crustalThick) +
                               pb(complMagn) + pb(timediff),
                             sigma.formula =  ~ pb(triggeringMag) + pb(heatFlow) +
                               pb(strainRate) + pb(dip) + pb(depth) +
                               cy(rake_modified) + pb(mantleThick) + pb(crustalThick) +
                               pb(complMagn) + pb(timediff),
                             family = GA(mu.link = "log", sigma.link ="log"),
                             data = japan_reg)

### Japan ohne short-term incompleteness

model_gamlss_japanOS <- gamlss(formula = triggeredMag ~ pb(triggeringMag) + 
                                 pb(heatFlow) + 
                                 pb(strainRate) + pb(dip) + pb(depth) +
                                 cy(rake_modified) + pb(mantleThick) + pb(crustalThick) +
                                 pb(timediff),
                               sigma.formula =  ~ pb(triggeringMag) + pb(heatFlow) +
                                 pb(strainRate) + pb(dip) + pb(depth) +
                                 cy(rake_modified) + pb(mantleThick) + pb(crustalThick) +
                                 pb(timediff),
                               family = GA(mu.link = "log", sigma.link ="log"),
                               data = japan_regOS)

### Kalifornien mit short-term incompleteness

model_gamlss_cali <- gamlss(formula = triggeredMag ~ pb(triggeringMag) + 
                              pb(heatFlow) + 
                              pb(strainRate) + pb(dip) + pb(depth) +
                              cy(rake_modified) + pb(crustalThick) +
                              pb(complMagn) + pb(timediff),
                            sigma.formula =  ~ pb(triggeringMag) + pb(heatFlow) +
                              pb(strainRate) + pb(dip) + pb(depth) +
                              cy(rake_modified) +  pb(crustalThick) +
                              pb(complMagn) + pb(timediff),
                            family = GA(mu.link = "log", sigma.link ="log"),
                            data = cali_reg)

### Kalifornien ohne short-term incompleteness

model_gamlss_caliOS <- gamlss(formula = triggeredMag ~ pb(triggeringMag) + 
                                pb(heatFlow) + 
                                pb(strainRate) + pb(dip) + pb(depth) +
                                cy(rake_modified) + pb(crustalThick) +
                                pb(timediff),
                              sigma.formula =  ~ pb(triggeringMag) + pb(heatFlow) +
                                pb(strainRate) + pb(dip) + pb(depth) +
                                cy(rake_modified) + pb(crustalThick) +
                                pb(timediff),
                              family = GA(mu.link = "log", sigma.link ="log"),
                              data = cali_regOS)


############################## GAMLSS plots ###########################################

### plot Erwartungswert 

### Einflussgröße 2: 2 Variablen in einem plot 

plot.ew <- function(Model, Einflussgroesse, splines = "pb", data, Ylim = NULL, Einflussgroesse2 = "NULL",
                     color = "darkred", Dicke = 1, xtitle = "einflussgröße") {
  se.shaded <- function(x, iy, i, ff = 2, Tms) { # konfidenzintervalle
    tms <- Tms
    tt <- ff *tms$se.fit[iy, i]
    xx <- c(x, rev(x))
    yy <- c(tms$fit[iy, i] - tt, rev(tms$fit[iy, i] + tt))
    yy <- exp(yy)
    polygon(xx, yy, col = "grey", border = "grey")
  }
  
  if(Einflussgroesse2 == "NULL") {
    variable <- data[[Einflussgroesse]]
     # tms schätzung der Koeffizienten 
    tms <- lpred(Model, what = "mu", se.fit = TRUE, type = "terms", 
                 terms = paste(splines, "(", Einflussgroesse, ")", sep = ""))
    muWerte <- exp(tms$fit) # umrechnung für die interpretation
    se <- tms$se.fit
    oo <- order(variable)
    par(mai = c(1, 1.5 , 0.2 , 0.2), omi = c(0, 0, 0, 0)) # einstellung ränder der graphik
    plot(variable[oo], muWerte[oo, 1], type = "n", xlab = xtitle, ylab = "Multiplikativer Effekt auf den\nErwartungswert der getriggerten Magnitude", 
         ylim = Ylim, col = "black", cex.lab = 1.4, cex.axis = 1.4)
    se.shaded(variable[oo], iy = oo, i = 1, Tms = tms)  
    lines(variable[oo], muWerte[oo, 1], col = color, lwd = Dicke)
    n <- length(variable)
    ylims <- range(muWerte, na.rm = TRUE)
    lines(rep.int(jitter(variable), rep.int(3, n)), rep.int(Ylim[1] + 
                                                              c(0, 0.05, NA) * diff(Ylim), n), col = color)
  }
  else { #identisch zu oben, nur für 2 Einflussgrößen
    variable1 <- data[[Einflussgroesse]]
    variable2 <- data[[Einflussgroesse2]]
    tms1 <- lpred(Model, what = "mu", se.fit = TRUE, type = "terms", 
                  terms = paste(splines, "(", Einflussgroesse, ")", sep = ""))
    tms2 <- lpred(Model, what = "mu", se.fit = TRUE, type = "terms", 
                  terms = paste(splines, "(", Einflussgroesse2, ")", sep = ""))
    muWerte1 <- exp(tms1$fit)
    muWerte2 <- exp(tms2$fit)
    oo1 <- order(variable1)
    oo2 <- order(variable2)
    par(mai = c(1, 1.5 , 0.2 , 0.2), omi = c(0, 0, 0, 0))
    plot(variable1[oo1], muWerte2[oo2, 1], type = "n", xlab = "Magnitude", ylab = "Multiplikativer Effekt auf den \nErwartungswert der getriggerten Magnitude", 
         ylim = Ylim, col = "black", cex.lab = 1.4, cex.axis = 1.4)
    n1 <- length(variable1)
    n2 <- length(variable2)
    se.shaded(variable1[oo1], iy = oo1, i = 1, Tms = tms1) 
    se.shaded(variable2[oo2], iy = oo2, i = 1, Tms = tms2) 
    lines(variable1[oo1], muWerte1[oo1, 1], col = color, lwd = Dicke)
    lines(variable2[oo2], muWerte2[oo2, 1], col = color, lwd = Dicke)
    lines(rep.int(jitter(variable1), rep.int(3, n1)), rep.int(Ylim[1] + 
                                                                c(0, 0.05, NA) * diff(Ylim), n1), col = color)
    lines(rep.int(jitter(variable2), rep.int(3, n2)), rep.int((Ylim[1] + 0.07 * diff(Ylim)) +
                                                                c(0, 0.05, NA) * diff(Ylim), n2), col = color)
  }
}



### plot Varianz


plot.v <- function(Model, Einflussgroesse, splines = "pb", data, Ylim = NULL, Einflussgroesse2 = "NULL",
                    color = "darkred", Dicke = 1, xtitle = "einflussgröße") {
  
  if(Einflussgroesse2 == "NULL") {
    variable <- data[[Einflussgroesse]]
    muWerte <- lpred(Model, what = "mu", se.fit = FALSE, type = "terms", 
                     terms = paste(splines, "(", Einflussgroesse, ")", sep = ""))
    sigmaWerte <- lpred(Model, what = "sigma", se.fit = FALSE, type = "terms", 
                        terms = paste(splines, "(", Einflussgroesse, ")", sep = ""))
    varia <- (exp(sigmaWerte) ^ 2 * exp(muWerte) ^ 2)
    oo <- order(variable)
    par(mai = c(1, 1.5 , 0.2 , 0.2), omi = c(0, 0, 0, 0))
    plot(variable[oo], varia[oo, 1], type = "n", xlab = xtitle, ylab = "Multiplikativer Effekt auf die \nVarianz der getriggerten Magnitude", 
         ylim = Ylim, col = "black", cex.lab = 1.4, cex.axis = 1.4)
    lines(variable[oo], varia[oo, 1], col = color, lwd = Dicke)
    n <- length(variable)
    ylims <- range(varia, na.rm = TRUE)
    lines(rep.int(jitter(variable), rep.int(3, n)), rep.int(Ylim[1] + 
                                                              c(0, 0.05, NA) * diff(Ylim), n), col = color)
  }
  else {
    variable1 <- data[[Einflussgroesse]]
    variable2 <- data[[Einflussgroesse2]]
    muWerte1 <- lpred(Model, what = "mu", se.fit = FALSE, type = "terms", 
                      terms = paste(splines, "(", Einflussgroesse, ")", sep = ""))
    sigmaWerte1 <- lpred(Model, what = "sigma", se.fit = FALSE, type = "terms", 
                         terms = paste(splines, "(", Einflussgroesse, ")", sep = ""))
    varia1 <- (exp(sigmaWerte1) ^ 2 * exp(muWerte1) ^ 2)
    muWerte2 <- lpred(Model, what = "mu", se.fit = FALSE, type = "terms", 
                      terms = paste(splines, "(", Einflussgroesse2, ")", sep = ""))
    sigmaWerte2 <- lpred(Model, what = "sigma", se.fit = FALSE, type = "terms", 
                         terms = paste(splines, "(", Einflussgroesse2, ")", sep = ""))
    varia2 <- (exp(sigmaWerte2) ^ 2 * exp(muWerte2) ^ 2)
    n1 <- length(variable1)
    n2 <- length(variable2)
    oo1 <- order(variable1)
    oo2 <- order(variable2)
    par(mai = c(1, 1.5 , 0.2 , 0.2), omi = c(0, 0, 0, 0))
    plot(variable1[oo1], varia1[oo1, 1], type = "n", xlab = "Magnitude", ylab = "Multiplikativer Effekt auf die \nVarianz der getriggerten Magnitude", 
         ylim = Ylim, col = "black", cex.lab = 1.4, cex.axis = 1.4)
    lines(variable1[oo1], varia1[oo1, 1], col = color, lwd = Dicke)
    lines(variable2[oo2], varia2[oo2, 1], col = color, lwd = Dicke)
    lines(rep.int(jitter(variable1), rep.int(3, n1)), rep.int(Ylim[1] + 
                                                                c(0, 0.05, NA) * diff(Ylim), n1), col = color)
    lines(rep.int(jitter(variable2), rep.int(3, n2)), rep.int((Ylim[1] + 0.07 * diff(Ylim)) +
                                                                c(0, 0.05, NA) * diff(Ylim), n2), col = color)
  }
}


### Beispiel für triggernden Magnitude Japan

plot.ew(model_gamlss_japan, Einflussgroesse = "triggeringMag", data = japan_reg,
        color = "cadetblue", xtitle = "triggernde Magnitude", Ylim = c(0.98, 1.04))

### für rake muss splines = "cy" angegeben werden, Ylim muss für jeden plot passend 
### gewählt werden


################################################################################################################
################################### Weiteres ###################################################################
################################################################################################################

### im folgenden kommen Ansätze, die wir ausprobiert hatten. Wir können nicht dafür 
### garantieren, dass die Ergebnisse korrekt sind und die nötigen Vorraussetzungen
### erfüllen

################################## clustervariable #############################################################

japan_reg$Cluster <- as.factor(japan_reg$Cluster)

model_gamlss_random <- gamlss(formula = triggeredMag ~ pb(triggeringMag) +
                                pb(depth) + pb(strike) + pb(dip) + pb(rake) +
                                pb(strainRate) + pb(heatFlow) + 
                                pb(crustalThick) + pb(mantleThick) + random(Cluster),
                              sigma.formula = ~ pb(triggeringMag) + pb(depth) +
                                pb(strike) + pb(dip) + pb(rake) + pb(strainRate) + pb(heatFlow) +
                                pb(crustalThick) + pb(mantleThick) + random(Cluster),
                              family = GA, data = japan_reg)


term.plot(object = model_gamlss_random, what = "mu", rug = TRUE,
          main = "Glatter Effekt bzgl. des Erwartungwerts",
          xlab = "Magnitude des triggerenden Erdbebens")

term.plot(object = model_gamlss_random, what = "sigma", rug = TRUE,
          main = "Glatter Effekt bzgl. der Standardabweichung")

model_gamlss_re <- gamlss(formula = triggeredMag ~ pb(triggeringMag) +
                            pb(depth) + pb(strike) + pb(dip) + pb(rake) +
                            pb(strainRate) + pb(heatFlow) + 
                            pb(crustalThick) + pb(mantleThick) + re(random = ~1|Cluster),
                          sigma.formula = ~ pb(triggeringMag) + pb(depth) +
                            pb(strike) + pb(dip) + pb(rake) + pb(strainRate) + pb(heatFlow) +
                            pb(crustalThick) + pb(mantleThick) + re(random = ~1|Cluster),
                          family = GA, data = japan_reg)

term.plot(object = model_gamlss_re, what = "mu", rug = TRUE,
          main = "Glatter Effekt bzgl. des Erwartungswertes",
          xlab = "Magnitude des triggerenden Erdbebens")

term.plot(object = model_gamlss_re, what = "sigma", rug = TRUE,
          main = "Glatter Effekt bzgl. der Standardabweichung")

######################## Quantilsregression ################################################


model_1 <- rq(triggeredMag ~ triggeringMag, data = japan_reg)
model_1
summary(model_1)

## regression 10 Quantile

q10 <- seq(0.1, 0.9, by=0.1)

model_2 <- rq(triggeredMag ~ triggeringMag, tau = q10, data = japan_reg)
summary(model_2)


ggplot(japan_reg, aes(triggeringMag, triggeredMag))+
  geom_point(size = 1, colour = "grey70")+
  geom_quantile(method = "rq", quantiles = q10, colour = "red")+
  geom_smooth(method = 'lm', colour = "blue", 
              se = FALSE, linetype = "11") 

plot(summary(model_2))

## regression 10 quantile ohne is.blind=TRUE

japan_reg_blind <- filter(japan_reg, isBlind != TRUE)

reg_blind <- rq(triggeredMag ~ triggeringMag, tau = q10, data = japan_reg_blind)

plot(summary(reg_blind))

## regressoin 100 quantile

q100 <- seq(0.01, 0.99, by = 0.01)

model_3 <- rq(triggeredMag ~ triggeringMag, tau = q100, data = japan_reg )

plot(summary(model_3))


