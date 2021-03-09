library(quantreg)
library(ggplot2)

model_1 <- rq(triggeredMag ~ triggeringMag, data = japan_reg)
model_1
summary(model_1)

## regression 4 Quantile

model_2 <- rq(triggeredMag ~ triggeringMag, tau = c(0.25, 0.5, 0.75), data = japan_reg)
summary(model_2)

## plot quantreg vs. lm


ggplot(japan_reg, aes(triggeringMag, triggeredMag))+
  geom_point(size=1, colour="grey70")+
  geom_quantile(method="rq", colour="red")+
  geom_smooth(method='lm', colour="blue", 
              se=FALSE, linetype="11") 

## Daten nach aufsteigender triggernder magnitude ordnen

japan_reg <- japan_reg[order(japan_reg$triggeringMag), ]

## Unterteilung des Datensatzes in quantile

japan_quantil_1 <- japan_reg[1:2581, ]
japan_quantil_2 <- japan_reg[2582:5161, ]
japan_quantil_3 <- japan_reg[5162:7741, ]
japan_quantil_4 <- japan_reg[7742:10322, ]

ggplot(japan_quantil_1, aes(triggeringMag, triggeredMag))+
  geom_point()+
  theme_bw()

ggplot(japan_quantil_4, aes(triggeringMag, triggeredMag))+
  geom_point()+
  theme_bw()

