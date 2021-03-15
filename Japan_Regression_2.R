library(quantreg)
library(ggplot2)

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

