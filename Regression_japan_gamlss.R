library(gam)
library(ggplot2)
library(gamlss)
library(mgcv)

#### gam modell


model_gam <- gam(triggeredMag ~ s(triggeringMag, bs = "ps") + heatFlow + strainRate +
                         dip + depth +
                         strike + rake + mantleThick,
                 family = Gamma(link = "log"),
                 data = japan_reg)

summary(model_gam)

plot(x = model_gam, shade = TRUE, select = 1,
     main = "Glatter Effekt der triggernden Magnitude")





plot(formula = model_gam$residuals ~ model_gam$fitted.values,
     xlab = "gefittete Werte", ylab = "Residuen")

plot(x = japan_reg$triggeredMag, y = model_gam$fitted.values,
     xlab = "observed", ylab = "fitted")



plot(x = japan_reg$triggeredMag, y = model_gamlss_ga$fitted.values,
     xlab = "observed", ylab = "fitted")




#### gamlss modell mit mehr Variablen

model_gamlss_3 <- gamlss(formula = triggeredMag ~ pb(triggeringMag) + heatFlow + strainRate +
                                 dip + depth +
                                 strike + rake + mantleThick,
                         sigma.formula =  ~ pb(triggeringMag) + heatFlow + strainRate +
                                 dip + depth +
                                 strike + rake + mantleThick,
                        family = GA(mu.link = "log", sigma.link ="log"),
                         data = japan_reg)

plot_mu <- term.plot(object = model_gamlss_3, what = "mu", rug = TRUE,
          main = "Glatter Effekt bezüglich des erwartungswerts",
          xlab = "Triggernde Magnitude")


 plot_sigma <- term.plot(object = model_gamlss_3, what = "sigma", rug = TRUE,
          main = "Glatter Effekt bezüglich der Standardabweichung",
          xlab = "Triggernde Magnitude")



model_gamlss_3
summary(model_gamlss_3)
plot(model_gamlss_3)


ggplot(data = japan_reg, aes(x = fitted(model_gamlss_3), y = triggeredMag)) +
               geom_point() +
               theme_bw() +
               xlab("Gefittet") +
               ylab("Beobachtet") +
        coord_cartesian(xlim = c(4, 9), ylim = c(4, 9))
     
        


data2 <- data.frame(japan_reg$depth, japan_reg$strike, japan_reg$dip, japan_reg$rake, japan_reg$strainRate,
                    japan_reg$heatFlow, japan_reg$crustalThick, japan_reg$mantleThick, japan_reg$elevation)

cov(data2)
cor(data2)
