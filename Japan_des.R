###+Kerndichteschätzer
ggplot(japan_reg, aes(triggeredMag)) +
  geom_density()

ggplot(japan_reg, aes(triggeringMag)) +
  geom_density()

### Differenz der zsichen getriggeter und triggender Magnitude
ggplot(japan_reg, aes(DifferenzMag)) +
  geom_density() +
  theme_bw()

ggplot(japan_reg, aes(DifferenzMag)) +
  geom_histogram()+
  theme_bw()

ggplot(japan_reg, aes(DifferenzMag))+
  geom_bar()+
  theme_bw()

### Plot zwischen triggender und getriggeter Magnitude
ggplot(japan_reg, aes(triggeringMag, triggeredMag))+
  geom_jitter() +
  theme_bw()
# mit lm Schätzung
ggplot(japan_reg, aes(triggeringMag, triggeredMag))+
  geom_jitter() +
  theme_bw() +
  geom_smooth(method = "lm")
