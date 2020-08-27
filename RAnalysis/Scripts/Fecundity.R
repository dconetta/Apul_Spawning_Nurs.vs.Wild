# Set Working Directory:
rm(list=ls()) #clears workspace

#load libraries
library(tidyverse)

#standard curve to get SA
wax.stds<-read.csv('Data/wax.standards_1.csv', header=T, sep=",")
wax.stds$delta.mass.g <- wax.stds$weight2.g - wax.stds$weight1.g
plot(surface.area.cm2~delta.mass.g, data = wax.stds)
model <- lm(surface.area.cm2~delta.mass.g , data = wax.stds)
abline(model, col = "red")
summary(model)

SA<-read.csv('Data/Apul_Wax_Data.csv', header=T, sep=",")
SA$surface.area.cm2 <- (model$coefficients[2]*(SA$waxedmass.g - SA$mass.g)) + model$coefficients[1]


# load data 
Oct.fec <- read.csv('Data/2019_October_Fecundity.csv', header=T, sep=",")
Oct.fec <- Oct.fec %>%
  group_by(Sample_ID, Origin ) %>%
  summarise(tot.eggs = sum(Total_number))

Oct.fec <- left_join(Oct.fec, SA, by="Sample_ID")

Oct.fec$fecundity <-Oct.fec$tot.eggs/Oct.fec$surface.area.cm2

pdf("Output/Fecundity.pdf")
Oct.fec %>%
  ggplot(aes(x = Origin, y = fecundity, color = Origin)) +
  labs(x = "", y = "Fecundity eggs/cm2") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic()
dev.off()

#stats
t.test(fecundity~Origin, data = Oct.fec) #Statistically significant if p-value <0.05

