# Set Working Directory:
rm(list=ls()) #clears workspace

#load libraries
library(tidyverse)

#FECUNDITY
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

#EGGS PER BUNDLE
# load data 
eggs.per.bundle <-read.csv('Data/Eggs.per.bundle.counts.csv', header=T, sep=",")

eggs.per.bundle <- eggs.per.bundle %>%
  group_by(Sample_ID, Origin ) %>%
  summarise(Mean.eggs = mean(Num.Eggs))

pdf("Output/eggs.per.bundle.pdf")
eggs.per.bundle %>%
  ggplot(aes(x = Origin, y = Mean.eggs, color = Origin)) +
  labs(x = "", y = "Eggs per Bundle") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic()
dev.off()

#stats
t.test(Mean.eggs~Origin, data = eggs.per.bundle) #Statistically significant if p-value <0.05


#EGG SIZE
# load data 
egg.size <-read.csv('Data/2019_October_Egg_Size.csv', header=T, sep=",")

egg.size$ratio <- egg.size$Diameter1_long/egg.size$Diameter2_short

egg.diam <- egg.size %>%
  group_by(Sample_ID, Origin ) %>%
  summarise(Mean.size = mean(Diameter1_long))

egg.ratio <- egg.size %>%
  group_by(Sample_ID, Origin ) %>%
  summarise(Mean.ratio = mean(ratio))

pdf("Output/egg.diameter.pdf")
egg.diam %>%
  ggplot(aes(x = Origin, y = Mean.size, color = Origin)) +
  labs(x = "", y = "Egg Diameter (mm)") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic()
dev.off()

pdf("Output/egg.ratio.pdf")
egg.ratio %>%
  ggplot(aes(x = Origin, y = Mean.ratio, color = Origin)) +
  labs(x = "", y = "Egg Ratio (Length:Width)") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic()
dev.off()

#Stats
t.test(Mean.size ~ Origin, data = egg.diam) #Statistically significant if p-value <0.05
t.test(Mean.ratio ~ Origin, data = egg.ratio) #Statistically significant if p-value <0.05