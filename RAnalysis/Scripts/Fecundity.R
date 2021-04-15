# Set Working Directory:
rm(list=ls()) #clears workspace

#load libraries
library(tidyverse)
library(Hmisc)
library(lubridate)
library(tidyr)
library(ggpubr)
library(dplyr)

#2019__________________________________________________________________________________________
#standard curve to get SA
setwd("C:/Users/dcone/Documents/Git-Hub/Apul_Spawning_Nurs.vs.Wild/RAnalysis")
wax.stds<-read.csv('Data/Oct_2019/wax.standards_1.csv', header=T, sep=",")
wax.stds$delta.mass.g <- wax.stds$weight2.g - wax.stds$weight1.g
plot(surface.area.cm2~delta.mass.g, data = wax.stds)
model <- lm(surface.area.cm2~delta.mass.g , data = wax.stds)
abline(model, col = "red")
summary(model)

SA<-read.csv('Data/Oct_2019/Apul_Wax_Data.csv', header=T, sep=",")
SA$surface.area.cm2 <- (model$coefficients[2]*(SA$waxedmass.g - SA$mass.g)) + model$coefficients[1]

SA <- SA %>%
  mutate(sample_id = ï..sample_id)

# load data 
Oct.fec <- read.csv('Data/Oct_2019/2019_October_Fecundity.csv', header=T, sep=",")
Oct.fec <- Oct.fec %>%
  group_by(sample_id, Origin ) %>%
  summarise(tot.eggs = sum(Total_number))

Oct.fec <- left_join(Oct.fec, SA, by="sample_id")

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

#2020_________________________________________________________________________________

setwd("C:/Users/dcone/Documents/Git-Hub/Apul_Spawning_Nurs.vs.Wild/RAnalysis")
wax.stds2<-read.csv('Data/Oct_2020/wax.standards_1.csv', header=T, sep=",")
wax.stds2$delta.mass.g <- wax.stds2$weight2.g - wax.stds2$weight1.g
#calculate the surface area
wax.stds2 <- wax.stds2 %>%
  mutate(surface.area.cm2 = (4*pi*(diameter/2)^2))

plot(surface.area.cm2~delta.mass.g, data = wax.stds2)
model <- lm(surface.area.cm2~delta.mass.g , data = wax.stds2)
abline(model, col = "red")
summary(model)

SA_2<-read.csv('Data/Oct_2020/Fecundity_Surface_Area.csv', header=T, sep=",") %>%
  filter(species == "A.pulchra") %>%
  filter(origin=="Mahana" | origin=="Nursery") %>%
  filter(treatment == "Wild" | treatment == "Nursery")

SA_2$surface.area.cm2 <- (model$coefficients[2]*(SA_2$weight2.g - SA_2$weight1.g)) + model$coefficients[1]
 

Oct.fec_1 <- read.csv('Data/Oct_2020/2020_October_Fecundity_counts.csv', header=T, sep=",")
Oct.fec_1.0 <- Oct.fec_1 %>%
  filter(!is.na(Total_number)) %>%
  filter(Species == "A. pulchra") %>%
  filter(Origin == "Wild" | Origin == "Nursery") %>%
  group_by(sample_id, Origin) %>%
  summarise(tot.eggs = sum(Total_number))

Oct.fec_final <- full_join(Oct.fec_1.0, SA_2, by="sample_id") #NOT LINING UP AS OF RIGHT NOW.

pdf("Output/Fecundity2020.pdf")
Oct.fec_1.0 %>%
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
