# Set Working Directory:
rm(list=ls()) #clears workspace

#load libraries
library(tidyverse)
library(Hmisc)
library(lubridate)
library(tidyr)
library(ggpubr)
library(dplyr)
library(broom)

#Setting Working Directory
setwd("~/URI/Lab-Notebook/Apul_Spawning_Nurs.vs.Wild/RAnalysis")
wax.stds2<-read.csv('Data/Oct_2020/wax.standards_1.csv', header=T, sep=",") #reading in wax standards file
wax.stds2$delta.mass.g <- wax.stds2$weight2.g - wax.stds2$weight1.g #calculating the change in mass from dry to wax dipped skeleton
#calculate the surface area
wax.stds2 <- wax.stds2 %>%
  mutate(surface.area.cm2 = (4*pi*(diameter/2)^2))

plot(surface.area.cm2~delta.mass.g, data = wax.stds2)
model2 <- lm(surface.area.cm2~delta.mass.g , data = wax.stds2)
abline(model2, col = "red")
summary(model2)

SA_2 <- read.csv('Data/Oct_2020/Fecundity_Surface_Area.csv', header=T, sep=",")

SA_2 <- filter(SA_2, species == "A.cyatherea")
  

SA_2$surface.area.cm2 <- (model2$coefficients[2]*(SA_2$weight2.g - SA_2$weight1.g)) + model2$coefficients[1]


Oct.fec_2 <- read.csv('Data/Oct_2020/2020_October_Fecundity_counts.csv', header=T, sep=",")
Oct.fec_2.0 <- Oct.fec_2 %>%
  filter(!is.na(Total_number)) %>%
  filter(Species == "A. cyatherea") %>%
  group_by(sample_id, Origin, Site) %>%
  summarise(tot.eggs = sum(Total_number))

Oct.fec_final <- left_join(SA_2, Oct.fec_2.0, by="sample_id")
Oct.fec_final$fecundity <-Oct.fec_final$tot.eggs/Oct.fec_final$surface.area.cm2

Oct.fec_final <- filter(Oct.fec_final, !is.na(fecundity)) #filtering out NA values for fecundity because some egg counts dont have fragment SA's

#Ordering the dataframe to have the nursery site first and then all the wild sites 
Oct.fec_final$origin <- factor(Oct.fec_final$origin, levels = c("Nursery", "Linareva", "Sofitel"))

pdf("Output/Acyatherea_Figs/Fecundity_Acy2020.pdf")
Oct.fec_final %>%
  ggplot(aes(x = origin, y = fecundity, color = treatment)) +
  labs(x = "Site", y = "Fecundity eggs/cm2", title = "A.cyatherea", color = "Treatment") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() 
dev.off()

#stats
t.test(fecundity~treatment, data = Oct.fec_final) #Statistically significant if p-value <0.05
