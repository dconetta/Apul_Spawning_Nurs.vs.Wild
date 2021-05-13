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
wax.stds3<-read.csv('Data/Oct_2020/wax.standards_1.csv', header=T, sep=",") #reading in wax standards file
wax.stds3$delta.mass.g <- wax.stds3$weight2.g - wax.stds2$weight1.g #calculating the change in mass from dry to wax dipped skeleton
#calculate the surface area
wax.stds3 <- wax.stds3 %>%
  mutate(surface.area.cm2 = (4*pi*(diameter/2)^2))

plot(surface.area.cm2~delta.mass.g, data = wax.stds3)
model2 <- lm(surface.area.cm2~delta.mass.g , data = wax.stds3)
abline(model2, col = "red")
summary(model2)

SA_3 <- read.csv('Data/Oct_2020/Fecundity_Surface_Area.csv', header=T, sep=",")

SA_3 <- filter(SA_3, species == "A.cyatherea")
  

SA_3$surface.area.cm2 <- (model2$coefficients[2]*(SA_3$weight2.g - SA_3$weight1.g)) + model2$coefficients[1]


Oct.fec_3 <- read.csv('Data/Oct_2020/2020_October_Fecundity_counts.csv', header=T, sep=",")
Oct.fec_3.0 <- Oct.fec_3 %>%
  filter(!is.na(Total_number)) %>%
  filter(Species == "A. cyatherea") %>%
  group_by(sample_id, Origin, Site) %>%
  summarise(tot.eggs = sum(Total_number))

Oct.fec_final1 <- left_join(SA_3, Oct.fec_3.0, by="sample_id")
Oct.fec_final1$fecundity <-Oct.fec_final1$tot.eggs/Oct.fec_final1$surface.area.cm2

Oct.fec_final1 <- filter(Oct.fec_final1, !is.na(fecundity)) #filtering out NA values for fecundity because some egg counts dont have fragment SA's

#Ordering the dataframe to have the nursery site first and then all the wild sites 
Oct.fec_final1$origin <- factor(Oct.fec_final1$origin, levels = c("Nursery", "Linareva", "Sofitel"))

pdf("Output/Acyatherea_Figs/Fecundity_Acy2020.pdf")
Oct.fec_final1 %>%
  ggplot(aes(x = origin, y = fecundity, color = treatment)) +
  labs(x = "Site", y = "Fecundity eggs/cm2", title = "A.cyatherea", color = "Treatment") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() 
dev.off()

#stats
model2 <- aov(fecundity ~ treatment*origin, data = Oct.fec_final1)
plot(model2, 1)
plot(model2, 2)
anova(model2)
TukeyHSD(model2)

#Putting Apul and Acy Figures together for 2020_____________________________________
Apul1 <- Oct.fec_final %>%
  ggplot(aes(x = treatment, y = fecundity, color = treatment)) +
  labs(x = "Site", y = "Fecundity eggs/cm2", title = "A.pulchra", color = "Treatment") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() 

Acy1 <- Oct.fec_final1 %>%
  ggplot(aes(x = origin, y = fecundity, color = treatment)) +
  labs(x = "Site", y = "Fecundity eggs/cm2", title = "A.cyatherea", color = "Treatment") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic()

Apul_vs_Acy_fec2020 <- ggarrange(Apul1,Acy1, ncol = 2, nrow = 1)

ggsave("Output/Acyatherea_Figs/Apul_vs_Acya_Fec2020.pdf", Apul_vs_Acy_fec2020, width=10, height=5)
