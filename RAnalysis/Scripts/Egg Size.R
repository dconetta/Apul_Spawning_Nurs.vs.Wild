# Set Working Directory:
rm(list=ls()) #clears workspace

#load libraries
library(tidyverse)
library(Hmisc)
library(lubridate)
library(tidyr)
library(ggpubr)
library(dplyr)

#2019_____________________________________________________________________________
# load data 
egg.size <-read.csv('Data/Oct_2019/2019_October_Egg_Size.csv', header=T, sep=",")

egg.size.mean <- egg.size %>%
  group_by(Sample_ID, Origin ) %>%
  summarise(Mean.size = mean(Diameter1_long))

pdf("Output/egg.size.pdf")
egg.size.mean %>%
  ggplot(aes(x = Origin, y = Mean.size, color = Origin)) +
  labs(x = "", y = "Egg Diameter (mm)") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() + 
  stat_compare_means(method = "t.test") #adding t.test comparisons/significance to ggplots
dev.off()

#Stats
t.test(Mean.size ~ Origin, data = egg.size.mean) #Statistically significant if p-value <0.05

#2020_____________________________________________________________________________

egg.size2 <-read.csv('Data/Oct_2020/2020_October_Egg_Size.csv', header=T, sep=",")

egg.size2.mean <- egg.size2 %>%
  group_by(Sample_ID, Origin) %>%
  summarise(Mean.size = mean(Diam_long_mm))

pdf("Output/egg.size.pdf")
egg.size2.mean %>%
  ggplot(aes(x = Origin, y = Mean.size, color = Origin)) +
  labs(x = "", y = "Egg Diameter (mm)") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() + 
  stat_compare_means(method = "t.test") #adding t.test comparisons/significance to ggplots
dev.off()

#Stats
t.test(Mean.size ~ Origin, data = egg.size2.mean) #Statistically significant if p-value <0.05

#Merge and Plot 2019 and 2020_______________________________________________________

es19 <- egg.size %>%
  mutate(Date = ymd(Date)) %>% #formatting Date 
  mutate(year = year(Date)) %>% #adding a year column
  mutate(Treatment = Origin) %>%
  group_by(Sample_ID, Treatment, year) %>%
  summarise(Mean.size = mean(Diameter1_long))
  
egg.size2.0 <- egg.size2 %>% #adding a treatment column that has Wild and Nursery based off of values/strings in Origin
  mutate(Treatment = case_when(
    endsWith(Origin, "a") ~ "Wild",
    endsWith(Origin, "y") ~ "Nursery",
  )) #adding a treatment column

es20 <- egg.size2.0 %>%
  mutate(Date = ymd(Date)) %>% #formatting Date 
  mutate(year = year(Date)) %>% #adding a year column
  group_by(Sample_ID, Treatment, year) %>%
  summarise(Mean.size = mean(Diam_long_mm))

#Merge es19 and es20
es_19_20 <- full_join(es19, es20)

#plot both 2019 and 2020 Egg Size

pdf("Output/eggs.size_2019.v.2020.pdf") #output PDF of 2019 and 2020 comparisons 
es_19_20 %>% 
  ggplot(aes(x = Treatment, y = Mean.size, color = Treatment)) +
  labs(x ="Treatment", y = "Egg Diameter (mm)") +
  facet_wrap(~year) +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = "mean_cl_normal", fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() + 
  stat_compare_means(method = "t.test") #adding t.test comparisons/significance to ggplots
dev.off()



