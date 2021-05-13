# Set Working Directory:
rm(list=ls()) #clears workspace

#load libraries
library(tidyverse)
library(Hmisc)
library(lubridate)
library(tidyr)
library(ggpubr)
library(broom)

#Set working directory
setwd("C:/Users/dcone/Documents/Git-Hub/Apul_Spawning_Nurs.vs.Wild/RAnalysis")
eggs.per.bundle2 <-read.csv('Data/Oct_2020/Eggs.per.bundle_2020.csv', header=T, sep=",")
eggs.per.bundle2.0 <- filter(eggs.per.bundle2, !is.na(Num.Eggs)) #getting rid of all NA values for num eggs column

eggs.per.bundle_Acy2020 <- eggs.per.bundle2.0 %>%
  filter(Species=="A.cyatherea") %>% 
  group_by(Sample_ID, Species, Origin, Treatment) %>% #Grouping all the wanted variables
  summarise(Mean.eggs = mean(Num.Eggs)) #Summarizing all the wells for each sample-ID and providing new Mean.eggs per sample

pdf("Output/Acyatherea_Figs/Eggs_per_Bundle_Acy2020.pdf")
eggs.per.bundle_Acy2020 %>%
  ggplot(aes(x = Origin, y = Mean.eggs, color = Treatment)) +
  labs(x ="Site", y = "Eggs per Bundle 2020", title = "A.cyatherea") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = "mean_cl_normal", fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic()
dev.off()

#stats - simple t-test to 
t.test(Mean.eggs~Treatment, data = eggs.per.bundle_Acy2020) #Statistically significant if p-value <0.05

