# Set Working Directory:
rm(list=ls()) #clears workspace

#load libraries
library(tidyverse)
library(Hmisc)

# load data 
#2019____________________________________________________
setwd("C:/Users/dcone/Documents/Git-Hub/Apul_Spawning_Nurs.vs.Wild/RAnalysis")
eggs.per.bundle_2019 <- read.csv('Data/Oct_2019/Eggs.per.bundle.counts.csv', header=T, sep=",")

eggs.per.bundle <- eggs.per.bundle_2019 %>%
  group_by(Sample_ID, Origin ) %>%
  summarise(Mean.eggs = mean(Num.Eggs))

pdf("Output/eggs.per.bundle_2019.pdf")
eggs.per.bundle %>%
  ggplot(aes(x = Origin, y = Mean.eggs, color = Origin)) +
  labs(x = "", y = "Eggs per Bundle 2019") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = "mean_cl_normal", fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic()
dev.off()

#stats
t.test(Mean.eggs~Origin, data = eggs.per.bundle) #Statistically significant if p-value <0.05

#2020______________________________________________________
setwd("C:/Users/dcone/Documents/Git-Hub/Apul_Spawning_Nurs.vs.Wild/RAnalysis")
eggs.per.bundle2 <-read.csv('Data/Oct_2020/Wild_vs_Nurs/Eggs.per.bundle_2020.csv', header=T, sep=",")
eggs.per.bundle2.0 <- filter(eggs.per.bundle2, !is.na(Num.Eggs)) #getting rid of all NA values for num eggs column

eggs.per.bundle_2020 <- eggs.per.bundle2.0 %>%
  filter(Species=="A.pulchra") %>% 
  filter(Origin=="Mahana" | Origin=="Nursery") %>%
  filter(Treatment=="Nursery" | Treatment=="Wild") %>%
  group_by(Sample_ID, Species, Origin, Treatment) %>% #Grouping all the wanted variables
  summarise(Mean.eggs = mean(Num.Eggs)) #Summarizing all the wells for each sample-ID and providing new Mean.eggs per sample

pdf("Output/eggs.per.bundle_2020.pdf")
eggs.per.bundle_2020 %>%
  ggplot(aes(x = Treatment, y = Mean.eggs, color = Treatment)) +
  labs(x ="", y = "Eggs per Bundle 2020") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = "mean_cl_normal", fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic()
dev.off()

#stats
t.test(Mean.eggs~Origin, data = eggs.per.bundle_2020) #Statistically significant if p-value <0.05
