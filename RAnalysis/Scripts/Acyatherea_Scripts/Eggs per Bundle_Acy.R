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

#Ordering the dataframe to have the nursery site first and then all the wild sites 
eggs.per.bundle_Acy2020$Origin <- factor(eggs.per.bundle_Acy2020$Origin, levels = c("Nursery", "Linareva", "Mahana", "Sofitel"))

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

#stats - Anova to test the differences between the means of each site broken up by treatment. No significance
model1 <- aov(Mean.eggs ~ Treatment*Origin, data = eggs.per.bundle_Acy2020)
plot(model1, 1)
plot(model1, 2)
anova(model1)
TukeyHSD(model1)

#putting A. pulchra and A. cyatherea plots together

Apul <- eggs.per.bundle_2020 %>%
  ggplot(aes(x = Treatment, y = Mean.eggs, color = Treatment)) +
  labs(x ="Site", y = "Eggs per Bundle 2020", title = "A.pulchra") +
  scale_y_continuous(limits = c(0,12)) + 
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = "mean_cl_normal", fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic()

Acy <- eggs.per.bundle_Acy2020 %>%
  ggplot(aes(x = Origin, y = Mean.eggs, color = Treatment)) +
  labs(x ="Site", y = "Eggs per Bundle 2020", title = "A.cyatherea") +
  scale_y_continuous(limits = c(0,12)) +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = "mean_cl_normal", fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic()

Apul_vs_Acy_epb2020 <- ggarrange(Apul,Acy, ncol = 2, nrow = 1)

ggsave("Output/Acyatherea_Figs/Apul_vs_Acya_EPB2020.pdf", Apul_vs_Acy_epb2020, width=10, height=5)

