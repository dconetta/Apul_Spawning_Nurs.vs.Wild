# Set Working Directory:
rm(list=ls()) #clears workspace

#load libraries
library(tidyverse)
library(Hmisc)
library(lubridate)
library(tidyr)
library(ggpubr)
library(broom)

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
  theme_classic() #+ 
  #stat_compare_means(method = "t.test") #adding t.test comparisons/significance to ggplots
dev.off()

#stats
t.test(Mean.eggs~Origin, data = eggs.per.bundle) #Statistically significant if p-value <0.05

#2020______________________________________________________
setwd("C:/Users/dcone/Documents/Git-Hub/Apul_Spawning_Nurs.vs.Wild/RAnalysis")
eggs.per.bundle2 <-read.csv('Data/Oct_2020/Eggs.per.bundle_2020.csv', header=T, sep=",")
eggs.per.bundle2.0 <- filter(eggs.per.bundle2, !is.na(Num.Eggs)) #getting rid of all NA values for num eggs column

eggs.per.bundle_2020 <- eggs.per.bundle2.0 %>%
  filter(Species=="A.pulchra") %>% 
  filter(Origin=="Mahana" | Origin=="Nursery") %>%
  filter(Treatment=="Nursery" | Treatment=="Wild") %>%
  group_by(Sample_ID, Species, Origin, Treatment) %>% #Grouping all the wanted variables
  summarise(Mean.eggs = mean(Num.Eggs)) #Summarizing all the wells for each sample-ID and providing new Mean.eggs per sample

#pdf("Output/eggs.per.bundle_2020.pdf")
eggs.per.bundle_2020 %>%
  ggplot(aes(x = Treatment, y = Mean.eggs, color = Treatment)) +
  labs(x ="", y = "Eggs per Bundle 2020") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = "mean_cl_normal", fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() #+ 
  #stat_compare_means(method = "t.test") #adding t.test comparisons/significance to ggplots
dev.off()

#stats
t.test(Mean.eggs~Origin, data = eggs.per.bundle_2020) #Statistically significant if p-value <0.05

#Merge 20219 with 2020 Data___________________________________________

#2019 has columns for Spawn Date, Species, Sample_ID, Origin, Well.Num, Num.Eggs

#2020 has columns for Spawn Date, Species, Sample_ID, Origin, TREATMENT, Well.Num, Num.Eggs
  #So 2020 has a column that 2019 does not, also will have to make the another column that is just year which can pull from Date


#Make new column for 2019 to break out Spawn.Date into YMD separately

epb19 <- eggs.per.bundle_2019 %>%
  mutate(Spawn.Date = ymd(Spawn.Date)) %>%
  mutate(year = year(Spawn.Date)) %>%
  mutate(Treatment = Origin) %>%
  group_by(Spawn.Date, year, Species, Sample_ID, Origin, Well.Num, Num.Eggs)

#make a new column for year from Spawn.Date
epb20 <- eggs.per.bundle2.0 %>%
  filter(Species=="A.pulchra") %>% 
  filter(Origin=="Mahana" | Origin=="Nursery") %>%
  filter(Treatment=="Nursery" | Treatment=="Wild") %>%
  mutate(Spawn.Date = ymd(Spawn.Date)) %>%
  mutate(year = year(Spawn.Date)) %>%
  group_by(Spawn.Date, year, Species, Sample_ID, Origin, Treatment, Well.Num, Num.Eggs)

#Merge epb19 and epb20 data sets to be able to compare the data between both
epb_19_20 <- full_join(epb19, epb20,
                        by = c("Spawn.Date", "Species", "Sample_ID", "Origin", "Treatment", "Well.Num", "Num.Eggs", "year"))

epb_19_20_final <- epb_19_20 %>% #summarize the mean eggs per bundle for each sample_ID
  group_by(Sample_ID, Treatment, year) %>%
  summarise(Mean.eggs = mean(Num.Eggs))

#pdf("Output/eggs.per.bundle_2019.v.2020.pdf") #output PDF of 2019 and 2020 comparisons 
epb_19_20_final %>% 
  ggplot(aes(x = Treatment, y = Mean.eggs, color = Treatment)) +
  labs(x ="Treatment", y = "Eggs per Bundle") +
  facet_wrap(~year) +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = "mean_cl_normal", fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() #+ 
  #stat_compare_means(method = "t.test") #adding t.test comparisons/significance to ggplots
dev.off()


#Saving the figure as Fig.1

Fig.1 <- epb_19_20_final %>% 
  ggplot(aes(x = Treatment, y = Mean.eggs, color = Treatment)) +
  labs(x ="Treatment", y = "Eggs per Bundle") +
  facet_wrap(~year) +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = "mean_cl_normal", fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() #+ 
  #stat_compare_means(method = "t.test") #adding t.test comparisons/significance to ggplots


#Two_Anova Stats
model1 <- aov(Mean.eggs ~ Treatment*year, data = epb_19_20_final)
plot(model1, 1)
plot(model1, 2)
anova(model1)

TukeyHSD(model1)

#no significance