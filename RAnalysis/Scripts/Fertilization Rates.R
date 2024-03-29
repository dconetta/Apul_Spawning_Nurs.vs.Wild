# Set Working Directory:
rm(list=ls()) #clears workspace
setwd("C:/Users/dcone/Documents/Git-Hub/Apul_Spawning_Nurs.vs.Wild/RAnalysis")

#load libraries
library(tidyverse)
library(Hmisc)
library(lubridate)
library(tidyr)
library(ggpubr)
library(dplyr)
library(car)
library(broom)

#2019_____________________________________________________________________________
# SPECIFIC CROSS FERTILIZATION

fert <-read.csv('Data/Oct_2019/2019_October_Fertilization_specific_crosses.csv', header=T, sep=",")

#Lining up all necessary colonies to merge with 2020 data as well as making proportions
fert_final <- fert %>%
  mutate(prop = (Fertilized_eggs/fert$Total_eggs)) %>%
  mutate(Temp.Treatment = Temperature) %>% 
  mutate(Male.Colony = Colony_Male) %>% #aligning all the male colonies
  mutate(Female.Colony = Colony_Female) %>%
  mutate(Temp.Treatment = as.factor(Temp.Treatment)) %>% #making temperature a factor not numeric
  mutate(Tube.Number = Tube_Number) %>%
  summarise(Tube.Number, Temp.Treatment, Male.Colony, Female.Colony, year, prop)
  

#fert$Temperature <- ordered(fert$Temperature, levels = c("27", "31"))

Female <- intToUtf8(9792) #making female and male signs
Male <- intToUtf8(9794)

pdf("Output/Fertilization_2019_Crosses.pdf", width=6, height=5)
fert_final %>%
  ggplot(aes(x = Female.Colony, y = prop, group = Temp.Treatment, color = Temp.Treatment)) +
  geom_jitter(width = 0.1)  +
  scale_color_manual(values=c("cyan", "coral")) +
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  facet_wrap(~ Male.Colony) +
  labs(x = "Female Colonies", y = "Proportion Fertilized", color = "Treatment") +
  theme_bw()
dev.off()

#STATS - (only on crosses which had more than zero and their reciprocals)
#Some sort of nest approach needed but right now do not know what that is so in mean time running t.test on all
#temperature treatments of each cross, requires filtering and then t.test
#C7 female and C14 male (significant - mean for ambient double the amount of the heat)
C7.f_C14.m_27C <- filter(fert_final, Male.Colony == "C14" & Female.Colony == "C7" & Temp.Treatment == 27)
C7.f_C14.m_31C <- filter(fert_final, Male.Colony == "C14" & Female.Colony == "C7" & Temp.Treatment == 31)
t.test(C7.f_C14.m_27C$prop, C7.f_C14.m_31C$prop)

#C14 female and C7 male (not significant) - means are basically zero.
C7.m_C14.f_27C <- filter(fert_final, Male.Colony == "C7" & Female.Colony == "C14" & Temp.Treatment == 27)
C7.m_C14.f_31C <- filter(fert_final, Male.Colony == "C7" & Female.Colony == "C14" & Temp.Treatment == 31)
t.test(C7.m_C14.f_27C$prop, C7.m_C14.f_31C$prop)

#C8 female and C14 male (significant - ambient five times larger than heat)
C8.f_C14.m_27C <- filter(fert_final, Male.Colony == "C14" & Female.Colony == "C8" & Temp.Treatment == 27)
C8.f_C14.m_31C <- filter(fert_final, Male.Colony == "C14" & Female.Colony == "C8" & Temp.Treatment == 31)
t.test(C8.f_C14.m_27C$prop, C8.f_C14.m_31C$prop)

#C14 female and C8 male (not significant)
C8.m_C14.f_27C <- filter(fert_final, Male.Colony == "C8" & Female.Colony == "C14" & Temp.Treatment == 27)
C8.m_C14.f_31C <- filter(fert_final, Male.Colony == "C8" & Female.Colony == "C14" & Temp.Treatment == 31)
t.test(C8.m_C14.f_27C$prop, C8.m_C14.f_31C$prop)

#C7 male and C8 Female (not significant - not going to bother with reciprocal cross)
C7.m_C8.f_27C <- filter(fert_final, Male.Colony == "C7" & Female.Colony == "C8" & Temp.Treatment == 27)
C7.m_C8.f_31C <- filter(fert_final, Male.Colony == "C7" & Female.Colony == "C8" & Temp.Treatment == 31)
t.test(C7.m_C8.f_27C$prop, C7.m_C8.f_31C$prop)

#2020_____________________________________________________________________________

fert2 <-read.csv('Data/Oct_2020/2020_Fertilization_Success.csv', header=T, sep=",")

fert2_final <- fert2 %>% #Getting the sums of all the eggs and fertilized eggs by tube number and making a proportion column - represents fert success
  mutate(Date.Crossed = ymd(Date.Crossed)) %>%
  mutate(year = year(Date.Crossed)) %>%
  group_by(Tube.Number, Temp.Treatment, Male.Colony, Female.Colony, year) %>%
  summarise(tot.fert = sum(Fert.Eggs), tot.eggs = sum(Total.Eggs)) %>% #sum all eggs and fert egg counts for each tube
  mutate(prop=(tot.fert/tot.eggs)) %>% #make proportion of fertilized eggs and total egg counts
  filter(!is.na(prop)) %>% #take out all NA values in prop column
  mutate(Temp.Treatment = as.factor(Temp.Treatment)) #making sure it is seeing Temp as Factor not numeric

fert2_final <- fert2_final %>%
  summarise(Tube.Number, Temp.Treatment, Male.Colony, Female.Colony, year, prop)

pdf("Output/Fertilization_2020_Crosses.pdf", width=6, height=5)
fert2_final %>%
  ggplot(aes(x = Female.Colony, y = prop, group = Temp.Treatment, color = Temp.Treatment)) +
  geom_jitter(width = 0.1)  +
  scale_color_manual(values=c("cyan", "coral")) +
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  facet_wrap(~ Male.Colony) +
  labs(x = "Female Colonies", y = "Proportion Fertilized") +
  theme_bw()
dev.off()
