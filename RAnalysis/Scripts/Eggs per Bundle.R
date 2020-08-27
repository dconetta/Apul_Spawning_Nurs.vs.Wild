# Set Working Directory:
rm(list=ls()) #clears workspace

#load libraries
library(tidyverse)

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
