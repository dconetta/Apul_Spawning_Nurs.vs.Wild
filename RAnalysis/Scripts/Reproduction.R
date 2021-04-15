# Set Working Directory:
rm(list=ls()) #clears workspace

#load libraries
library(tidyverse)
library(ggpubr)
library(showtext)
showtext_auto()

#FECUNDITY
#standard curve to get SA
wax.stds<-read.csv('Data/Oct_2019/wax.standards_1.csv', header=T, sep=",")
wax.stds$delta.mass.g <- wax.stds$weight2.g - wax.stds$weight1.g
plot(surface.area.cm2~delta.mass.g, data = wax.stds)
model <- lm(surface.area.cm2~delta.mass.g , data = wax.stds)
abline(model, col = "red")
summary(model)

SA<-read.csv('Data/Oct_2019/Apul_Wax_Data.csv', header=T, sep=",")
SA$surface.area.cm2 <- (model$coefficients[2]*(SA$waxedmass.g - SA$mass.g)) + model$coefficients[1]


# load data 
Oct.fec <- read.csv('Data/Oct_2019/2019_October_Fecundity.csv', header=T, sep=",")
Oct.fec_1 <- Oct.fec %>%
  group_by(Sample_ID, Origin ) %>%
  summarise(tot.eggs = sum(Total_number))

Oct.fec_1.0 <- left_join(Oct.fec_1, SA, by="Sample_ID")

Oct.fec$fecundity <-Oct.fec$tot.eggs/Oct.fec$surface.area.cm2

Fig1 <- Oct.fec %>%
  ggplot(aes(x = Origin, y = fecundity, color = Origin)) +
  labs(x = "", y = "Fecundity eggs/cm2", title="A") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() +
  theme(legend.position = "none")

#stats
t.test(fecundity~Origin, data = Oct.fec) #Statistically significant if p-value <0.05

#EGGS PER BUNDLE
# load data 
eggs.per.bundle <-read.csv('Data/Eggs.per.bundle.counts.csv', header=T, sep=",")

eggs.per.bundle <- eggs.per.bundle %>%
  group_by(Sample_ID, Origin ) %>%
  summarise(Mean.eggs = mean(Num.Eggs))

Fig2 <- eggs.per.bundle %>%
  ggplot(aes(x = Origin, y = Mean.eggs, color = Origin)) +
  labs(x = "", y = "Eggs per Bundle", title="B") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() +
  theme(legend.position = "none")

#stats
t.test(Mean.eggs~Origin, data = eggs.per.bundle) #Statistically significant if p-value <0.05


#EGG SIZE
# load data 
egg.size <-read.csv('Data/2019_October_Egg_Size.csv', header=T, sep=",")

egg.size$ratio <- egg.size$Diameter1_long/egg.size$Diameter2_short

egg.diam <- egg.size %>%
  group_by(Sample_ID, Origin ) %>%
  summarise(Mean.size = mean(Diameter1_long))

egg.ratio <- egg.size %>%
  group_by(Sample_ID, Origin ) %>%
  summarise(Mean.ratio = mean(ratio))

Fig3 <- egg.diam %>%
  ggplot(aes(x = Origin, y = Mean.size, color = Origin)) +
  labs(x = "", y = "Egg Diameter (mm)", title="C") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() +
  theme(legend.position = "none")

Fig4 <- egg.ratio %>%
  ggplot(aes(x = Origin, y = Mean.ratio, color = Origin)) +
  labs(x = "", y = "Egg Ratio (Length:Width)", title="D") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() + 
  theme(legend.position = "none")


#Stats
t.test(Mean.size ~ Origin, data = egg.diam) #Statistically significant if p-value <0.05
t.test(Mean.ratio ~ Origin, data = egg.ratio) #Statistically significant if p-value <0.05

Fig <- ggarrange(Fig1,Fig2,Fig3,Fig4, ncol = 2, nrow = 2)
ggsave("Output/Repro_Figs.pdf", Fig, width=6, height=6)


# SPECIFIC CROSS FERTILIZATION

fert <-read.csv('Data/2019_October_Fertilization_specific_crosses.csv', header=T, sep=",")
fert$prop <- fert$Fertilized_eggs/fert$Total_eggs
fert$Temperature <- as.factor(fert$Temperature)
#fert$Temperature <- ordered(fert$Temperature, levels = c("27", "31"))

female <- intToUtf8(9792)
male <- intToUtf8(9794)

pdf("Output/Fertilization_Specific_Crosses.pdf", width=6, height=5)
fert %>%
  ggplot(aes(x = Colony_Female, y = prop, group = Temperature, color = Temperature)) +
  geom_jitter(width = 0.1)  +
  scale_color_manual(values=c("cyan", "coral")) +
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  facet_wrap(~ Colony_Male) +
  labs(x = "", y = "Proportion Fertilized") +
  theme_bw()
dev.off()


