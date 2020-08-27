# Set Working Directory:
rm(list=ls()) #clears workspace

# load data 
egg.size <-read.csv('Data/2019_October_Egg_Size.csv', header=T, sep=",")

egg.size <- egg.size %>%
  group_by(Sample_ID, Origin ) %>%
  summarise(Mean.size = mean(Diameter1_long))

pdf("Output/egg.size.pdf")
egg.size %>%
  ggplot(aes(x = Origin, y = Mean.size, color = Origin)) +
  labs(x = "", y = "Egg Diameter (mm)") +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic()
dev.off()

#Stats
t.test(Mean.size ~ Origin, data = egg.size) #Statistically significant if p-value <0.05



