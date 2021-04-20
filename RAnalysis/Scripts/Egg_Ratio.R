#Egg Ratio__________________________________________________________
#2019 and 2020
#2019
egg.size <-read.csv('Data/Oct_2019/2019_October_Egg_Size.csv', header=T, sep=",") #Load 2019 data
egg.size$ratio <- egg.size$Diameter1_long/egg.size$Diameter2_short #make a ratio

egg.ratio19 <- egg.size %>%
  mutate(Date = ymd(Date)) %>% #formatting Date 
  mutate(year = year(Date)) %>% #adding a year column
  mutate(Treatment = Origin) %>%
  group_by(Sample_ID, Treatment, year) %>%
  summarise(Mean.ratio = mean(ratio))

#2020
egg.size2 <-read.csv('Data/Oct_2020/2020_October_Egg_Size.csv', header=T, sep=",")
egg.size2$ratio <- egg.size2$Diam_long_mm/egg.size2$Diam_short_mm #make a ratio

egg.size2.0 <- egg.size2 %>% #adding a treatment column that has Wild and Nursery based off of values/strings in Origin
  mutate(Treatment = case_when(
    endsWith(Origin, "a") ~ "Wild",
    endsWith(Origin, "y") ~ "Nursery",
  )) #adding a treatment column

egg.ratio20 <- egg.size2.0 %>%
  mutate(Date = ymd(Date)) %>% #formatting Date 
  mutate(year = year(Date)) %>% #adding a year column
  group_by(Sample_ID, Treatment, year) %>%
  summarise(Mean.ratio = mean(ratio))

#Merge and Compare

egg_ratio_19_20 <- full_join(egg.ratio19, egg.ratio20) 

pdf("Output/egg.ratio_2019.v.2020.pdf") #output PDF of 2019 and 2020 comparisons 
egg_ratio_19_20 %>%
  ggplot(aes(x = Treatment, y = Mean.ratio, color = Treatment)) +
  labs(x = "Treatment", y = "Egg Diameter Ratio") +
  facet_wrap(~year) +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() #+ 
  #stat_compare_means(method = "t.test") #adding t.test comparisons/significance to ggplots

Fig.3 <- egg_ratio_19_20 %>%
  ggplot(aes(x = Treatment, y = Mean.ratio, color = Treatment)) +
  labs(x = "Treatment", y = "Egg Diameter Ratio") +
  facet_wrap(~year) +
  geom_jitter(width = 0.1) +                                            # Plot all points
  stat_summary(fun.data = mean_cl_normal, fun.args = list(mult = 1),    # Plot standard error
               geom = "errorbar", color = "black", width = 0.1) +
  stat_summary(fun = mean, geom = "point", color = "black") +          # Plot mean
  theme_classic() #+ 
  #stat_compare_means(method = "t.test") #adding t.test comparisons/significance to ggplots

#Stats (T-test within year, Anova to compare means between treatment and year)
t.test(Mean.ratio ~ Treatment, data = egg.ratio19) #Statistically significant if p-value <0.05
t.test(Mean.ratio ~ Treatment, data = egg.ratio20) #Statistically significant if p-value <0.05

model3 <- aov(Mean.ratio ~ Treatment*year, data = egg_ratio_19_20)
plot(model3, 1)
plot(model3, 2)
anova(model3)
#siginicant for year but nothing else

#Putting all Comparison Plots together (Fig.1 = Eggs per Bundle ; Fig.2 = Egg Size ; Fig.3 = Egg Diam Ratio)
Rep_Char_Fig_Final <- ggarrange(Fig.1,Fig.2,Fig.3,Fig.4, ncol = 2, nrow = 2)


ggsave("Output/Repro_Figs_19v20.pdf", Fig, width=10, height=10)
