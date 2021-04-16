# Set Working Directory:
rm(list=ls()) #clears workspace
setwd("C:/Users/dcone/Documents/Git-Hub/Apul_Spawning_Nurs.vs.Wild/RAnalysis")

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

pdf("Output/Fertilization_Specific_Crosses.pdf", width=6, height=5)
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

