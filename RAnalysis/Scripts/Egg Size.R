# Set Working Directory:
rm(list=ls()) #clears workspace
setwd("~/URI/Lab-Notebook/Apul_Spawning_Nurs.vs.Wild/RAnalysis/Data/Egg Size") #set working

# load data 
eggs.size <-read.csv('Egg.size.csv', header=T, sep=",")
eggs.size$avg_egg_size <-(((egg.size$Diameter1_long)+(egg.size$Diameter2_short))/2) #making a new column in the data to get avg egg size
avg.size <- aggregate(avg_egg_size ~ Sample_ID*Origin, data = egg.size, FUN = mean) #aggregate of avg egg size per sampled colony and organized further by site of origin

#Boxplot for Fert Rates per Temp Treatment
boxplot(avg_egg_size ~ Origin, data = avg.size, xlab = 'Site of Origin', ylab = 'Average Egg Size', col = c("red", "blue")) #Creating boxplot to compare fertilization rates between temp treatments
dev.off()


#Stats
t.test(avg_egg_size ~ Origin, data = avg.size) #Statistically significant if p-value <0.05



