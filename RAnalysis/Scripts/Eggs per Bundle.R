# Set Working Directory:
rm(list=ls()) #clears workspace
setwd("~/URI/Lab-Notebook/Apul_Spawning_Nurs.vs.Wild/RAnalysis/Data/Eggs Per Bundle") #set working

# load data 
eggs.per.bundle <-read.csv('Eggs.per.bundle.counts.csv', header=T, sep=",")
boxplot(Num.Eggs ~ Condition, data = eggs.per.bundle, xlab = 'Coral Colony Origin', ylab = 'Eggs per Bundle', col = c("red", "blue"))
dev.off()

#stats
t.test(Num.Eggs~Condition, data = eggs.per.bundle) #Statistically significant if p-value <0.05
