# Set Working Directory:
rm(list=ls()) #clears workspace
setwd("~/URI/Lab-Notebook/Apul_Spawning_Nurs.vs.Wild/RAnalysis/Data/Fecundity") #set working

#standard curve to get SA
wax.stds<-read.csv('wax.standards_1.csv', header=T, sep=",")
plot(weight1.g~surface.area.cm2, data = wax.stds)
model <- lm(weight1.g~surface.area.cm2, data = wax.stds)
abline(model, col = "red")
summary(model)

# load data 
Oct.fec <-read.csv('2019_October_Fecundity_summary.csv', header=T, sep=",")
Oct.fec$fecundity <-Oct.fec$Num_Eggs/Oct.fec$SA
boxplot(fecundity ~ Origin, data = Oct.fec, xlab = 'Coral Colony Origin', ylab = 'Fecundity (bundles/cm^2)', col = c("red", "blue"))
dev.off()


#stats
t.test(Fecundity~Origin, data = Oct.fec) #Statistically significant if p-value <0.05

