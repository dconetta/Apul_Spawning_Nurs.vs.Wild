# Set Working Directory:
setwd("~/URI/Lab-Notebook/E5_ROL/RAnalysis/Data") #set working

# load data 
eggs.size <-read.csv('Egg.size.csv', header=T, sep=",")
boxplot(Avg_Size ~ Condition, data = eggs.per.bundle, xlab = 'Coral Colony Origin', ylab = 'Eggs Size (mm^2)', col = c("red", "blue"))
dev.off()

#stats
t.test(Avg_Size~Condition, data = eggs.size) #Statistically significant if p-value <0.05