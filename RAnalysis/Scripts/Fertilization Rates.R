# Set Working Directory:
rm(list=ls()) #clears workspace
setwd("~/URI/Lab-Notebook/Apul_Spawning_Nurs.vs.Wild/RAnalysis/Data/Fertilization") #set working

# load data 
fert<-read.csv('Fertilization_Rates.csv', header=T, sep=",")
fert$fert_rates <-fert$Fertilized_eggs/fert$Total_number #making a new column in the data to get percentage of eggs fertilized
avg.fert <- aggregate(fert_rates ~ Colony.Id..Male.*Temperature..C., data = fert, FUN = mean)
boxplot(fert_rates ~ Temperature..C., data = avg.fert, xlab = 'Temperature Treatment (C)', ylab = 'Percent of Eggs Fertilized', ylim(0, 0.35), col = c("red", "blue")) #Creating boxplot to compare fertilization rates between temp treatments
legend("topright", inset=.02, title = "Temp Treatment", legend=c("27C", "31C"), col=c("blue","red"), pch=15, cex=1)


#Boxplots To compare fertilization rates of colonies between temp treatments
boxplot(fert_rates~Col_ID*Temp_Treat, data = fert, xlab = 'Male Cross Colony ID', ylab = 'Percent of Eggs Fertilized', col = c("blue", "blue","blue", "blue", "blue", "red", "red", "red", "red", "red"), axes=F) #Creating boxplot to compare fertilization rates between temp treatments
box()
axis(1, at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), labels=c("C7", "C8", "C12", "C14", "mix", "C7", "C8", "C12", "C14", "mix"))
axis(2, at=seq(0,1,0.2), seq(0,1,0.2), las=1)
legend("topright", inset=.02, title = "Temp Treatment", legend=c("27C", "31C"), col=c("blue","red"), pch=15, cex=1)
dev.off()

#ggplot FINAL (Used this One)
ggplot(fert, aes(x=Colony.Id..Male.,y=fert_rates, fill=factor(Temperature..C.))) +
  geom_boxplot() + 
  labs(fill = "Temperature") + 
  xlab("Colony ID") +
  ylab("Percent Eggs Fertilized") +
  theme_bw(base_size = 16)


#Stats
t.test(fert_rates ~ Temperature..C., data = avg.fert) #Statistically significant if p-value <0.05

aov(fert_rates ~ Colony.Id..Male.*Temperature..C., data = fert)->model1
par(mfrow=c(1,3))
hist(residuals(model1)) #look at normality of data
boxplot(residuals(model1)) #look at normality of data
plot(model1$fitted.values, model1$residuals)
summary(model1)
