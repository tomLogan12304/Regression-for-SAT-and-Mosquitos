Logan Tom
dir()
sat <- read.csv("SAT (1).csv")

#Scatter plot
plot(sat$SAT, sat$GPA)

satModel <- lm(GPA ~ SAT, data = sat)
summary(satModel)


#Scatter plot that includes regression line
plot(sat$SAT, sat$GPA)
abline(reg = satModel)

#Diagnostic Plots
plot(satModel)

#Retrieving important quantities
coefficients(satModel)
residuals(satModel)
fitted.values(satModel)




#Mosquito Model
mosquitos <- read.csv("mosquitos (1).csv")
summary(mosquitos)

#Plot histograms of both variables
hist(mosquitos$AegPupae)
hist(mosquitos$DevelopmentSites)

#Create the scatter plot
plot(mosquitos$DevelopmentSites, mosquitos$AegPupae)

#Estimate the regression model
mosqModLinear <- lm(AegPupae ~ DevelopmentSites, data = mosquitos)
summary(mosqModLinear)

#Create the scatter plot with the regression model
plot(mosquitos$DevelopmentSites, mosquitos$AegPupae)
abline(reg = mosqModLinear)

#Assess the diagnostic plots
plot(mosqModLinear)


#Outlier Section

#Run the model without the outlier
mosqModLinear30 <- lm(AegPupae ~ DevelopmentSites, data = mosquitos[-30,])
summary(mosqModLinear30)

#Visualize the difference in models with and without the outlier
plot(mosquitos$DevelopmentSites, mosquitos$AegPupae)
points(mosquitos$DevelopmentSites[30], mosquitos$AegPupae[30], col = "red")
abline(reg = mosqModLinear)
abline(reg = mosqModLinear30, col = "red")

#Assess the diagnostic plots of the model without outlier
plot(mosqModLinear30)


#Log Model
hist(log(mosquitos$AegPupae))

#Estimate the log model parameters
mosqModLog <- lm(log(AegPupae) ~ DevelopmentSites, data = mosquitos)
summary(mosqModLog)

#Plot the estimated model on the scatter plot
plot(mosquitos$DevelopmentSites, log(mosquitos$AegPupae))
abline(reg = mosqModLog)

#Assess the diagnostic plots of the log model
plot(mosqModLog)




#Log-Log Model
hist(log(mosquitos$DevelopmentSites))

#Estimate the log-log model parameters
mosqModLogLog <- lm(log(AegPupae) ~ log(DevelopmentSites), data = mosquitos)
summary(mosqModLogLog)

#Plot the estimated model on the scatter plot
plot(log(mosquitos$DevelopmentSites), log(mosquitos$AegPupae))
abline(reg = mosqModLogLog)

#Assess the diagnostic plots of the log-log model
plot(mosqModLogLog)


