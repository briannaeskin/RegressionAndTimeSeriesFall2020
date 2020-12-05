##This is the code for the inital model analysis, including the splitting of data to Train and Test sets
library("faraway")
library("leaps")
library("lmtest")
library("MASS")

#read data
clinton <- read.csv("https://raw.githubusercontent.com/briannaeskin/RegressionAndTimeSeriesFall2020/main/ClintonElectionData1992.csv", header = TRUE, row.names = 1, stringsAsFactors = FALSE)

#inital model
lmod_clinton <- lm(VotingPct ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, clinton)
summary(lmod_clinton)

#VIF Check
x <- model.matrix(lmod_clinton)[,-1]
vif(x)

#Error Assumptions
#Constant Variance
plot(fitted(lmod_clinton), residuals(lmod_clinton), xlab="Fitted", ylab="Residuals")
abline(h=0)
plot(fitted(lmod_clinton),sqrt(abs(residuals(lmod_clinton))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))))
#Normality
qqnorm(residuals(lmod_clinton),ylab="Residuals",main="")
qqline(residuals(lmod_clinton))
shapiro.test(residuals(lmod_clinton))
#Correlated Errors
n <- length(residuals(lmod_clinton))
plot(tail(residuals(lmod_clinton),n-1) ~ head(residuals(lmod_clinton),n-1),
     xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),main="Successive Fitted Errors")
abline(h=0,v=0,col=grey(0.75))
dwtest(lmod_clinton)

#Box-Cox
boxcox(lmod_clinton,plotit=T,lambda=seq(0.5,1.5,by=0.05))

#Outlier/Influential Checks
#Leverage
hatv <- hatvalues(lmod_clinton)
counties <- row.names(clinton)
halfnorm(hatv, labs=counties,ylab="Leverages")
head(sort(hatv,decreasing=T))
#Outliers
stud <- rstudent(lmod_clinton)
jackres <- stud*(2693/(2694-stud^2))^0.5
head(sort(abs(jackres),decreasing=T))
qt(0.05/2704,2693)
#Influential Points
cook <- cooks.distance(lmod_clinton)
head(sort(abs(cook),decreasing=T))
halfnorm(cook,3,labs=counties,ylab="Cook's Distance")

#Remove Kings NY from analysis and repeat analysis
clinton <- clinton[-c(1602),]

lmod_clinton <- lm(VotingPct ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, clinton)
summary(lmod_clinton)

#VIF Check
x <- model.matrix(lmod_clinton)[,-1]
vif(x)

#Error Assumptions
#Constant Variance
plot(fitted(lmod_clinton), residuals(lmod_clinton), xlab="Fitted", ylab="Residuals")
abline(h=0)
plot(fitted(lmod_clinton),sqrt(abs(residuals(lmod_clinton))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))))
#Normality
qqnorm(residuals(lmod_clinton),ylab="Residuals",main="")
qqline(residuals(lmod_clinton))
shapiro.test(residuals(lmod_clinton))
#Correlated Errors
n <- length(residuals(lmod_clinton))
plot(tail(residuals(lmod_clinton),n-1) ~ head(residuals(lmod_clinton),n-1),
     xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),main="Successive Fitted Errors")
abline(h=0,v=0,col=grey(0.75))
dwtest(lmod_clinton)

#Box-Cox
boxcox(lmod_clinton,plotit=T,lambda=seq(0.5,1.5,by=0.05))

#Outlier/Influential Checks
#Leverage
hatv <- hatvalues(lmod_clinton)
counties <- row.names(clinton)
halfnorm(hatv, labs=counties,ylab="Leverages")
head(sort(hatv,decreasing=T))
#Outliers
stud <- rstudent(lmod_clinton)
jackres <- stud*(2693/(2694-stud^2))^0.5
head(sort(abs(jackres),decreasing=T))
qt(0.05/2704,2693)
#Influential Points
cook <- cooks.distance(lmod_clinton)
head(sort(abs(cook),decreasing=T))
halfnorm(cook,3,labs=counties,ylab="Cook's Distance")

#Transform predictor by .75 as a result of Box Cox without Kings, NY. See checks in clinton_model_build_tinker, lines 1-49 for tests with both 0.5 and 0.75
lmod_clinton_0.75 <- lm(VotingPct^0.75 ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, clinton)

#Error Assumptions
#Constant Variance
plot(fitted(lmod_clinton_0.75), residuals(lmod_clinton_0.75), xlab="Fitted", ylab="Residuals")
abline(h=0)
plot(fitted(lmod_clinton_0.75),sqrt(abs(residuals(lmod_clinton_0.75))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))))
#Normality
qqnorm(residuals(lmod_clinton_0.75),ylab="Residuals",main="")
qqline(residuals(lmod_clinton_0.75))
shapiro.test(residuals(lmod_clinton_0.75))
#Correlated Errors
n <- length(residuals(lmod_clinton_0.75))
plot(tail(residuals(lmod_clinton_0.75),n-1) ~ head(residuals(lmod_clinton_0.75),n-1),
     xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),main="Successive Fitted Errors")
abline(h=0,v=0,col=grey(0.75))
dwtest(lmod_clinton_0.75)

#Outlier/Influential Checks
#Leverage
hatv <- hatvalues(lmod_clinton_0.75)
counties <- row.names(clinton)
halfnorm(hatv, labs=counties,ylab="Leverages")
head(sort(hatv,decreasing=T))
#Outliers
stud <- rstudent(lmod_clinton_0.75)
jackres <- stud*(2692/(2693-stud^2))^0.5
head(sort(abs(jackres),decreasing=T))
qt(0.05/2703,2692)
#Influential Points
cook <- cooks.distance(lmod_clinton_0.75)
head(sort(abs(cook),decreasing=T))
halfnorm(cook,3,labs=counties,ylab="Cook's Distance")

#BIC Model Selection (Backwards, after transform)
clinton_back_BIC <- regsubsets(model.matrix(lmod_clinton_0.75)[,-1],clinton$VotingPct^0.75,method='backward',nvmax=9)
clinton_back_BIC_summary <- summary(clinton_back_BIC)
plot(clinton_back_BIC_summary$bic, main='backward search: BIC')
which.min(clinton_back_BIC_summary$bic)

lmod_clinton_0.75 <- lm(VotingPct^0.75 ~ Savings + Poverty + Veterans + Female + PopDensity, clinton)
summary(lmod_clinton_0.75)

#Here, we double checked if transformation was still necessary after BIC model selection, see clinton_model_build_tinker lines 50-81

#VIF Check
x_0.75 <- model.matrix(lmod_clinton_0.75)[,-1]
vif(x_0.75)

#Error Assumptions
#Constant Variance
plot(fitted(lmod_clinton_0.75), residuals(lmod_clinton_0.75), xlab="Fitted", ylab="Residuals")
abline(h=0)
plot(fitted(lmod_clinton_0.75),sqrt(abs(residuals(lmod_clinton_0.75))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))))
#Normality
qqnorm(residuals(lmod_clinton_0.75),ylab="Residuals",main="")
qqline(residuals(lmod_clinton_0.75))
shapiro.test(residuals(lmod_clinton_0.75))
#Correlated Errors
n <- length(residuals(lmod_clinton_0.75))
plot(tail(residuals(lmod_clinton_0.75),n-1) ~ head(residuals(lmod_clinton_0.75),n-1),
     xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),main="Successive Fitted Errors")
abline(h=0,v=0,col=grey(0.75))
dwtest(lmod_clinton_0.75)

#Outlier/Influential Checks
#Leverage
hatv <- hatvalues(lmod_clinton_0.75)
counties <- row.names(clinton)
halfnorm(hatv, labs=counties,ylab="Leverages")
head(sort(hatv,decreasing=T))
#Outliers
stud <- rstudent(lmod_clinton_0.75)
jackres <- stud*(2696/(2697-stud^2))^0.5
head(sort(abs(jackres),decreasing=T))
qt(0.05/2703,2696)
#Influential Points
cook <- cooks.distance(lmod_clinton_0.75)
head(sort(abs(cook),decreasing=T))
halfnorm(cook,3,labs=counties,ylab="Cook's Distance")

#Here, I checked if was worth it to remove Hudson, NJ from data. See clinton_model_build_tinker lines 83-100

lmod_clinton_final <- lmod_clinton_0.75
