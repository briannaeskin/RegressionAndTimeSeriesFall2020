##This is the code for the inital model analysis, including the splitting of data to Train and Test sets
library("faraway")
library("leaps")
library("lmtest")
library("MASS")

#read data
clinton <- read.csv("https://raw.githubusercontent.com/briannaeskin/RegressionAndTimeSeriesFall2020/main/ClintonElectionData1992.csv", header = TRUE, row.names = 1)

#Leverage/Outliers/Influential (Full Data, Full variables)
lmod_clinton <- lm(VotingPct ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, clinton)
summary(lmod_clinton)
#Leverage
hatv <- hatvalues(lmod_clinton)
counties <- row.names(clinton)
halfnorm(hatv, labs=counties,ylab="Leverages")
#Outliers
stud <- rstudent(lmod_clinton)
jackres <- stud*(2693/(2694-stud^2))^0.5
head(sort(abs(jackres),decreasing=T))
qt(0.05/2704,2693)
#Influential Points
cook <- cooks.distance(lmod_clinton)
head(sort(abs(cook),decreasing=T))
halfnorm(cook,3,labs=counties,ylab="Cook's Distance")
#Model Comparison without Kings, NY
lmod_clinton_2 <- lm(VotingPct ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, clinton, subset=(cook < max(cook)))
summary(lmod_clinton_2)
summary(lmod_clinton)

#split to test and train data
sample_size <- floor(0.8*nrow(clinton))
set.seed(001)

data_sample <- sample(seq_len(nrow(clinton)),size=sample_size)
train_clinton <- clinton[data_sample,]
test_clinton <- clinton[-data_sample,]
lmod_clinton_3 <- lm(VotingPct ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, train_clinton)
summary(lmod_clinton_3)

#Variance Inflation Factor
x <- model.matrix(lmod_clinton_3)[,-1]
vif(x)

#Partial Regression Plots
d <- residuals(lm(VotingPct ~ Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, train_clinton))
m <- residuals(lm(MedAge ~ Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, train_clinton))
plot(m,d,xlab="Median Age residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_3)['MedAge'])

d <- residuals(lm(VotingPct ~ MedAge + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, train_clinton))
m <- residuals(lm(Savings ~ MedAge + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, train_clinton))
plot(m,d,xlab="Savings residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_3)['Savings'])

d <- residuals(lm(VotingPct ~ MedAge + Savings + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, train_clinton))
m <- residuals(lm(PerCapIncome ~ MedAge + Savings + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, train_clinton))
plot(m,d,xlab="PerCapIncome residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_3)['PerCapIncome'])
head(sort(abs(m),decreasing=T))

d <- residuals(lm(VotingPct ~ MedAge + Savings + PerCapIncome + Veterans + Female + PopDensity + NursingHome + Crime, train_clinton))
m <- residuals(lm(Poverty ~ MedAge + Savings + PerCapIncome + Veterans + Female + PopDensity + NursingHome + Crime, train_clinton))
plot(m,d,xlab="Poverty residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_3)['Poverty'])
head(sort(abs(m),decreasing=T))

d <- residuals(lm(VotingPct ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + PopDensity + NursingHome + Crime, train_clinton))
m <- residuals(lm(Female ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + PopDensity + NursingHome + Crime, train_clinton))
plot(m,d,xlab="Female residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_3)['Female'])
head(sort(abs(m),decreasing=T))

d <- residuals(lm(VotingPct ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + NursingHome + Crime, train_clinton))
m <- residuals(lm(PopDensity ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + NursingHome + Crime, train_clinton))
plot(m,d,xlab="PopDensity residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_3)['PopDensity'])
head(sort(abs(m),decreasing=T))

d <- residuals(lm(VotingPct ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome, train_clinton))
m <- residuals(lm(Crime ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome, train_clinton))
plot(m,d,xlab="Crime residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_3)['Crime'])
head(sort(abs(m),decreasing=T))

#Error Assumptions
#Constant Variance
plot(fitted(lmod_clinton_3), residuals(lmod_clinton_3), xlab="Fitted", ylab="Residuals")
abline(h=0)
plot(fitted(lmod_clinton_3),sqrt(abs(residuals(lmod_clinton_3))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))))

#Normality
qqnorm(residuals(lmod_clinton_3),ylab="Residuals",main="")
qqline(residuals(lmod_clinton_3))
shapiro.test(residuals(lmod_clinton_3))

#Correlated Errors
dwtest(lmod_clinton_3)

#Box-Cox
boxcox(lmod_clinton_3,plotit=T,lambda=seq(0.5,1.5,by=0.1))

#BIC Model Selection (Backwards, no transform)
clinton_back_BIC <- regsubsets(x,train_clinton$VotingPct,method='backward',nvmax=9)
clinton_back_BIC_summary <- summary(clinton_back_BIC)
plot(clinton_back_BIC_summary$bic, main='backward search: BIC')
which.min(clinton_back_BIC_summary$bic)

#Filtered Model, no transform
lmod_clinton_4 <- lm(VotingPct ~ Savings + Poverty + Female + PopDensity, train_clinton)
summary(lmod_clinton_4)

x2 <- model.matrix(lmod_clinton_4)[,-1]
vif(x2)

d <- residuals(lm(VotingPct ~ Poverty + Female + PopDensity, train_clinton))
m <- residuals(lm(Savings ~ Poverty + Female + PopDensity, train_clinton))
plot(m,d,xlab="Savings residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_4)['Savings'])

d <- residuals(lm(VotingPct ~ Savings + Female + PopDensity, train_clinton))
m <- residuals(lm(Poverty ~ Savings + Female + PopDensity, train_clinton))
plot(m,d,xlab="Poverty residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_4)['Poverty'])

d <- residuals(lm(VotingPct ~ Savings + Poverty + PopDensity, train_clinton))
m <- residuals(lm(Female ~ Savings + Poverty + PopDensity, train_clinton))
plot(m,d,xlab="Female residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_4)['Female'])

d <- residuals(lm(VotingPct ~ Savings + Poverty + Female, train_clinton))
m <- residuals(lm(PopDensity ~ Savings + Poverty + Female, train_clinton))
plot(m,d,xlab="PopDensity residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_4)['PopDensity'])

plot(fitted(lmod_clinton_4), residuals(lmod_clinton_4), xlab="Fitted", ylab="Residuals")
abline(h=0)
plot(fitted(lmod_clinton_4),sqrt(abs(residuals(lmod_clinton_4))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))))

qqnorm(residuals(lmod_clinton_4),ylab="Residuals",main="")
qqline(residuals(lmod_clinton_4))
shapiro.test(residuals(lmod_clinton_4))

dwtest(lmod_clinton_4)

#BIC Model Selection (Backwards, root transform)
clinton_back_BIC <- regsubsets(x,train_clinton$VotingPct^0.5,method='backward',nvmax=9)
clinton_back_BIC_summary <- summary(clinton_back_BIC)
plot(clinton_back_BIC_summary$bic, main='backward search: BIC')
which.min(clinton_back_BIC_summary$bic)

#Filtered Model, root response
lmod_clinton_5 <- lm(I(VotingPct^0.5) ~ Savings + Poverty + Veterans + Female + PopDensity, train_clinton)
summary(lmod_clinton_5)

x3 <- model.matrix(lmod_clinton_5)[,-1]
vif(x3)

d <- residuals(lm(VotingPct^0.5 ~ Poverty + Veterans + Female + PopDensity, train_clinton))
m <- residuals(lm(Savings ~ Poverty + Veterans + Female + PopDensity, train_clinton))
plot(m,d,xlab="Savings residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_5)['Savings'])

d <- residuals(lm(VotingPct^0.5 ~ Savings + Veterans + Female + PopDensity, train_clinton))
m <- residuals(lm(Poverty ~ Savings + Veterans + Female + PopDensity, train_clinton))
plot(m,d,xlab="Poverty residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_5)['Poverty'])

d <- residuals(lm(VotingPct^0.5 ~ Savings + Poverty + Female + PopDensity, train_clinton))
m <- residuals(lm(Veterans ~ Savings + Poverty + Female + PopDensity, train_clinton))
plot(m,d,xlab="Veterans residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_5)['Veterans'])

d <- residuals(lm(VotingPct^0.5 ~ Savings + Poverty + Veterans + PopDensity, train_clinton))
m <- residuals(lm(Female ~ Savings + Poverty + Veterans + PopDensity, train_clinton))
plot(m,d,xlab="Female residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_5)['Female'])

d <- residuals(lm(VotingPct^0.5 ~ Savings + Poverty + Veterans + Female, train_clinton))
m <- residuals(lm(PopDensity ~ Savings + Poverty + Veterans + Female, train_clinton))
plot(m,d,xlab="PopDensity residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_5)['PopDensity'])

plot(fitted(lmod_clinton_5), residuals(lmod_clinton_5), xlab="Fitted", ylab="Residuals")
abline(h=0)
plot(fitted(lmod_clinton_5),sqrt(abs(residuals(lmod_clinton_5))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))))

qqnorm(residuals(lmod_clinton_5),ylab="Residuals",main="")
qqline(residuals(lmod_clinton_5))
shapiro.test(residuals(lmod_clinton_5))

dwtest(lmod_clinton_5)


#BIC Model Selection (Backwards, 3/4 transform)
clinton_back_BIC <- regsubsets(x,train_clinton$VotingPct^0.75,method='backward',nvmax=9)
clinton_back_BIC_summary <- summary(clinton_back_BIC)
plot(clinton_back_BIC_summary$bic, main='backward search: BIC')
which.min(clinton_back_BIC_summary$bic)

#Filtered Model, 3/4 response
lmod_clinton_6 <- lm(I(VotingPct^0.75) ~ Savings + Poverty + Female + PopDensity, train_clinton)
summary(lmod_clinton_6)

x4 <- model.matrix(lmod_clinton_6)[,-1]
vif(x4)

d <- residuals(lm(VotingPct^0.75 ~ Poverty + Female + PopDensity, train_clinton))
m <- residuals(lm(Savings ~ Poverty + Female + PopDensity, train_clinton))
plot(m,d,xlab="Savings residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_6)['Savings'])

d <- residuals(lm(VotingPct^0.75 ~ Savings + Female + PopDensity, train_clinton))
m <- residuals(lm(Poverty ~ Savings + Female + PopDensity, train_clinton))
plot(m,d,xlab="Poverty residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_6)['Poverty'])

d <- residuals(lm(VotingPct^0.75 ~ Savings + Poverty + Veterans + PopDensity, train_clinton))
m <- residuals(lm(Female ~ Savings + Poverty + Veterans + PopDensity, train_clinton))
plot(m,d,xlab="Female residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_6)['Female'])

d <- residuals(lm(VotingPct^0.75 ~ Savings + Poverty + Veterans + Female, train_clinton))
m <- residuals(lm(PopDensity ~ Savings + Poverty + Veterans + Female, train_clinton))
plot(m,d,xlab="PopDensity residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_6)['PopDensity'])

plot(fitted(lmod_clinton_6), residuals(lmod_clinton_6), xlab="Fitted", ylab="Residuals")
abline(h=0)
plot(fitted(lmod_clinton_6),sqrt(abs(residuals(lmod_clinton_6))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))))

qqnorm(residuals(lmod_clinton_6),ylab="Residuals",main="")
qqline(residuals(lmod_clinton_6))
shapiro.test(residuals(lmod_clinton_6))

dwtest(lmod_clinton_6)

#Filtered Model, 3/4 response and square PopDensity
lmod_clinton_7 <- lm(I(VotingPct^0.75) ~ Savings + Poverty + Female + PopDensity + I(PopDensity^2), train_clinton)
summary(lmod_clinton_7)

x5 <- model.matrix(lmod_clinton_7)[,-1]
vif(x5)

d <- residuals(lm(VotingPct ~ Poverty + Female + PopDensity + I(PopDensity^2), train_clinton))
m <- residuals(lm(Savings ~ Poverty + Female + PopDensity, train_clinton))
plot(m,d,xlab="Savings residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_6)['Savings'])

d <- residuals(lm(VotingPct ~ Savings + Female + PopDensity, train_clinton))
m <- residuals(lm(Poverty ~ Savings + Female + PopDensity, train_clinton))
plot(m,d,xlab="Poverty residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_6)['Poverty'])

d <- residuals(lm(VotingPct ~ Savings + Poverty + Veterans + PopDensity, train_clinton))
m <- residuals(lm(Female ~ Savings + Poverty + Veterans + PopDensity, train_clinton))
plot(m,d,xlab="Female residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_6)['Female'])

d <- residuals(lm(VotingPct ~ Savings + Poverty + Veterans + Female, train_clinton))
m <- residuals(lm(PopDensity ~ Savings + Poverty + Veterans + Female, train_clinton))
plot(m,d,xlab="PopDensity residuals",ylab="VotingPct Residuals")
abline(0,coef(lmod_clinton_6)['PopDensity'])

plot(fitted(lmod_clinton_6), residuals(lmod_clinton_6), xlab="Fitted", ylab="Residuals")
abline(h=0)
plot(fitted(lmod_clinton_6),sqrt(abs(residuals(lmod_clinton_6))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))))

qqnorm(residuals(lmod_clinton_6),ylab="Residuals",main="")
qqline(residuals(lmod_clinton_6))
shapiro.test(residuals(lmod_clinton_6))

dwtest(lmod_clinton_6)

lmod_clinton_final <- lmod_clinton_6
