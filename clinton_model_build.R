install.packages("lmtest")
library("lmtest")
library("faraway")
library("MASS")
library("leaps")


#read data
clinton <- read.csv("https://raw.githubusercontent.com/briannaeskin/RegressionAndTimeSeriesFall2020/main/ClintonElectionData1992.csv", header = TRUE, row.names = 1)


#Leverage/Outliers/Influential (Full Data)
#Leverage
lmod_clinton <- lm(VotingPct ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, clinton)
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

lmod_clinton_2 <- lm(VotingPct ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, clinton, subset=(cook < max(cook)))
summary(lmod_clinton_2)
summary(lmod_clinton)


#split to test and train data
sample_size <- floor(0.8*nrow(clinton))
set.seed(001)

data_sample <- sample(seq_len(nrow(clinton)),size=sample_size)
train_clinton <- clinton[data_sample,]
test_clinton <- clinton[-data_sample,]

lmod_clinton <- lm(VotingPct^0.75 ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, train_clinton)
summary(lmod_clinton)

#Variance Inflation Factor
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
dwtest(lmod_clinton)

#Box-Cox
boxcox(lmod_clinton,plotit=T,lambda=seq(0.5,1.5,by=0.1))


#Partial Regression Plots
d <- residuals(lm(VotingPct ~ Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, train_clinton))
m <- residuals(lm(MedAge ~ Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, train_clinton))
plot(m,d,xlab="Median Age residuals",ylab="VotingPct Residuals")


#BIC Model Selection (Backwards)
clinton_back_BIC <- regsubsets(x,train_clinton$VotingPct^0.75,method='backward',nvmax=9)
clinton_back_BIC_summary <- summary(clinton_back_BIC)
plot(clinton_back_BIC_summary$bic, main='backward search: BIC')
which.min(clinton_back_BIC_summary$bic)

#BIC Model Selection (Forwards)
clinton_forward_BIC <- regsubsets(x,train_clinton$VotingPct^0.75,method='forward',nvmax=9)
clinton_forward_BIC_summary <- summary(clinton_forward_BIC)
plot(clinton_forward_BIC_summary$bic, main='forward search: BIC')
which.min(clinton_forward_BIC_summary$bic)

lmod_clinton_filtered <- lm(VotingPct^0.75 ~ Savings + Poverty + Female + PopDensity, train_clinton)
summary(lmod_clinton_filtered)

#Now repeat other analysis
x <- model.matrix(lmod_clinton_filtered)[,-1]
vif(x)

plot(fitted(lmod_clinton_filtered), residuals(lmod_clinton_filtered), xlab="Fitted", ylab="Residuals")
abline(h=0)
plot(fitted(lmod_clinton_filtered),sqrt(abs(residuals(lmod_clinton_filtered))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))))

qqnorm(residuals(lmod_clinton_filtered),ylab="Residuals",main="")
qqline(residuals(lmod_clinton_filtered))
shapiro.test(residuals(lmod_clinton_filtered))

dwtest(lmod_clinton)

boxcox(lmod_clinton,plotit=T,lambda=seq(0.5,1.5,by=0.1))

#Partial Regression Plots
d <- residuals(lm(VotingPct^0.75 ~ Poverty + Female + PopDensity, train_clinton))
m <- residuals(lm(Savings ~ Poverty + Female + PopDensity, train_clinton))
plot(m,d,xlab="Savings residuals",ylab="VotingPct Residuals")

d <- residuals(lm(VotingPct^0.75 ~ Savings + Female + PopDensity, train_clinton))
m <- residuals(lm(log(Poverty) ~ Savings + Female + PopDensity, train_clinton))
plot(m,d,xlab="Savings residuals",ylab="VotingPct Residuals")

d <- residuals(lm(VotingPct^0.75 ~ Savings + Poverty + PopDensity, train_clinton))
m <- residuals(lm(log(Female) ~ Savings + Poverty + PopDensity, train_clinton))
plot(m,d,xlab="Savings residuals",ylab="VotingPct Residuals")

d <- residuals(lm(VotingPct^0.75 ~ Savings + Poverty + Female, train_clinton))
m <- residuals(lm(log(PopDensity) ~ Savings + Poverty + Female, train_clinton))
plot(m,d,xlab="Savings residuals",ylab="VotingPct Residuals")