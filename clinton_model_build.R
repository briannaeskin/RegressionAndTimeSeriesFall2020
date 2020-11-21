install.packages("lmtest")
library("lmtest")
library("faraway")

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

lmod_clinton <- lm(VotingPct ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, train_clinton)


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
