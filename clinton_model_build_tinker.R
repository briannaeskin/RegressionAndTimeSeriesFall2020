lmod_clinton_0.5 <- lm(VotingPct^0.5 ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, clinton)
summary(lmod_clinton_0.5)

#VIF Check
x_0.5 <- model.matrix(lmod_clinton_0.5)[,-1]
vif(x_0.5)

#Error Assumptions
#Constant Variance
plot(fitted(lmod_clinton_0.5), residuals(lmod_clinton_0.5), xlab="Fitted", ylab="Residuals")
abline(h=0)
plot(fitted(lmod_clinton_0.5),sqrt(abs(residuals(lmod_clinton_0.5))),xlab="Fitted",ylab=expression(sqrt(hat(epsilon))))
#Normality
qqnorm(residuals(lmod_clinton_0.5),ylab="Residuals",main="")
qqline(residuals(lmod_clinton_0.5))
shapiro.test(residuals(lmod_clinton_0.5))
#Correlated Errors
n <- length(residuals(lmod_clinton_0.5))
plot(tail(residuals(lmod_clinton_0.5),n-1) ~ head(residuals(lmod_clinton_0.5),n-1),
     xlab = expression(hat(epsilon)[i]), ylab = expression(hat(epsilon)[i+1]),main="Successive Fitted Errors")
abline(h=0,v=0,col=grey(0.75))
dwtest(lmod_clinton_0.5)




lmod_clinton_0.75 <- lm(VotingPct^0.75 ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, clinton)
summary(lmod_clinton_0.75)

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

#BIC Model Selection (Backwards, no transform)
clinton_back_BIC <- regsubsets(model.matrix(lmod_clinton)[,-1],clinton$VotingPct,method='backward',nvmax=9)
clinton_back_BIC_summary <- summary(clinton_back_BIC)
plot(clinton_back_BIC_summary$bic, main='backward search: BIC')
which.min(clinton_back_BIC_summary$bic)


lmod_clinton <- lm(VotingPct ~ Savings + Poverty + Veterans + Female + PopDensity, clinton)
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

#Check Without Hudson
lmod_clinton_0.75_2 <- lm(VotingPct^0.75 ~ Savings + Poverty + Veterans + Female + PopDensity, clinton, subset=(cook < max(cook)))
summary(lmod_clinton_0.75_2)
summary(lmod_clinton_0.75)

#Check Partial Residual Plots
#With
d <- residuals(lm(VotingPct^0.75 ~ Savings + Poverty + Veterans + Female, clinton))
m <- residuals(lm(PopDensity ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + NursingHome + Crime, clinton))
plot(m,d,xlab="PopDensity residuals With Hudson",ylab="VotingPct^0.75 Residuals")
abline(0,coef(lmod_clinton_0.75_2)['PopDensity'])
head(sort(abs(m),decreasing=T))
#Without
d <- residuals(lm(VotingPct^0.75 ~ Savings + Poverty + Veterans + Female, clinton,subset=(cook < max(cook))))
m <- residuals(lm(PopDensity ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + NursingHome + Crime, clinton, subset=(cook < max(cook))))
plot(m,d,xlab="PopDensity residuals Without Hudson",ylab="VotingPct^0.75 Residuals")
abline(0,coef(lmod_clinton_0.75_2)['PopDensity'])
head(sort(abs(m),decreasing=T))
