#Test - lambda = 0.5, full model
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

#Test lambda = 0.75, full model
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

