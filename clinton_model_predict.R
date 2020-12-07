#This script is for the train/test split (3:1) and analysis

library(tidyverse)

set.seed(123)
sample_clinton <- sort(sample(nrow(clinton), floor(nrow(clinton)*.75)))
train_clinton <- clinton[sample_clinton,]
test_clinton <- clinton[-sample_clinton,]

lmod_clinton_train <- lm(VotingPct ~ Savings + Poverty + Veterans + Female + PopDensity, train_clinton)
summary(lmod_clinton_train)

fitted_clinton_train <- fitted(lmod_clinton_train,train_clinton)
fitted_clinton_test <- predict(lmod_clinton_train, test_clinton)
predict_clinton_test <- predict(lmod_clinton_train, test_clinton, interval="prediction")
conf_clinton_test <- predict(lmod_clinton_train, test_clinton, interval="confidence")

head(fitted_clinton_test)
head(predict_clinton_test)
head(conf_clinton_test)

rmse <- function(x,y) sqrt(mean((x-y)^2))
rmse(fitted_clinton_train, train_clinton$VotingPct)
rmse(fitted_clinton_test, test_clinton$VotingPct)
rmse(predict_clinton_test,test_clinton$VotingPct)
rmse(conf_clinton_test,test_clinton$VotingPct)

predict_clinton_test <- cbind(predict_clinton_test,test_clinton$VotingPct) %>%
  data.frame()
fitted_clinton_train <- cbind(fitted_clinton_train,train_clinton$VotingPct) %>%
  data.frame()

ggplot(data=predict_clinton_test,mapping=aes(x=V4,y=fit,ymin=lwr,ymax=upr)) +
  geom_point() +
  geom_abline(intercept=0,slope=1) +
  geom_ribbon(alpha=0.5)

chisq.test(c(16,132,255,160,80,20,10,3), p=c(10/676,127/676,257/676,175/676,72/676,21/676,8/676,6/676))
