#This script is for the train/test split (3:1) and analysis

library(tidyverse)

floor(nrow(clinton)*0.75)
train_clinton <- clinton[1:2027,]
test_clinton <- clinton[2028:2703,]

lmod_clinton_train <- lm(VotingPct^0.75 ~ Savings + Poverty + Veterans + Female + PopDensity, train_clinton)
summary(lmod_clinton_train)

fitted_clinton_train <- fitted(lmod_clinton_train,train_clinton)
fitted_clinton_train <- fitted_clinton_train^(4/3)
fitted_clinton_test <- predict(lmod_clinton_train, test_clinton)
fitted_clinton_test <- fitted_clinton_test^(4/3)
predict_clinton_test <- predict(lmod_clinton_train, test_clinton, interval="prediction")
conf_clinton_test <- predict(lmod_clinton_train, test_clinton, interval="confidence")
predict_clinton_test <- predict_clinton_test^(4/3)
conf_clinton_test <- conf_clinton_test^(4/3)

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
conf_clinton_test <- cbind(conf_clinton_test,test_clinton$VotingPct) %>%
  data.frame()

ggplot(data=predict_clinton_test, mapping=aes(x=fit, y=V4)) +
  geom_point() +
  geom_abline(aes(intercept=0,slope=1)) +
  geom_ribbon(data=predict_clinton_test, aes(ymin=lwr, ymax=upr, fill='prediction'), alpha=0.3) +
  geom_smooth(data=conf_clinton_test,method="lm")
