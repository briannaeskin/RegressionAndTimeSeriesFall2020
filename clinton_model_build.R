library("faraway")

#read data
clinton <- read.csv("https://raw.githubusercontent.com/briannaeskin/RegressionAndTimeSeriesFall2020/main/ClintonElectionData1992.csv", header = TRUE, row.names = 1)

#Leverage/Outliers/Influential (Full Data)
#Leverage
lmod_clinton <- lm(VotingPct ~ MedAge + Savings + PerCapIncome + Poverty + Veterans + Female + PopDensity + NursingHome + Crime, clinton)
hatv <- hatvalues(lmod_clinton)
counties <- row.names(clinton)
halfnorm(hatv, labs=counties,ylab="Leverages")


#split to test and train data
sample_size <- floor(0.8*nrow(clinton))
set.seed(777)

data_sample <- sample(seq_len(nrow(clinton)),size=sample_size)
train_clinton <- clinton[data_sample,]
test_clinton <- clinton[-data_sample,]

