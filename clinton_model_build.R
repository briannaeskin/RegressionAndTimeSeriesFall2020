#read data
clinton <- read.csv("https://raw.githubusercontent.com/briannaeskin/RegressionAndTimeSeriesFall2020/main/ClintonElectionData1992.csv")

#split to test and train data
sample_size <- floor(0.8*nrow(clinton))
set.seed(777)

data_sample <- sample(seq_len(nrow(clinton)),size=sample_size)
train_clinton <- clinton[data_sample,]
test_clinton <- clinton[-data_sample,]

