# Loading necessary libraries and dataset
# 	install.packages("readr")
	library(readr)
	ChurnCsv <- read_csv("Telco-Customer-Churn.csv")

# Checking for columns with misshing values
	cols_having_Null = colnames(ChurnCsv)[colSums(is.na(ChurnCsv)) > 0]
	show(cols_having_Null)
	
# Average of column TotalCharges with missing values
	average = round(mean(ChurnCsv$TotalCharges,na.rm = TRUE),4)
	
# Filling the missing values	
	for(i in 1:nrow(ChurnCsv[,cols_having_Null])){
		if (is.na(ChurnCsv[i,cols_having_Null])){
			ChurnCsv[i,cols_having_Null] <- average
		}
	}

# Training and test split
	library(caTools)
	original_outcomes = table(ChurnCsv$Churn)
	original_outcomes
	sample = sample.split(ChurnCsv$Churn,SplitRatio = 0.7)
	train = subset(ChurnCsv,sample)
	test = subset(ChurnCsv,!sample)
	
# 2.1. Our baseline model in classification is to always predict the most frequent outcome in the training set. What is the most frequent outcome?
	test_set_outcome <- table(test$Churn)
	train_set_outcome <- table(train$Churn)
	most_freq_outcome <- names(which.max(train_set_outcome))
	
	cat('The most frequent outcome in training set is \t\"',most_freq_outcome,'\"\n',sep = '')
	
# 2.2. What is the accuracy of this baseline model on the training set?
	train_set_accuracy <- unname(train_set_outcome[most_freq_outcome])/sum(unname(train_set_outcome))
	
	cat('The accuracy of this baseline model on the training set is:',round(train_set_accuracy * 100,4),'%\n',sep = '')
	
# 2.3. What is the accuracy of this baseline model on the test set?
	test_set_accuracy <- unname(test_set_outcome[most_freq_outcome])/sum(unname(test_set_outcome))
	
	cat('The accuracy of this baseline model on the test set is:',round(test_set_accuracy * 100,4),'%\n',sep = '')
	
# 2.4. What is the true positive (TP) rate of the baseline model on the test set? 
	
