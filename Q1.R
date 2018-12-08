# Loading necessary libraries and dataset
# install.packages("readr")
	
	library(readr)
	Telco_Customer_Churn <- read_csv("Telco-Customer-Churn.csv")


#	1.1. What is the proportion of males/females in the dataset?
	gend <- Telco_Customer_Churn$gender
	gend_agreg <- aggregate(data.frame(count = gend), list(value = gend), length)
	gend_ratio <- gend_agreg$count[2]/gend_agreg$count[1]

	cat('The ratio of male/femal in data is: ',gend_agreg$count[2],'/' ,gend_agreg$count[1],' or ', round(gend_ratio, 4),'\n')

#	1.2. What is the proportion of senior citizens in the dataset? 
	Sen_Cit <- Telco_Customer_Churn$SeniorCitizen	
	SC_agreg <- aggregate(x = data.frame(Sen_Cit), list(value = Sen_Cit),length)
	Sen_Cit_ratio <- round(SC_agreg$Sen_Cit[2]/sum(SC_agreg$Sen_Cit),4)
	
	cat('The Proportion of Senior Citizens in data is: ',Sen_Cit_ratio,' or ',Sen_Cit_ratio*100 ,'%\n')

#	1.3. What is the most common type of contract in the dataset? 
	Contract <- Telco_Customer_Churn$Contract
	Cont_agreg <- aggregate(x = data.frame(Contract), list(value=Contract),length)
	max_Cont_field <- Cont_agreg$value[which.max(Cont_agreg$Contract)]
	
	cat('The most common type of contract in the dataset is: ',max_Cont_field,'\n')

#	1.4. What is the most common type of internet service in the dataset? 
	Int_Serv <- Telco_Customer_Churn$InternetService
	IS_agreg <- aggregate(x = data.frame(Int_Serv), list(value=Int_Serv),length)
	max_IS_field <- IS_agreg$value[which.max(IS_agreg$Int_Serv)]
	
	cat('The most common type of internet service in the dataset is: ',max_IS_field,'\n')
	
#	1.5. What is the least common payment method in the dataset? 
	Pay_Method <- Telco_Customer_Churn$PaymentMethod
	PM_agreg <- aggregate(x = data.frame(Pay_Method), list(value=Pay_Method),length)
	min_PM_field <- PM_agreg$value[which.min(PM_agreg$Pay_Method)]
	
	cat('The least common payment method in the dataset is: ',min_PM_field,'\n')
	
#	1.6. What is the average tenure of male and female customers?
	Ten_Cal <- Telco_Customer_Churn[, c(2,6)]
	males_ten = subset(Ten_Cal, subset = (Ten_Cal[,1] == "Male"))
	female_ten = subset(Ten_Cal, subset = (Ten_Cal[,1] == "Female"))
	male_ave = round(sum(males_ten$tenure)/length(males_ten$tenure),4)
	female_ave = round(sum(female_ten$tenure)/length(female_ten$tenure),4)

	cat('The average tenures of males and females are ',male_ave,' & ',female_ave,' respectively.\n')
	
#	1.7. Is Streaming TV favored over the Streaming Movies service?
	TV_count = length(subset(Telco_Customer_Churn$StreamingTV,subset = (Telco_Customer_Churn$StreamingTV == 'Yes')))
	Mov_count = length(subset(Telco_Customer_Churn$StreamingMovies,subset = (Telco_Customer_Churn$StreamingMovies == 'Yes')))
	
	cat('Is Streaming TV favored over the Streaming Movies service? \t', ifelse(TV_count > Mov_count,"YES!","NO!"),'\n')

#	1.8. How many customers churned out of the total customers? 
	churn_count = length(subset(Telco_Customer_Churn$Churn,subset = (Telco_Customer_Churn$Churn == 'Yes')))
	
	cat(churn_count ,'customers churned out of the total customers!,\n')

# 1.9. What is the mean “monthly charges” amongst customers with month-to-month contracts? 
	Cont_MonCharge = Telco_Customer_Churn[,c('Contract','MonthlyCharges')]
	MonChages_for_montomon = subset(Cont_MonCharge,Cont_MonCharge$Contract == "Month-to-month")
	
	cat('The mean “monthly charges” amongst customers with month-to-month contracts is:',round(mean(MonChages_for_montomon$MonthlyCharges),4),'\n')
	
# 1.10.	What is the gender wise average of “total charges” amongst customers? 
	Gend_Charges = Telco_Customer_Churn[,c('gender','TotalCharges')]
	male_charges = subset(Gend_Charges, Gend_Charges$gender == "Male")
	female_charges = subset(Gend_Charges, Gend_Charges$gender == "Female")
	male_charges_ave = round(sum(male_charges$TotalCharges,na.rm = TRUE)/length(male_charges$TotalCharges),4)	
	female_charges_ave = round(sum(female_charges$TotalCharges,na.rm = TRUE)/length(female_charges$TotalCharges),4)	
	
	gend_wise_ave = data.frame(c("Male","Female"),c(male_charges_ave,female_charges_ave))
	colnames(gend_wise_ave) <- c("Gender","Total Charges Average")
	show(gend_wise_ave)
