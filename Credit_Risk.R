#Reading the data
loan <- readRDS("Loandata.rds")
View(loan)
names(loan)
dim(loan)
str(loan)
summary(loan)

#Data Exploration
install.packages("gmodels")
library(gmodels)

CrossTable(loan$loan_status) #11% are loan defaulters

CrossTable(loan$grade, loan$loan_status, prop.r = TRUE, 
           prop.c = F, prop.t = F, prop.chisq = F)

#Spotting outliers using histogram and scatterplots
hist_1 <- hist(loan$loan_amnt)
hist_1$breaks

hist_2 <- hist(loan$loan_amnt, breaks = 200, xlab = "Loan Amount", 
               main = "Histogram of the loan amount")

plot(loan$age, ylab = "Age")

outlier_index <- which(loan$age > 122) 

loan2 <- loan[-outlier_index,]
plot(loan2$age)

plot(loan$age, loan$annual_inc, xlab = "Age", ylab = "Annual Income")

summary(loan$int_rate) #2776 missing data
na_index <- which(is.na(loan$int_rate))
loan2 <- loan[-na_index, ]
sum(is.na(loan2$int_rate))


#Replacing missing values with median
median_ir <- median(loan$int_rate, na.rm = TRUE)
loan$int_rate[na_index] <- median_ir


outlier_index_ai <- which(loan2$annual_inc == 6000000)
loan2 <- loan2[-outlier_index_ai, ]
plot(loan2$age, loan2$annual_inc)

