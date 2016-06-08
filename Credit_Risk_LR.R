#Data Splitting into train and test data

set.seed(567)

#Row numbers for training set
index_train <- sample(1:nrow(loan2), 2/3 * nrow(loan2)) #2/3 of dataset

loan_train <- loan2[index_train, ]
loan_test <- loan2[-index_train, ]

#Logistic Regression Model

lr_loan <- glm(loan_status~ age + int_rate + grade + loan_amnt +
                 annual_inc, family = "binomial", data = loan_train )

summary(lr_loan)

loan_predict <- predict(lr_loan, loan_test, type = "response")
range(loan_predict)

#Logistic Regression Model with all variables

lr_loan_all <- glm(loan_status ~ ., family = "binomial", data = loan_train)

loan_predict_all <- predict(lr_loan_all, loan_test, type ="response")
range(loan_predict_all, na.rm = T)

#Cut off of 15%

lr_cutoff <- ifelse(loan_predict > 0.15, 1, 0)

#confusion Matrix
tab_cm <- table(loan_test$loan_status, lr_cutoff)

# Computing Accuracy
acc_logit <- sum(diag(tab_cm)) / nrow(loan_test)
acc_logit
#74.2% model accuracy
