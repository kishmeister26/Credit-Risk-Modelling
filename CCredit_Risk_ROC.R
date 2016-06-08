install.packages("pROC")
library(pROC)

#ROC curve for Logistic Regression
roc_logit <- roc(loan_test$loan_status, loan_predict)

plot(roc_logit)

auc(roc_logit)  #0.658


#ROC curve for Decision Tree
roc_tree <- roc(loan_test$loan_status, pred_undersmaple)
roc_tree_2 <- roc(loan_test$loan_status, pred_prior)
lines(roc_tree, col = "blue")
lines(roc_tree_2, col = "green")


#AUC 
auc(roc_tree) #0.5997
auc(roc_tree_2) #0.6016