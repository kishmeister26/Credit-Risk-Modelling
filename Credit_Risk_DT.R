#Decision Tree
install.packages("rpart")
library(rpart)
library(rpart.plot)

#Undersampling the training set with rpart.control

tree_undersample <- rpart(loan_status ~ age + int_rate + grade 
                          + loan_amnt +
                            annual_inc
                          , method = "class", 
                          data = loan_train, 
                          control = rpart.control(cp = 0.001))

plot(tree_undersample, uniform = T)

#Tree with adusted prior probabilities

tree_prior <- rpart(loan_status ~ ., method = "class", 
                    data = loan_train, parms = 
                      list(prior=c(0.7,0.3 )),
                    control = rpart.control(cp = 0.001))

plot(tree_prior)
text(tree_prior)

#Pruning the Decision Tree
set.seed(345)
tree_prior <- rpart(loan_status ~ ., method = "class", 
                    data = loan_train, parms = list(prior = c(0.7,0.3)),
                    control = rpart.control(cp = 0.001))
plotcp(tree_prior)

tree_min <- tree_prior$cptable[which.min(tree_prior$cptable[, "xerror"])
                               ,"CP"]
ptree_prior <- prune(tree_prior, cp = tree_min)
prp(ptree_prior)

#Predictions
pred_undersmaple <- predict(tree_undersample, loan_test, type = "class")
pred_prior <- predict(tree_prior, loan_test, type = "class")

#confusion matrix
confmat_undersample <- table(loan_test$loan_status, pred_undersmaple)
confmat_prior <- table(loan_test$loan_status, pred_prior)

#accuracy
acc_undersample <- sum(diag(confmat_undersample)) / nrow(loan_test)
acc_prior <- sum(diag(confmat_prior)) / nrow(loan_test)

acc_undersample # 88.8%
acc_prior #85.4%
