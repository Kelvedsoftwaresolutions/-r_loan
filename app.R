# libraries
library(caTools)
library(caret)
library(e1071)
library(rpart)
library(randomForest)
##

# dataset original
loan_dataset_original <- read.csv(file.choose())
test_csv <- read.csv(file.choose())
##

# dataset
loan_dataset <- loan_dataset_original
View(loan_dataset)
##

# analysis
head(loan_dataset)
colnames(loan_dataset)
dim(loan_dataset)
summary(loan_dataset) # No NA's in any
str(loan_dataset)
boxplot(loan_dataset$Age)
boxplot(loan_dataset$Experience)
boxplot(loan_dataset$Income)
boxplot(loan_dataset$Family)
boxplot(loan_dataset$CCAvg)
boxplot(loan_dataset$Education)
boxplot(loan_dataset$Mortgage)
boxplot(loan_dataset$PersonalLoan)
##

# pre processing
# selecting experience freater than 0
loan_dataset = subset(loan_dataset, loan_dataset$Experience >= 0)
# removed zipcode
loan_dataset = loan_dataset[,-4]
##

# dataset splitting
set.seed(200)
split = sample.split(loan_dataset$CreditCard, SplitRatio = 0.75)
table(split)
training = subset(loan_dataset, split == T)
test = subset(loan_dataset, split == F)
##

# logistic regression
lr_model = glm(CreditCard~., data = training, family = "binomial")
summary(lr_model)
lr_predict = predict(lr_model, newdata = test, type = "response")
lr_predict
lr_result = ifelse(lr_predict > 0.5, 1, 0)
cm_lr = table(test$CreditCard, lr_result)
confusionMatrix(cm_lr) # 0.73 accuracy
##

# desicion tree
dt_model = rpart(CreditCard~., data = training)
summary(dt_model)
dt_predict = predict(dt_model, newdata = test)
dt_predict
dt_result = ifelse(dt_predict > 0.5, 1, 0)
cm_dt = table(test$CreditCard, dt_result)
cm_dt
confusionMatrix(cm_dt) # 0.73 accuracy
##

# random forest
rf_model = randomForest(CreditCard~., data=training, ntree = 1000)
rf_predict = predict(rf_model, newdata = test)
rf_predict
rf_result = ifelse(rf_predict > 0.5, 1, 0)
cm_rf = table(test$CreditCard, rf_result)
confusionMatrix(cm_rf) # 0.73
##

# support vector machine

##