library(C50)
library(gmodels)

credit <- read.csv("/Users/themiskavour/Documents/GitHub_Repos_WIP/Machine-Learning-with-R-datasets/credit.csv")
str(credit)

table(credit$checking_balance)
table(credit$savings_balance)

summary(credit$months_loan_duration)
summary(credit$amount)

table(credit$default)

# Splitting to trainning and testing
RNGversion("3.5.2");set.seed(123)
train_sample <- sample(1000,900)
str(train_sample)

credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# Construction of the model
credit_model <- C5.0(credit_train[-17], as.factor(credit_train$default))

credit_model
summary(credit_model)

# Evaluation
credit_predict <- predict(credit_model, credit_test)
CrossTable(credit_test$default, credit_predict,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Default', 'Preedicted Default'))

# Bossting the tree to increase performance
credit_boost10 <- C5.0(credit_train[-17], as.factor(credit_train$default),
                       trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost10_pred <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost10_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('Actual Default', 'Predicted Default'))
