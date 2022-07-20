library(C50)

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

# Constraction of the model
# 