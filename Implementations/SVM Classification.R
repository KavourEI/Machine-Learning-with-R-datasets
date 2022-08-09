library(tidyverse)
# install.packages("kernlab")
library(kernlab)

letters <- read.csv('/Users/themiskavour/Documents/GitHub_Repos_WIP/Machine-Learning-with-R-datasets/letterdata.csv', stringsAsFactors = TRUE)
str(letters)

letters_train <- letters[1:16000,]
letters_test <- letters[16001:20000,]

letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")

letter_classifier

letter_predict <- predict(letter_classifier, letters_test)

table(letter_predict, letters_test$letter)

agreement <- letter_predict ==letters_test$letter
table(agreement)
prop.table(table(agreement))

letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")
letter_predict_rbf <- predict(letter_classifier_rbf, letters_test)
agreement_rbf <- letter_predict_rbf == letters_test$letter
table(agreement_rbf)
prop.table(table(agreement_rbf))

cost_values <- c(1, seq(from = 5, to = 40, by = 5))
start_time <- Sys.time()
accuracy_values <- sapply(cost_values, function(x){
  set.seed(12345)
  m <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot", C = x)
  pred <- predict(m, letters_test)
  agree <- ifelse(pred == letters_test$letter,1,0)
  accuracy <- sum(agree)/nrow(letters_test)
  return(accuracy)
})
end_time <- Sys.time()
duration <- end_time - start_time
duration
plot(cost_values, accuracy_values, type = 'b')

letter_classifier_rbf_c10 <- ksvm(letter~ ., data = letters_train, kernel = "rbfdot", C = 10)
letter_predict_rbf_c10 <- predict(letter_classifier_rbf_c10, letters_test)
agreement_rbf_c10 <- letter_predict_rbf_c10 == letters_test$letter
prop.table(table(agreement_rbf_c10))
