library(readxl) # read excel files with data
library(tm) # text mining package
library(SnowballC) # stemmers
library(wordcloud) # wordcloud plotting
library(e1071) # Naive Bayes Classifier
library(gmodels) # CrossTable

convert_counts <- function(x){
  x <- ifelse(x>0, "Yes", "No")
}

sms_raw <- read.csv("/Users/themiskavour/Documents/GitHub_Repos_WIP/Machine-Learning-with-R-datasets/sms_spam.csv", stringsAsFactors = FALSE)

# Let's take a closer look on the features of sms_raw.
str(sms_raw)

# We see that the fature called type should become factor as it has two values ("ham" and "spam")
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

# Create a corpus containg the text SMSs in order to clean them.
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# To see the text that is hidden in an element of a corpus we use the following method
as.character(sms_corpus[[1]])

# Now it is time for us to clean those objects
# Initially we will make all the characters lowercase
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
as.character(sms_corpus_clean[[1]])

# Second step is to remove the numbers from the SMSs
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

# Now it is time to remove the stop words (like and, or, but etc)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

# We continue by removing the punctuation as well
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)

# Stem the words of our document, meaning learned, leans and other variations, will all become learn. 
# Not a good idea here, so we are not going to apply it
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

# Remove additional whitespace
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# Splitting the data to Training and Test data
sms_dtm_train <- sms_dtm[1:4169,]
sms_dtm_test <- sms_dtm[4170:5574,]
sms_train_labels <- sms_raw[1:4169,]$type
sms_test_labels <- sms_raw[4170:5574,]$type

# Wordcloud experimentations
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

findFreqTerms(sms_dtm_train,5)
sms_freq_words <- findFreqTerms(sms_dtm_train,5)

sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[, sms_freq_words]

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

# Naive Bayes Classifier

sms_classifier <- naiveBayes(sms_train, sms_train_labels)

sms_test_predict <- predict(sms_classifier, sms_test)
CrossTable(sms_test_predict, sms_test_labels,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c("Predicted","Actual"))

sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)
sms_test_predict2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_predict2, sms_test_labels,
           prop.chisq = FALSE, prop.r = FALSE, prop.c = FALSE,
           dnn = c("Predicted", "Actual"))
