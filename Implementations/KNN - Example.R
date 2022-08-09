library(class)
library(gmodels)
library(crosstable)

normalize <- function(x){
  return((x - min(x))/(max(x)-min(x)))
}

wbcd <-  read.csv("/Users/themiskavour/Documents/My_Projects/wisc_bc_data.csv", stringsAsFactors = FALSE)

str(wbcd)

#Dropping the ID columns because it is unique value and can result overfitting
wbcd <- wbcd[-1]

# Let's take a closer look at diagnosis because this is the one variable we are trying to predict
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"),labels = c("Benign","Malignant"))
round(prop.table(table(wbcd$diagnosis))*100,digits = 1)

# Other feature
summary(wbcd[c("radius_mean","area_mean", "smoothness_mean")])
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)

wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]

wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)

CrossTable(x = wbcd_test_labels , y = wbcd_test_pred , prop.chisq = FALSE)

#Improving the result
wbcd_z <- as.data.frame(scale(wbcd[-1]))

summary(wbcd_z$area_mean)

wbcd_z_train <- wbcd_z[1:469,]
wbcd_z_test <- wbcd_z[470:569,]

wbcd_z_train_labels <- wbcd[1:469,1]
wbcd_z_test_labels <- wbcd[470:569,1]

wbcd_z_test_pred <- knn(train = wbcd_z_train, test = wbcd_z_test, cl = wbcd_z_train_labels, k = 21)
CrossTable(x = wbcd_z_test_labels, y = wbcd_z_test_pred, prop.chisq = FALSE)
