library(dplyr)
library(stats)
library(gt)

teens <- read.csv("/Users/themiskavour/Documents/GitHub_Repos_WIP/Machine-Learning-with-R-datasets/snsdata.csv", stringsAsFactors = TRUE)
str(teens)

teens %>% 
  summarise(count = sum(is.na(.)))

table(teens$gender, useNA = 'ifany')

apply(apply(teens, MARGIN = 2, is.na), MARGIN = 2, sum)

summary(teens$age)

teens$age <- ifelse(teens$age >=13 & teens$age < 20,teens$age, NA)
summary(teens$age)

teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

mean(teens$age, na.rm = TRUE)
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)
av_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm=TRUE))
summary(ifelse(is.na(teens$age), av_age, teens$age))

interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests,scale))
summary(interests_z)


RNGversion("3.5.2")
set.seed(2345)
teen_cluster <- kmeans(interests_z, 5)

prop.table(teen_cluster$size)*100
centers1 <- teen_cluster$centers
centers2 <- as.data.frame(centers1)


(max_row_positions <- map_int(
  colnames(centers2),
  ~ pull(centers2, .) %>% which.max()
) %>% setNames(colnames(centers2)))

(maxstyles <- map2_chr(
  seq_along(max_row_positions), max_row_positions,
  ~ paste0("%>% tab_style( style = cell_fill(color = 'lightblue'),
           locations = cells_body(columns = ", .x, ",rows = ", .y, "))")
))

(code_to_run <- paste0("gt(centers2)", paste0(maxstyles, collapse = " ")))

eval(parse_expr(code_to_run))

teens$cluster <- teen_cluster$cluster

aggregate(data = teens, age ~ cluster, mean)
aggregate(data = teens, female ~ cluster, mean)
aggregate(data = teens, friends ~ cluster, mean)
