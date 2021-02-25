setwd("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800")
getwd()

name <- paste("Reco_dataset_clean",".csv",sep = "")
xactions <- read.csv(name)

source(file.path("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800", "FA800_Helper.r"))

user_data <- getUserdata(xactions)

colnames(user_data)

hist(user_data$user_age, main = "Histogram-User Age", xlab = "User Age")
plot(as.factor(user_data$user_gender), main = "Gender Proportion")
hist(user_data$num_visits,main = "Histogram- Per User Visits", 
     xlab = "User Visits")

hist(user_data$avg_visit_days, main = "Histogram - Average Days between visits", 
     xlab = "Average Days between visits")
plot(as.factor(xactions$item_name), main = "Products/ Services Used")



