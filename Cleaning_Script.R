
setwd("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800")
getwd()

name <- paste("amrs02_massagegreen_%_GuestTransactions",".csv",sep = "")
xactions <- read.csv(name)

source(file.path("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800", "FA800_Helper.r"))

colnames(xactions)

del_cols <- c("org_account_name","center_name","user_dob","user_wid",
              "user_zipcode","sale_price","median_num_user_visits",
              "stdev_num_user_visits","user_sales_rank",
              "user_sales_percent_rank","top_user_sales_percentile_value",
              "employee_name","total_user_sales")

del_ind <- find_col_ind(col_names = del_cols, xactions = xactions)
xactions <- xactions[,-del_ind]

colnames(xactions)

xactions <- gender_imp(xactions)

xactions[,"user_name"] <- as.factor(xactions[,"user_name"])
n <- length(levels(xactions[,"user_name"]))
levels(xactions[,"user_name"]) <- 1:n

xactions[,"item_name"] <- as.factor(xactions[,"item_name"])
n <- length(levels(xactions[,"item_name"]))
levels(xactions[,"item_name"]) <- 1:n

xactions[,"item_type_code"] <- as.factor(xactions[,"item_type_code"])
n <- length(levels(xactions[,"item_type_code"]))

levels(xactions[,"item_type_code"]) <- c("A","B","C","D","E","F",
                                         "G","H","I","J","K","L",
                                         "M","N","O","P")

xactions[,"item_category"] <- as.factor(xactions[,"item_category"])
n <- length(levels(xactions[,"item_category"]))

levels(xactions[,"item_category"]) <- 1:n


xactions[,"item_parent_category"] <- as.factor(xactions[,"item_parent_category"])
n <- length(levels(xactions[,"item_parent_category"]))

levels(xactions[,"item_parent_category"]) <- c("A","B","C","D","E","F",
                                               "G","H","I","J","K","L",
                                               "M","N","O","P","Q","R",
                                               "S","T","U","V","W","X")

colnames(xactions)[4] <- "user_id"

write.csv(xactions, file = "Recommendation_Dataset.csv",row.names = FALSE)


