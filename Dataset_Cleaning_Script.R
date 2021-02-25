
setwd("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800")
getwd()


name <- paste("Recommendation_Dataset",".csv",sep = "")
xactions <- read.csv(name)

source(file.path("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800", "FA800_Helper.r"))

######################################################################

colnames(xactions)

result <- remove_blank(xactions)
xactions <- result[[1]]
emp_cell <- result[[2]]


result <- remove_NA(xactions)
xactions <- result[[1]]
NA_cell <- result[[2]]
##### Convert columns into categorical and date variables

col_type <- list("date","num","num","char","num","cat","num",
                 "num","date","date","num","char","char","char",
                 "char")

names(col_type) <- colnames(xactions)

xactions <- convert_var(dataframe = xactions, 
                        col_names = names(col_type), 
                        col_types = col_type)

# Remove NAs in Dataframe

result <- remove_NA(xactions)
xactions <- result[[1]]
NA1_cell <- result[[2]]

# Maintain Track of col_name and removed rows

row_tracker <- cbind.data.frame(names(col_type),emp_cell,NA_cell,NA1_cell)
colnames(row_tracker) <- c("Name","Empty cells","NA Cells","NA1 Cells")


# Remove users with invalid age details

age_max = 90
age_min = 10

m = nrow(xactions)
xactions <- age_cutoff(xactions,mi = age_min, ma = age_max)
n = nrow(xactions)

age_drop <- rep(0,length(col_type))
age_drop[which(colnames(xactions)=="user_age")] = m-n

row_tracker <- cbind.data.frame(row_tracker,age_drop)
colnames(row_tracker) <- c("Name","Empty cells","NA Cells",
                           "NA1 Cells","Age_drop")
####################################################################

# Distinct Items greater than 2

m = nrow(xactions)

xactions <- sqldf(
  "select *
   from xactions
   where num_distinct_items >= 2")

n = nrow(xactions)

lack_of_items <- rep(0,length(col_type))
lack_of_items[which(colnames(xactions)=="num_distinct_items")] = m-n

row_tracker <- cbind.data.frame(row_tracker,lack_of_items)
colnames(row_tracker) <- c("Name","Empty cells","NA Cells",
                           "NA1 Cells","Age_drop","lack_of_items")

#################################################################

write.csv(xactions,"Reco_dataset_clean.csv", row.names = FALSE)

write.csv(row_tracker,"Removed_rows_details.csv", row.names = FALSE)

