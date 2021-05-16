suppressPackageStartupMessages(
  {
    library(argparser, quietly=TRUE)
    library(cluster)
    library(data.table)
    library(dplyr)
    library(factoextra)
    library(janitor)
    library(knitr)
    library(NbClust)
    library(recommenderlab)
    library(reshape2)
    library(rpart)
    library(rpart.plot)
    library(sqldf)
    library(stringr)
    library(tools)
    library(UBL)
    library(caret)
    
  }
)

setwd("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800")
getwd()

name <- paste("Reco_dataset_clean",".csv",sep = "")
xactions <- read.csv(name)

source(file.path("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800", "FA800_Helper.r"))

user_data <- getUserdata(xactions)

user_data$user_gender <- as.factor(user_data$user_gender)

######################################################################

# Sampling Variables

age_bin_br <- 20
vis_lim <- 0.1
samp_lim <- round(nrow(user_data)*0.001,0)
und_samp_typ <- TRUE
samp_method <- "levels"

# Convert to Bins

age_breaks <- breaks_create(user_data[,"user_age"], bin_br = age_bin_br)
vis_breaks <- vis_break_create(user_data[,"num_visits"],lim = vis_lim)

user_data <- convert_bin(user_data,"user_age",age_breaks)
user_data <- convert_bin(user_data,"num_visits",vis_breaks)

summary(user_data$num_visits_bin)
summary(user_data$user_gender)
summary(user_data$user_age_bin)

user_data[,"Bal_Var"] <- with(user_data, interaction(user_gender, 
                                                     user_age_bin,
                                                     num_visits_bin))

summary(user_data$Bal_Var)
plot(user_data$Bal_Var)


if(samp_method == "None"){
  
  xactions_train_resamp <- sqldf(
    "select a.*,
    b.user_age_bin,
    b.num_visits_bin,
    b.Bal_Var
   from xactions a, user_data b
   where a.user_id = b.user_id")
  
}else{
  result <- sampling_data(train =  user_data, samp_lim, Under = und_samp_typ, 
                                         samp_method = samp_method)
  xactions_train_resamp <- result[[1]]
  user_resample <- result[[2]]
  
}

setwd("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800\\Datasets")

visits_file <- paste("Recos_Visits_Resample","_",samp_method,"_",
                     und_samp_typ,".csv",sep = "")

users_file <- paste("Recos_Users_Resample","_",samp_method,"_",
                     und_samp_typ,".csv",sep = "")

write.csv(xactions_train_resamp,visits_file, row.names = FALSE)
write.csv(user_resample,users_file, row.names = FALSE)
