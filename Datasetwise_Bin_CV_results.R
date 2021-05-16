suppressPackageStartupMessages(
  {
    library(sqldf)
    library(recommenderlab)
    library(ggplot2)
  }
)
#####################################################################

setwd("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800\\Datasets")

source(file.path("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800", "FA800_Helper.r"))

predType = "topNList"

test_sp <- 0.85
nPred = 2
gn = -1
k_val = 3

und_samp_typ <- TRUE
samp_methods <- c("levels","direct")

dataset_names <- c("Recos_Visits_Resample_None_True.csv")

for(samp_method in samp_methods){
  tmp <- paste("Recos_Visits_Resample","_",samp_method,"_",
               und_samp_typ,".csv",sep = "")
  dataset_names <- c(dataset_names,tmp)
}

ratings_type <- c("Computed","Binary")
rec_methods <- c("UBCF","IBCF")
baseline_methods <- c("Popular")
all_methods <- c(rec_methods,baseline_methods)

sampling_names <- c("No Sampling","Levels Sampling","Direct Sampling")

bins_list <- c("num_visits_bin","user_gender","user_age_bin")

dataset_bins_results <- NULL

for(name in dataset_names){
  
  i = which(dataset_names == name)
  dataset <- sampling_names[i]
  xactions <- read.csv(name)
  
  for (colname in bins_list){
    print(colname)
    print(name)
    tmp_results <- dataset_cv_computer(xactions,all_methods,ratings_type,
                                       rec_methods,colname,
                                       dataset,test_sp,k_val,gn)
    
    dataset_bins_results <- rbind.data.frame(dataset_bins_results,
                                             tmp_results)
  }

}

colnames(dataset_bins_results) <- c("Dataset","User Group","Ratings Type",
                                    "Method","User Bins","Accuracy")

file_name <- paste("Sampling_Binwise_results.csv", sep = "")
write.csv(dataset_bins_results,file_name, row.names = FALSE)
