###########################################################
library(sqldf)
library(recommenderlab)

setwd("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800\\Datasets")

source(file.path("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800", "FA800_Helper.r"))


predType = "topNList"
test_sp <- 0.85
nPred = 2
gn = -1
k_val = 5

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
baseline_methods <- c("Popular","Random")
all_methods <- c(rec_methods,baseline_methods)

sampling_names <- c("No Sampling","Levels Sampling","Direct Sampling")

results_col <- c("Dataset","Ratings_Type","Method","Accuracy")

results <- NULL

for(dataset in dataset_names){
  
  i = which(dataset_names == dataset)
  name <- sampling_names[i]
  xactions <- read.csv(dataset)
  
  # Creating  Ratings Matrix and Eval Schemes
  
  ratingsDF <- getUserRatingsData(xactions, itemType = "%")
  ratings_mat <- getNormalizedRatingsMatrix(ratingsDF, normType = NULL)
  binary_mat <- binarize(ratings_mat, minRating=0.0001)
  
  e <- evaluationScheme(ratings_mat, method="split", train=test_sp,
                        k=1, given = gn, goodRating = 0)
  
  f <- evaluationScheme(binary_mat, method="split", train=test_sp,
                        k=1, given = gn)
  
  for (type in ratings_type){
    if (type == "Computed"){
      for(recMethod in all_methods){
        r <- Recommender(getData(e, "train"), recMethod)
        p <- predict(r, getData(e, "known"), type=predType, n = nPred)
        over_acc <- calcPredictionAccuracy(p, getData(e, "unknown"), 
                                           given=gn, goodRating=0)[1]
        comp_row <- c(name,type,recMethod,round(over_acc,4))
        results <- rbind.data.frame(results, comp_row)
        
      }
      
    }else{
      for(recMethod in rec_methods){
        r <- Recommender(getData(f, "train"), recMethod)
        p <- predict(r, getData(f, "known"), type=predType, n = nPred)
        over_acc <- calcPredictionAccuracy(p, getData(e, "unknown"), 
                                           given=gn, goodRating=0)[1]
        comp_row <- c(name,type,recMethod,round(over_acc,4))
        results <- rbind.data.frame(results, comp_row)
      }
    }
  }
  
  colnames(results) <- results_col 
}

file_name <- paste("Recos_results.csv", sep = "")
write.csv(results,file_name, row.names = FALSE)


#################################################################

cv_results <- NULL

for(dataset in dataset_names){
  
  i = which(dataset_names == dataset)
  name <- sampling_names[i]
  xactions <- read.csv(dataset)
  
  # Creating  Ratings Matrix and Eval Schemes
  
  ratingsDF <- getUserRatingsData(xactions, itemType = "%")
  ratings_mat <- getNormalizedRatingsMatrix(ratingsDF, normType = NULL)
  binary_mat <- binarize(ratings_mat, minRating=0.0001)
  
  k_cross <- evaluationScheme(ratings_mat, method="cross", train=test_sp,
                              k=k_val, given = gn, goodRating = 0)
  
  f_cross <- evaluationScheme(binary_mat, method="cross", train=test_sp,
                              k=k_val, given = gn, goodRating = 0)
  
  
  for (type in ratings_type){
    if (type == "Computed"){
      for(recMethod in all_methods){
        comp_rat <- cross_val_comp(r_matrix =  ratings_mat, test_sp,
                                   k_cross, k_val,gn,
                                   recMethod = recMethod, predType, 
                                   nPred)
        
        comp_row <- c(name,type,recMethod,round(comp_rat[1],4))
        cv_results <- rbind.data.frame(cv_results, comp_row)
        
      }
      
    }else{
      for(recMethod in rec_methods){
        comp_rat <- cross_val_comp(r_matrix =  binary_mat, test_sp,
                                   f_cross, k_val,gn,
                                   recMethod = recMethod, predType, 
                                   nPred)
        
        comp_row <- c(name,type,recMethod,round(comp_rat[1],4))
        cv_results <- rbind.data.frame(cv_results, comp_row)
      }
    }
  }
  
  colnames(cv_results) <- results_col 
}


file_name <- paste("Recos_cv_results.csv", sep = "")
write.csv(cv_results,file_name, row.names = FALSE)



