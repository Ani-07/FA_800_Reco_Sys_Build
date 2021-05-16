suppressPackageStartupMessages(
  {
    library(sqldf)
    library(recommenderlab)
    library(ggplot2)
  }
)
#####################################################################

predType = "topNList"

test_sp <- 0.85
nPred = 2
gn = -1
k_val = 5

###################################################################

setwd("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800\\Datasets")
getwd()

name <- "Recos_Visits_Resample_None_True"
file <- paste(name,".csv",sep = "")
xactions <- read.csv(file)

source(file.path("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800", "FA800_Helper.r"))

#########################################################################
#Creating Ratings Matrix

ratingsDF <- getUserRatingsData(xactions, itemType = "%")
ratings_mat_pr_nrml <- df2RatingsMatrix(ratingsDF)
binary_mat <- binarize(ratings_mat_pr_nrml, minRating=0.0001)
ratings_mat <- getNormalizedRatingsMatrix(ratingsDF, normType = NULL)

#######################################################################

k_cross <- evaluationScheme(ratings_mat, method="cross", train=test_sp,
                            k=k_val, given = gn, goodRating = 0)

########################################################################
k_cross <- evaluationScheme(ratings_mat, method="cross", train=test_sp,
                            k=k_val, given = gn, goodRating = 0)

comp_rat_Ubcf <- cross_val_comp(r_matrix =  ratings_mat, test_sp,k_cross, k_val,gn,
               recMethod = "UBCF", predType,nPred)

popular <- cross_val_comp(r_matrix =  ratings_mat, test_sp,k_cross, 
                          k_val,gn,recMethod = "Popular", predType, 
                          nPred)

random <- cross_val_comp(r_matrix =  ratings_mat, test_sp,k_cross, k_val,
                         gn,recMethod = "Random", predType,nPred)


f_cross <- evaluationScheme(binary_mat, method="cross", train=test_sp,
                            k=k_val, given = gn, goodRating = 0)

binary_rat_Ubcf <- cross_val_comp(r_matrix =  binary_mat, test_sp,
                                  f_cross, k_val,gn,recMethod = "UBCF", 
                                  predType,nPred)

#######################################################################

overall_results <- data.frame(comp_rat_Ubcf)

overall_results <- rbind.data.frame(overall_results, binary_rat_Ubcf, popular, random)

overall_results <- round(overall_results,4)

overall_results <- data.frame(t(overall_results))

colnames(overall_results) <- c("Computed_UBCF","Binary_UBCF","Popular", 
                               "Random")

file_name <- paste(name,"_overall", "_results.csv", sep = "")

write.csv(overall_results,file_name, row.names = FALSE)


##########################################################
bins <- levels(as.factor(xactions[,"user_age_bin"]))

####################################################################

cross_val_acc_bins_methods <- NULL

k_cross <- evaluationScheme(ratings_mat, method="cross", train=test_sp,
                            k=k_val, given = gn, goodRating = 0)

comp_rat_Ubcf_cv <- cross_val_bin_comp(ratings_mat,k_cross,
                                       recMethod = "UBCF",bins, 
                                       k_val = k_val)

tmp_row <- c(dataset, colname, ratings_type,recMethod, cross_val_acc_bins_tmp)

cross_val_acc_bins_methods <- rbind.data.frame(cross_val_acc_bins_methods,
                                               cross_val_acc_bins_tmp)

file_name <- paste(name,"_comp_ubcf_cv", "_results.csv", sep = "")
write.csv(comp_rat_Ubcf_cv,file_name, row.names = FALSE)

#################################################################

binary_rat_Ubcf_cv <- cross_val_bin_comp(binary_mat,f_cross,bins)
binary_rat_Ubcf_cv[,1] <- bins
binary_cv <- binary_rat_Ubcf_cv[,c(1,4)]
file_name <- paste(name,"_binary_ubcf_cv", "_results.csv", sep = "")
write.csv(binary_cv,file_name, row.names = FALSE)

#####################################################

popular_cv <- cross_val_bin_comp(ratings_mat,k_cross,bins,
                                       recMethod = "Popular")
popular_cv[,1] <- bins
popular_cv <- popular_cv[,c(1,4)]
file_name <- paste(name,"_popular_cv", "_results.csv", sep = "")
write.csv(popular_cv,file_name, row.names = FALSE)

##############################################################

random_cv <- cross_val_bin_comp(ratings_mat,k_cross,bins,
                                 recMethod = "Random")
random_cv[,1] <- bins
random_cv <- random_cv[,c(1,4)]
file_name <- paste(name,"_random_cv", "_results.csv", sep = "")
write.csv(random_cv,file_name, row.names = FALSE)

##############################################################

test <- rbind.data.frame(colnames(overall_results), overall_results[1,])

test <- as.data.frame(t(test))
colnames(test) <- c("Method","TP")

file_name <- paste(name,"_overall", "_results.csv", sep = "")

write.csv(test,file_name, row.names = FALSE)


barplot(test[,1],test[,2])

p<- ggplot(data=test, aes(x=1, y=TP)) +
   geom_bar(stat="identity")

