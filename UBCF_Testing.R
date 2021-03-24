suppressPackageStartupMessages(
  {
    library(sqldf)
    library(recommenderlab)
  }
)
#####################################################################

predType = "topNList"

test_sp <- 0.85
nPred = 2
gn = -1
k_val = 3

###################################################################

setwd("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800")
getwd()

name <- "Reco_dataset_clean"
file <- paste(name,".csv",sep = "")
xactions <- read.csv(file)

source(file.path("C:\\Users\\v-anirudhs\\Documents\\Project\\FA800", "FA800_Helper.r"))

# UBCF Testing

# Overall Accuracy

ratingsDF <- getUserRatingsData(xactions, itemType = "%")
ratings_mat <- getNormalizedRatingsMatrix(ratingsDF, normType = NULL)
binary_mat <- binarize(ratings_mat, minRating=0.0001)

########################################################################

recMethod = "UBCF"

e <- evaluationScheme(ratings_mat, method="split", train=test_sp,
                      k=1, given = gn, goodRating = 0)

r <- Recommender(getData(e, "train"), recMethod)
p <- predict(r, getData(e, "known"), type=predType, n = nPred)

product_recos <- product_counter(p,ratings_mat)
print(product_recos[1:6,])

over_acc <- calcPredictionAccuracy(p, getData(e, "unknown"), 
                                   given=gn, goodRating=0)

print(round(over_acc,4))
#############################################################

f <- evaluationScheme(binary_mat, method="split", train=test_sp,
                      k=1, given = gn)

r_binary <- Recommender(getData(f, "train"), recMethod)
p_binary <- predict(r_binary, getData(f, "known"), type=predType, n = nPred)

product_recos_binary <- product_counter(p_binary,ratings_mat)
print(product_recos_binary[1:6,])

over_acc <- calcPredictionAccuracy(p_binary, getData(f, "unknown"), 
                                   given=gn)

print(round(over_acc,4))

#####################################################################

recMethod = "Random"

r <- Recommender(getData(e, "train"), recMethod)
p <- predict(r, getData(e, "known"), type=predType, n = nPred)

product_recos <- product_counter(p,ratings_mat)
print(product_recos[1:6,])

over_acc <- calcPredictionAccuracy(p, getData(e, "unknown"), 
                                   given=gn, goodRating=0)

print(round(over_acc,4))

##########################################################

