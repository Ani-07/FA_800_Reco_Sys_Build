#Helper

######################################################################
gender_imp <- function(xactions){
  
  # Read the table
  ref_name <- paste("census-female-names",".csv",sep = "")
  gender_ref <- read.csv(ref_name, header = FALSE)
  fem_names <- gender_ref[,1]
  
  gen_NA <- which(is.na(xactions[,"user_gender"]))
  gen_bl <- which(xactions[,"user_gender"]=="Not Applicable")
  gen_ind <- c(gen_NA,gen_bl)
  
  #Check if gender is NA
  for(i in gen_ind){
    
    #Identify First Name
    nm <- xactions[i,"user_name"]
    first <- strsplit(nm,"[.]")[[1]][1]
    
    #Check if First Name in List
    if (first %in% fem_names){
      xactions[i,"user_gender"] = "Female"
    }else{
      xactions[i,"user_gender"] = "Male"
    }}
  return(xactions)
}  

getUserCountData <- function(xacts, itemType, itemProperty = "item_name") {
  
  q <- "select user_name, 
          %s, 
          SUM(1.0) rating 
        from xacts
        where item_type_code like '%s'
        and sale_price > 0
        group by user_name, %s
        order by user_name, %s"
  q <- sprintf(q, itemProperty, itemType, itemProperty, itemProperty)
  rData <- sqldf(q)
  return(rData)
}

getUserdata <- function(xactions){
  
  user_data <- sqldf("select user_id,
            user_age,
            user_gender,
            --user_zipcode,
            MAX(num_user_visits) num_visits,
            MAX(days_from_visit) total_days,
            
            cast(MAX(days_from_visit) as float)/cast(MAX(num_user_visits) as float) avg_visit_days
            from xactions
            --where sale_price > 0
            group by user_id
            order by user_id asc")
  
  return(user_data)
}

getUserProd <- function(xactions){
  
  # Create user product count data
  count <- getUserCountData(xactions,itemType = "%")
  users <- sort(unique(count$user_name))
  products <- sort(unique(count$item_name))
  
  # Create User Product Matrix
  user_prod_matrix <- matrix(0L, nrow = length(users), ncol = length(products))
  rownames(user_prod_matrix) <- users
  colnames(user_prod_matrix) <- products
  user_prod_matrix <- as.data.frame(user_prod_matrix)
  
  # Fill User Product Matrix based on count
  for ( i in 1:nrow(count)){
    user_prod_matrix[count[i,1],count[i,2]] <- count[i,3]
  }
  
  user_data <- getUserdata(xactions)
  user_data <- cbind.data.frame(user_data[,-1],user_prod_matrix)
  
  return(user_data)
}

getUserdata2 <- function(xactions){
  
  user_data <- sqldf("select user_id,
            user_age,
            user_gender,
            --user_zipcode,
            MAX(num_user_visits) num_visits,
            MAX(days_from_visit) total_days,
            Bal_Var,
            user_age_bin,
            num_visits_bin,
            cast(MAX(days_from_visit) as float)/cast(MAX(num_user_visits) as float) avg_visit_days
            from xactions
            --where sale_price > 0
            group by user_id
            order by user_id asc")
  
  return(user_data)
}

###############################################################

find_col_ind <- function(col_names, xactions){
  inds <- NULL
  for(i in col_names){
    ind <- which(colnames(xactions)  == i)
    inds <- c(inds, ind)
  }
  return(inds)
}

convert_var <- function(dataframe, col_names, col_types){
  
  for(i in 1:length(col_names)){
    name = col_names[i]
    type = col_types[i]
    
    if (type == "cat"){
      dataframe[name] <- as.factor(dataframe[,name])
    }else if(type == "date"){
      dataframe[name] <- as.Date(dataframe[,name])
    }else if(type == "num"){
      dataframe[name] <- as.numeric(dataframe[,name])
    }else {
      dataframe[name] <- as.character(dataframe[,name])
    }
  }
  return(dataframe)
} 

age_cutoff <- function(df, mi, ma){
  
  if (anyNA(df[,"user_age"])){
    tmp_df <- df[-which(is.na(df[,"user_age"])),]
  }else{
    tmp_df <- df
  }
  
  if (max(tmp_df$user_age) > ma){
    df <- df[-which(df[,"user_age"] > ma),]
  }
  
  if (min(tmp_df$user_age) < mi){
    df <- df[-which(df[,"user_age"] < mi),]
  }
  return(df)
}

remove_NA1 <- function(user_data, name_col){
  
  tmp_df <- user_data[-which(is.na(user_data[,"user_age"])),]
  
  if (max(tmp_df$user_age) > ma){
    user_data <- user_data[-which(user_data[,"user_age"] > ma),]
  }
  
  if (min(tmp_df$user_age) < mi){
    user_data <- user_data[-which(user_data[,"user_age"] < mi),]
  }
  return(user_data)
}

remove_blank <- function(xactions){
  col_nam <- colnames(xactions)
  
  emp_cell <- NULL
  
  for (i in 1:length(col_nam)) {
    m = nrow(xactions)
    if (length(which(xactions[,i] == ""))>0){
      xactions <- xactions[-which(xactions[,i] == ""),]
    }
    n = nrow(xactions)
    emp_cell <- c(emp_cell, m-n)
  }
  res <- list(xactions, emp_cell)
  return(res)
}

remove_NA <- function(xactions){
  col_nam <- colnames(xactions)
  
  NA_cell <- NULL
  
  for (i in 1:length(col_nam)) {
    m = nrow(xactions)
    if(length(which(is.na(xactions[,i])))>0){
      xactions <- xactions[-which(is.na(xactions[,i])),]
    }
    n = nrow(xactions)
    NA_cell <- c(NA_cell, m-n)
  }
  res <- list(xactions, NA_cell)
  return(res)
}

samp_perc_lim <- function(perc, lim){
  perc <- ifelse(perc<lim,perc,lim)
  return(perc)
}

breaks_create <- function(num, bin_br){
  ran <- range(num)
  beg <- ifelse(ran[1]%%5>5,ran[1]-5,ran[1])
  end <- ifelse(ran[2]%%5<5,ran[2]+5,ran[2])
  breaks <- seq(round_half_up(beg,-1),round_half_up(end,-1),bin_br)
  return(breaks)
}


vis_break_create <- function(visits, lim = 0.1){
  ran <- range(visits)
  
  vis_breaks <- NULL
  
  gen_breaks <- c(ran[1],6,10,15,30,50,70,90,ran[2])
  
  for(i in gen_breaks){
    if(length(which(visits>i))>lim*length(visits)){
      vis_breaks <- c(vis_breaks,i)
    }else{
      vis_breaks <- c(vis_breaks,i,ran[2])
      break
    }
  }
  return(vis_breaks)
}


convert_bin <- function(test, col_name, breaks){
  
  new <- paste(col_name, "_bin", sep = "")
  test[new] <- cut(test[,col_name],breaks, include.lowest = TRUE)
  
  return(test)
}

reg_rand_samp <- function(test, col_name, perc, Under = TRUE){
  
  over_perc = perc[[1]]
  under_perc = perc[[2]]
  
  name <- paste(col_name,"~ .", sep = " ")
  name <- as.formula(name)
  bal_samp <- RandOverClassif(name, test, C.perc=over_perc)
  
  if (Under == TRUE){
    und_perc <- ifelse(under_perc<1,under_perc,1)
    bal_samp <- RandUnderClassif(name, bal_samp, C.perc=und_perc)
  }
  
  return(bal_samp)
}

imb_perc_comp <- function(test, name, samp_lim){
  bin_cat <- test[,name]
  
  avg <- length(bin_cat)/length(summary(bin_cat))
  age_perc <- as.list(avg/summary(bin_cat))
  names(age_perc) <- levels(bin_cat)
  
  over_perc <- ifelse(age_perc==Inf,1,age_perc)
  over_perc <- ifelse(over_perc>1,over_perc,1)
  over_perc <- samp_perc_lim(over_perc,samp_lim)
  
  under_perc <- ifelse(age_perc<1,age_perc,1)
  
  res <- list(over_perc,under_perc)
  
  return(res)
}

resampling_comb <- function(xactions, user_resample){
  
  user_df <- sqldf(
    "select user_id,
            user_age_bin,
            num_visits_bin,
            user_gender,
            Bal_Var,
            count(user_id) count
   from user_resample
   group by user_id")
  
  
  Resampled_train_actions <- NULL
  
  for(i in 1:nrow(user_df)){
    user_id <- user_df[i,"user_id"]
    count <- user_df[i,"count"]
    print(user_id)
    print(count)
    name_ind <- which(xactions[,"user_id"] == user_id)
    mini_acts <- NULL
    for(j in 1:count){
      tmp <- xactions[name_ind,]
      new_id <- paste(user_id,j,sep = "_")
      
      tmp[,"user_id_1"] <- rep(new_id,nrow(tmp))
      tmp[,"user_age_bin"] <- rep(user_df[i,"user_age_bin"],nrow(tmp))
      tmp[,"num_visits_bin"] <- rep(user_df[i,"num_visits_bin"],nrow(tmp))
      tmp[,"user_gender"] <- rep(user_df[i,"user_gender"],nrow(tmp))
      tmp[,"Bal_Var"] <- rep(user_df[i,"Bal_Var"],nrow(tmp))
      
      mini_acts <- rbind.data.frame(mini_acts,tmp)
    }
    Resampled_train_actions <- rbind.data.frame(Resampled_train_actions,
                                                mini_acts)
  }
  
  return(Resampled_train_actions)
}

level_sampling <- function(data, samp_lim, Under){
  
  train1 <- RandOverClassif(user_gender ~ ., data, C.perc="balance")
  
  age_perc <- imb_perc_comp(train1, name = "user_age_bin", 
                            samp_lim = samp_lim)
  
  train2 <- reg_rand_samp(train1,"user_age_bin",perc = age_perc, 
                          Under = und_samp_typ)
  
  vis_perc <- imb_perc_comp(train2, name = "num_visits_bin", 
                            samp_lim = samp_lim)
  
  train3 <- reg_rand_samp(train2,"num_visits_bin",perc = vis_perc, 
                          Under = und_samp_typ)
  
  res <- list(train1,train2, train3)
  return(res)
}

direct_sampling <- function(data, samp_lim, Under){
  
  comb_perc <- imb_perc_comp(data, name = "Bal_Var", 
                             samp_lim = samp_lim)
  
  train4 <- reg_rand_samp(data,"Bal_Var",perc = comb_perc, 
                          Under = und_samp_typ)
  
  return(train4)
}

splitXactions <- function(xactions, xVisits) {
  # ensure there are enough distinct items and visits for user (for training and evaluation)
  preXactions <- subset(xactions, xactions$visit <= (xactions$num_user_visits - xVisits))
  postXactions <- NULL
  if (xVisits == 0) {
    # special case where only recommendations are wanted
    postXactions <- lastXactions
  }
  else {
    postXactions <- subset(xactions, xactions$visit > (xactions$num_user_visits - xVisits))
  }
  
  actionsList <- list("pre" = preXactions, "post" = postXactions)
  return(actionsList)
}

data_test <- function(xactions_test_in,xactions_test_out){
  test1 <- getUserdata(xactions_test_in)
  test2 <- getUserdata(xactions_test_ou)
  
  res1 <- nrow(test1) == nrow(test2)
  
  res2 <- sum(test1$name == test2$name) == nrow(test1)
  
  res <- c(res1,res2)
  return(res)
}

sampling_data <- function(train,samp_lim, Under, samp_method){
  if(samp_method == "levels"){
    
    samp_res <- level_sampling(data = train, samp_lim = samp_lim, 
                               Under = und_samp_typ)
    
    user_resample <- samp_res[[3]]
    
    # Reconversion back to actions table
    
    xactions_train_resamp <- resampling_comb(xactions, user_resample)
    
  }else if(samp_method == "direct"){
    
    user_resample <- direct_sampling(data = train, samp_lim = samp_lim, 
                                     Under = und_samp_typ)
    # Reconversion back to actions table
    
    xactions_train_resamp <- resampling_comb(xactions, user_resample)
  }
  res <- list(xactions_train_resamp,user_resample)
  
  return(res)
}

getUserRatingsData <- function(xacts, itemType, itemProperty = "item_name") {
  xacts$days_from_visit <- xacts$days_from_visit + 1
  q <- "select user_id, 
          %s, 
          SUM(1.0/days_from_visit) rating 
        from xacts
        where item_type_code like '%s'
        --and sale_price > 0
        group by user_id, %s
        order by user_id, %s"
  q <- sprintf(q, itemProperty, itemType, itemProperty, itemProperty)
  rData <- sqldf(q)
  return(rData)
}

df2RatingsMatrix <- function(dfRatings) {
  #dfRatings <- rmNoise(dfRatings, dfRatings$rating)
  rateMat <- as(dfRatings, "realRatingMatrix")
  rateMat
}

getNormalizedRatingsMatrix <- function(dfRatings, normType = NULL) {
  r <- df2RatingsMatrix(dfRatings)
  return(normalize(r, method = normType))
}

getRecommender <- function(rateMat, recMethod) {
  r <- Recommender(rateMat, method = recMethod)
}

product_counter <- function(p,ratings_mat){
  p1 <- as(p, "list")
  
  products <- colnames(ratings_mat)
  count_recos <- rep(0, length(products))
  
  product_recos <- data.frame(products, count_recos)
  
  for(user in 1:length(p1)){
    for(product in p1[[user]]){
      #product <- p1[[1]][1]
      ind <- which(product_recos$products == product)
      product_recos$count_recos[ind] <- product_recos$count_recos[ind] + 1
    }
  }
  product_recos <- product_recos[order(product_recos$count_recos, decreasing = TRUE),]
  return(product_recos)
}

bin_search <- function(name,users,colname){
  ind <- which(users$user_id == name)
  return(users[, colname][ind])
}

cross_val_bin_comp <- function(preMatrix,k_cross,colname,bins,
                               recMethod = "UBCF", k_val = 5){
  
  users <- getUserdata2(xactions)
  
  k_binwise_acc <- NULL
  
  for(i in bins){
    bin_k_acc <- NULL
    
    for(j in 1:k_val){
      print(i)
      print(j)
      
      train_ind <- k_cross@runsTrain[[j]]
      test_ind <- which(!(c(1:nrow(preMatrix)) %in% train_ind))
      
      train_users <- rownames(preMatrix[train_ind,])
      train_bins <- sapply(train_users, bin_search, users,colname)
      
      # Bin wise accuracy
      
      test_users <- rownames(preMatrix[test_ind,])
      test_bins <- sapply(test_users, bin_search, users,colname)
      
      rows <- which(test_bins == i)
      test_count <- length(rows)
      
      train <- which(train_bins == i)
      train_count <- length(train)
      
      test_input <- k_cross@knownData[test_ind[rows],]
      test_output <- k_cross@unknownData[test_ind[rows],]
      
      train_input <- k_cross@knownData[train_ind,]
      
      r <- Recommender(train_input, recMethod)
      p1 <- predict(r, test_input, type=predType, n = nPred)
      
      
      bin_acc <- calcPredictionAccuracy(p1, test_output, 
                                        given=gn, goodRating=0)
      
      tmp_j <- c(train_count,test_count, bin_acc)
      
      bin_k_acc <- rbind(bin_k_acc, tmp_j)
      
    }
    tmp_i <- apply(bin_k_acc, mean, MARGIN = 2)
    
    tmp_row <- c(i,round(tmp_i,4))
    
    k_binwise_acc <- rbind.data.frame(k_binwise_acc,tmp_row)
    
    colnames(k_binwise_acc) <- c("Bin","Train","Test","TP","FP","FN","TN",
                                 "precision","recall","TPR","FPR")
    
  }
  k_binwise_acc <- as.data.frame(k_binwise_acc[,c(1,4)])
  return(k_binwise_acc)
}

cross_val_comp <- function(r_matrix,test_sp, k_cross,k_val,gn,recMethod, 
                           predType, nPred){
  
  results <- evaluate(k_cross, method=recMethod, type = predType,n=nPred)
  k_fold_acc <- avg(results)
  return(k_fold_acc)
}


dataset_cv_computer <- function(xactions,all_methods,ratings_type,
                                rec_methods,colname,dataset,test_sp,
                                k_val,gn){
  
  bins <- levels(as.factor(xactions[,colname]))
  
  b_len <- length(bins)
  
  result_cols <- c("Dataset","Ratings_Type","Method","Bins","Accuracy")
  
  result <- NULL
  
  ratingsDF <- getUserRatingsData(xactions, itemType = "%")
  ratings_mat_pr_nrml <- df2RatingsMatrix(ratingsDF)
  binary_mat <- binarize(ratings_mat_pr_nrml, minRating=0.0001)
  ratings_mat <- getNormalizedRatingsMatrix(ratingsDF, normType = NULL)
  
  
  k_cross <- evaluationScheme(ratings_mat, method="cross", train=test_sp,
                              k=k_val, given = gn, goodRating = 0)
  
  f_cross <- evaluationScheme(binary_mat, method="cross", train=test_sp,
                              k=k_val, given = gn, goodRating = 0)
  
  for (type in ratings_type){
    
    if (type == "Computed"){
      
      for(recMethod in all_methods){
        print(recMethod)
        print(type)
        extra_cols <- c(dataset, colname, type,recMethod)
        extra_cols <- matrix(rep(extra_cols,b_len),ncol = length(extra_cols), 
                             byrow = TRUE)
        
        tmp_acc <- cross_val_bin_comp(ratings_mat,k_cross,
                                      recMethod = recMethod,colname,bins, 
                                      k_val = k_val)
        
        tmp_row <- cbind.data.frame(extra_cols,tmp_acc)
        
        result <- rbind.data.frame(result,tmp_row)
      }
      
    }else{
      print("Correct")
      for(recMethod in rec_methods){
        print(recMethod)
        print(type)
        
        extra_cols <- c(dataset, colname, type,recMethod)
        extra_cols <- matrix(rep(extra_cols,b_len),ncol = length(extra_cols), 
                             byrow = TRUE)
        
        tmp_acc <- cross_val_bin_comp(binary_mat,f_cross,
                                      recMethod = recMethod,colname, bins, 
                                      k_val = k_val)
        
        tmp_row <- cbind.data.frame(extra_cols,tmp_acc)
        
        result <- rbind.data.frame(result,tmp_row)
        
      }
    }
   }
  return(result)
}


