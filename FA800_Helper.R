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
    und_perc <- ifelse(age_perc<1,age_perc,1)
    bal_samp <- RandUnderClassif(age_bin ~ ., bal_samp, C.perc=und_perc)
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
    "select name,
  count(name) count
   from user_resample
   group by name")
  
  
  Resampled_train_actions <- NULL
  
  for(i in 1:nrow(user_df)){
    user_name <- user_df[i,"name"]
    count <- user_df[i,"count"]
    print(user_name)
    print(count)
    name_ind <- which(xactions[,"user_name"] == user_name)
    mini_acts <- NULL
    for(j in 1:count){
      tmp <- xactions[name_ind,]
      new_id <- paste(user_name,j,sep = "_")
      tmp[,"user_id"] <- rep(new_id,nrow(tmp))
      mini_acts <- rbind.data.frame(mini_acts,tmp)
    }
    Resampled_train_actions <- rbind.data.frame(Resampled_train_actions,
                                                mini_acts)
  }
  
  return(Resampled_train_actions)
}

level_sampling <- function(data, samp_lim, Under){
  train1 <- RandOverClassif(user_gender ~ ., train, C.perc="balance")
  
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
  
  comb_perc <- imb_perc_comp(train, name = "Bal_Var", 
                             samp_lim = samp_lim)
  
  train4 <- reg_rand_samp(train,"Bal_Var",perc = comb_perc, 
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
    
    user_resample <- samp_res[3]
    
    # Reconversion back to actions table
    
    xactions_train_resamp <- resampling_comb(xactions, user_resample)
    
  }else if(samp_method == "direct"){
    
    user_resample <- direct_sampling(data = train, samp_lim = samp_lim, 
                                     Under = und_samp_typ)
    # Reconversion back to actions table
    
    xactions_train_resamp <- resampling_comb(xactions, user_resample)
  }
  return(xactions_train_resamp)
}


