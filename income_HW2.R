## Reading the income dataset from file
income_train_df<-read.csv(file.choose())

size<-dim(income_train_df)
income_train_row<-size[1]
income_train_col<-size[2]


## Outlier eliminations
age_mean <- mean(income_train_df$age)
age_sd <- sd(income_train_df$age)

hpw_mean <- mean(income_train_df$hour_per_week)
hpw_sd <- sd(income_train_df$hour_per_week)

net_cap<-vector(mode ="integer",length = income_train_row)

for(j in 1:income_train_row)
  net_cap[j]=income_train_df[["capital_gain"]][j]-income_train_df[["capital_loss"]][j]

netCap_mean <- mean(net_cap)
netCap_sd <- sd(net_cap)

outlier_index_vec<-vector(mode ="integer",length = income_train_row)
count<-1

#Compute all rows that need to be removed
for(i in 1:income_train_row)
{
  age<-income_train_df[["age"]][i];
  hpw<-income_train_df[["hour_per_week"]][i]
  net<-net_cap[i]
  
  if(age > (age_mean+3*age_sd) | age < (age_mean-3*age_sd) |
     hpw > (hpw_mean+3*hpw_sd) | hpw < (hpw_mean-3*hpw_sd) |
     net > (netCap_mean+3*netCap_sd) | net < (netCap_mean-3*netCap_sd)) {
    
    outlier_index_vec[count] <- i;
    count<-count+1
  }
}

old_income_train_df <- income_train_df
income_train_df <- income_train_df[-outlier_index_vec,]

size<-dim(income_train_df)
income_train_row<-size[1]
income_train_col<-size[2]

print(income_train_row)
print(income_train_col)

##----------------------------------------------------------
#Function for getting max occurance of a column value

maxOccurance <- function(varcol)
{
  
  items <- unique(varcol)
  freq <- vector(mode="numeric",length=length(items))
  
  for(i in 1:length(items))
  {
    freq[i] <- length(subset(varcol,varcol==items[i]))
    
  }
  
  order_vec <- order(freq,decreasing=TRUE)
  
  max_index <- order_vec[1]
  
  result <- items[max_index]
  
  if(result==" ?")
  {
    max_index <- order_vec[2]
    result <- items[max_index]
  }
  
  return (result)
}

#result <- maxOccurance(newInc_train_df$race)



##------------------------------------------------------------
## Missing value calculations

find_missing_Train<-function(){
  
  for(i in 1:length(income_train_df$workclass))
  {
    if(income_train_df[["workclass"]][i]==" ?"){
      income_train_df[["workclass"]][i]=maxOccurance(income_train_df$workclass)
    }
  }
  
  for(i in 1:length(income_train_df$occupation))
  {
    if(income_train_df[["occupation"]][i]==" ?"){
      income_train_df[["occupation"]][i]=maxOccurance(income_train_df$occupation)
    }
  }
  
  
  for(i in 1:length(income_train_df$native_country))
  {
    if(income_train_df[["native_country"]][i]==" ?"){
      income_train_df[["native_country"]][i]=maxOccurance(income_train_df$native_country)
    }
  }
  
  return(income_train_df)
  
}

income_train_df<-find_missing_Train()

#-------------------------------------------------------------
#Creating a new income data frame to hold final data set
temp_df <- income_train_df
drops <- c("age","workclass","fnlwgt","education","education_cat","marital_status","occupation","relationship","race","gender","capital_gain","capital_loss","hour_per_week","native_country","class")
newInc_train_df <- temp_df[,!(names(temp_df) %in% drops)]

#--------------------------------------------------------------

#discretization of  age 

age_vec<-vector(mode ="integer",length = income_train_row)

discretize_age<-function(){
  
  age_vec <- age<-cut(income_train_df$age,seq(0,100,10),labels=c(1:10))
  return(age_vec)
}

age_vec <- discretize_age();

#Binding age to newInc
newInc_train_df <- cbind(newInc_train_df,age_vec)

#Binding workclass to newInc
newInc_train_df$workclass <- income_train_df$workclass


#normalize fnwgt using normalization
nfnlwgt<-vector(mode ="integer",length = income_train_row)

#Binding education_cat to newInc
newInc_train_df$education_cat <- income_train_df$education_cat

#Binding marital_status to newInc
newInc_train_df$marital_status <- income_train_df$marital_status

#Binding occupation to newInc
newInc_train_df$occupation <- income_train_df$occupation

#Binding relationship to newInc
newInc_train_df$relationship <- income_train_df$relationship

#Binding race to newInc
newInc_train_df$race <- income_train_df$race

#Binding gender to gender
newInc_train_df$gender <- income_train_df$gender

#normalize capital gain-loss using net

net_capital<-vector(mode ="integer",length = income_train_row)

find_zscore_net_capital<-function(){
  
  min_cap<-max(income_train_df[["capital_loss"]])
  max_cap<-max(income_train_df[["capital_gain"]])
  
  mean_cap<-mean(net_capital)
  sd_cap<-sd(net_capital)
  
  for(j in 1:income_train_row){
    net_capital[j]=income_train_df[["capital_gain"]][j]-income_train_df[["capital_loss"]][j]
    #    print(net_capital[j])
  }
  
  for(j in 1:income_train_row){
    net_capital[j]=(net_capital[j]-mean_cap)/sd_cap
  }
  
  return(net_capital)
}

net_capital<-find_zscore_net_capital()

#Binding net capital to newInc
newInc_train_df <- cbind(newInc_train_df,net_capital)


#normalize hour_per_week based on divided factor 10 

hour_per_week<-vector(mode ="integer",length = income_train_row)

zscore_normalize_hour_per_week<-function(){
  
  mean_hour_per_week<-mean(income_train_df$hour_per_week)
  sd_hour_per_week<-mean(income_train_df$hour_per_week)
  
  for(j in 1:income_train_row){
    hour_per_week[j]=(income_train_df[["age"]][j]-mean_hour_per_week)/sd_hour_per_week
  }
  return(hour_per_week)
}

hour_per_week<-zscore_normalize_hour_per_week()

#Binding hour_per_week to newInc
newInc_train_df <- cbind(newInc_train_df,hour_per_week)

#Binding native_country to newInc
newInc_train_df$native_country <- income_train_df$native_country

#----------------------------------------------------



convert_TrainDF_to_matrix<-function()
{
  
  size<-dim(newInc_train_df)
  income_train_row<-size[1]
  income_train_col<-size[2]
  print(income_train_row)
  print(income_train_col)
  
  data_mat_Train<-matrix(0L,nrow=income_train_row,ncol=income_train_col+1)
  
  colnames(data_mat_Train)<-c("transID","age","workclass","education_cat","marital_status","occupation","relationship","race","gender","net_capital","hour_per_week","native_country")
  
  y<-rownames(newInc_train_df)
  y<-as.numeric(y)
  data_mat_Train[,1]<-cbind(y)
  y<-as.integer(factor(newInc_train_df$age))
  data_mat_Train[,2]<-cbind(y)
  y<-as.integer(factor(newInc_train_df$workclass))
  data_mat_Train[,3]<-cbind(y)
  data_mat_Train[,4]<-cbind(newInc_train_df$education_cat)
  y<-as.integer(factor(newInc_train_df$marital_status))
  data_mat_Train[,5]<-cbind(y)
  y<-as.integer(factor(newInc_train_df$occupation))
  data_mat_Train[,6]<-cbind(y)
  y<-as.integer(factor(newInc_train_df$relationship))
  data_mat_Train[,7]<-cbind(y)
  y<-as.integer(factor(newInc_train_df$race))
  data_mat_Train[,8]<-cbind(y)
  y<-as.integer(factor(newInc_train_df$gender))
  data_mat_Train[,9]<-cbind(y)
  data_mat_Train[,10]<-cbind(newInc_train_df$net_capital)
  data_mat_Train[,11]<-cbind(newInc_train_df$hour_per_week)
  y<-as.integer(factor(newInc_train_df$native_country))
  data_mat_Train[,12]<-cbind(y)
  
  return(data_mat_Train)
}

data_mat_Train <- convert_TrainDF_to_matrix()



#Normalize income Test dataset based on Train dataset parameters

income_test_df<-read.csv(file.choose())

size<-dim(income_train_df)
income_test_row<-size[1]
income_test_col<-size[2]


# Find missing values for Test dataset wrt to Training set 
find_missing_Test<-function(){
  
  for(i in 1:length(income_test_df$workclass))
  {
    if(income_test_df[["workclass"]][i]==" ?"){
      income_train_df[["workclass"]][i]=maxOccurance(income_train_df$workclass)
    }
  }
  
  for(i in 1:length(income_test_df$occupation))
  {
    if(income_test_df[["occupation"]][i]==" ?"){
      income_train_df[["occupation"]][i]=maxOccurance(income_train_df$occupation)
    }
  }
  
  
  for(i in 1:length(income_test_df$native_country))
  {
    if(income_test_df[["native_country"]][i]==" ?"){
      income_train_df[["native_country"]][i]=maxOccurance(income_train_df$native_country)
    }
  }
  
  return(income_test_df)
  
}

income_test_df<-find_missing_Test()

#-------------------------------------------------------------
#Creating a new income data frame to hold final data set
temp_Test_df <- income_test_df
drops <- c("age","workclass","fnlwgt","education","education_cat","marital_status","occupation","relationship","race","gender","capital_gain","capital_loss","hour_per_week","native_country","class")
newInc_Test_df <- temp_Test_df[,!(names(temp_Test_df) %in% drops)]

#--------------------------------------------------------------

#discretization of  age 

age_vec_test<-vector(mode ="integer",length = income_test_row)

discretize_age_test<-function(){
  
  age_vec_test <- age<-cut(income_test_df$age,seq(0,100,10),labels=c(1:10))
  return(age_vec_test)
}

age_vec_test <- discretize_age_test();

#Binding age to newInc
newInc_Test_df <- cbind(newInc_Test_df,age_vec)

#Binding workclass to newInc
newInc_Test_df$workclass <- newInc_Test_df$workclass


#normalize fnwgt using normalization
nfnlwgt<-vector(mode ="integer",length = newInc_Test_df)

#Binding education_cat to newInc
newInc_Test_df$education_cat <- newInc_Test_df$education_cat

#Binding marital_status to newInc
newInc_Test_df$marital_status <- newInc_Test_df$marital_status

#Binding occupation to newInc
newInc_Test_df$occupation <- newInc_Test_df$occupation

#Binding relationship to newInc
newInc_Test_df$relationship <- newInc_Test_df$relationship

#Binding race to newInc
newInc_Test_df$race <- newInc_Test_df$race

#Binding gender to gender
newInc_Test_df$gender <- newInc_Test_df$gender

#normalize capital gain-loss using net

net_cap_test<-vector(mode ="integer",length = income_test_row)
#Reduce Captial gain/loss to Net capital attribute
for(j in 1:income_test_row)
  net_cap_test[j]=income_test_df[["capital_gain"]][j]-income_test_df[["capital_loss"]][j]

find_zscore_net_capital_Test<-function(){
  
  min_cap<-max(income_train_df[["capital_loss"]])
  max_cap<-max(income_train_df[["capital_gain"]])
  
  mean_cap<-mean(net_capital)
  sd_cap<-sd(net_capital)
  
  for(j in 1:income_test_row){
    net_cap_test[j]=income_test_df[["capital_gain"]][j]-income_test_df[["capital_loss"]][j]
    #    print(net_capital[j])
  }
  
  for(j in 1:income_test_row){
    net_cap_test[j]=(net_cap_test[j]-mean_cap)/sd_cap
  }
  
  return(net_cap_test)
}

net_cap_test<-find_zscore_net_capital_Test()

#Binding net capital to newInc
newInc_Test_df <- cbind(newInc_train_df,net_cap_test)

#normalize hour_per_week based on divided factor 10 

hour_per_week_Test<-vector(mode ="integer",length = income_test_row)

zscore_normalize_hour_per_week_Test<-function(){
  
  mean_hour_per_week<-mean(income_train_df$hour_per_week)
  sd_hour_per_week<-mean(income_train_df$hour_per_week)
  
  for(j in 1:income_train_row){
    hour_per_week_Test[j]=(income_test_df[["age"]][j]-mean_hour_per_week)/sd_hour_per_week
  }
  return(hour_per_week_Test)
}

hour_per_week_Test<-zscore_normalize_hour_per_week_Test()

#Binding hour_per_week to newInc
newInc_Test_df <- cbind(newInc_Test_df,hour_per_week_Test)

#Binding native_country to newInc
newInc_Test_df$native_country <- newInc_Test_df$native_country


##------------------------------------------------------------

#Distance Calculation

#Euclid distance for income data

eu_mat <- matrix(0L,nrow = income_train_row,ncol = 11,byrow = T)
colnames(eu_mat) <- c("Transaction ID","1st","1-dist","2nd","2-dist","3rd","3-dist","4","4-dist","5","5-dist")

age_levels <- nlevels(newInc_train_df$age)
education_cat_levels <- max(newInc_train_df$education_cat)

euclid_income<-function()
{
  #First data point with all others
  #for(i in 1:income_train_row)
  for(i in 1:income_train_row)
  {	
    
    least_k_dist<-matrix(c(1,100,2,100,3,100,4,100,5,100),nrow=5,ncol=2,byrow=T)
    # least_k_dist<-round(least_k_dist,digits=3)
    
    colnames(least_k_dist) <- c("index","euclid distance")
    for(j in 1:income_train_row)
    {
      if(i!=j)
      {
        
        dist_income<-vector(mode ="integer",length = 12)
        
        dist_income[1] <- (((data_mat_Train[i,2]-data_mat_Train[j,2])/age_levels)^2)
        
        if(data_mat_Train[i,3] != data_mat_Train[j,3])
          dist_income[2]=1
        
        dist_income[3] <- (((data_mat_Train[i,4] - data_mat_Train[j,4])/education_cat_levels)^2)
        
        if(data_mat_Train[i,5] != data_mat_Train[j,5])
          dist_income[4]=1
        
        if(data_mat_Train[i,6] != data_mat_Train[j,6])
          dist_income[5]=1
        
        if(data_mat_Train[i,7] != data_mat_Train[j,7])
          dist_income[6]=1
        
        if(data_mat_Train[i,8] != data_mat_Train[j,8])
          dist_income[7]=1
        
        if(data_mat_Train[i,9] != data_mat_Train[j,9])
          dist_income[8]=1
        
        dist_income[9] <- ((data_mat_Train[i,10] - data_mat_Train[j,10])^2) 
        
        dist_income[10] <- ((data_mat_Train[i,11] - data_mat_Train[j,11])^2) 
        
        if(data_mat_Train[i,12] != data_mat_Train[j,12])
          dist_income[11]=1
        
        dist_income<-round(dist_income,digits=3)
        
        d <- sum(dist_income)^0.5
        
        d<-round(d,digits=3)
        
        if(least_k_dist[5,2]>d)
        {
          least_k_dist[5,2]=d
          least_k_dist[5,1]=data_mat_Train[j,1];
          least_k_dist<-least_k_dist[order(least_k_dist[,2]),]
        }
        
      }
    }
    
    ## Now to save it in a result matrix
    
    eu_mat[i,1]<- data_mat_Train[i,1]
    eu_mat[i,2]<- least_k_dist[1,1]
    eu_mat[i,3]<- least_k_dist[1,2]
    eu_mat[i,4]<- least_k_dist[2,1]
    eu_mat[i,5]<- least_k_dist[2,2]
    eu_mat[i,6]<- least_k_dist[3,1]
    eu_mat[i,7]<- least_k_dist[3,2]
    eu_mat[i,8]<- least_k_dist[4,1]
    eu_mat[i,9]<- least_k_dist[4,2]
    eu_mat[i,10]<- least_k_dist[5,1]
    eu_mat[i,11]<- least_k_dist[5,2]
  }
  return(eu_mat)
}

eu_mat<-euclid_income()

# ##--------------------------------------------------------------------------
# #Manhattan distance for income data

# man_mat <- matrix(0L,nrow = income_train_row,ncol = 11,byrow = T)
# colnames(man_mat) <- c("Transaction ID","1st","1-dist","2nd","2-dist","3rd","3-dist","4","4-dist","5","5-dist")

# #transID <- rownames(newInc_train_df)

# manhattan_income<-function()
# {
# #First data point with all others
# for(i in 1:income_train_row)
# {	

# least_k_dist<-matrix(c(1,100,2,100,3,100,4,100,5,100),nrow=5,ncol=2,byrow=T)
# # least_k_dist<-round(least_k_dist,digits=3)

# colnames(least_k_dist) <- c("index","manhattan distance")
# for(j in 1:income_train_row)
# {
# if(i!=j)
# {

# dist_income<-vector(mode ="integer",length = 12)

# dist_income[1] <- abs(data_mat_Train[i,2]-data_mat_Train[j,2])/age_levels

# if(data_mat_Train[i,3] != data_mat_Train[j,3])
# dist_income[2]=1

# dist_income[3] <- abs(data_mat_Train[i,4] - data_mat_Train[j,4])/education_cat_levels


# if(data_mat_Train[i,5] != data_mat_Train[j,5])
# dist_income[4]=1

# if(data_mat_Train[i,6] != data_mat_Train[j,6])
# dist_income[5]=1

# if(data_mat_Train[i,7] != data_mat_Train[j,7])
# dist_income[6]=1

# if(data_mat_Train[i,8] != data_mat_Train[j,8])
# dist_income[7]=1

# if(data_mat_Train[i,9] != data_mat_Train[j,9])
# dist_income[8]=1

# dist_income[9] <- abs(data_mat_Train[i,10] - data_mat_Train[j,10])

# dist_income[10] <- abs(data_mat_Train[i,11] - data_mat_Train[j,11])

# if(data_mat_Train[i,12] != data_mat_Train[j,12])
# dist_income[11]=1

# dist_income<-round(dist_income,digits=3)

# d <- sum(dist_income)

# #d<-round(d,digits=3)

# if(least_k_dist[5,2]>d)
# {
# least_k_dist[5,2]=d
# least_k_dist[5,1]=data_mat_Train[j,1];
# least_k_dist<-least_k_dist[order(least_k_dist[,2]),]
# }
# }
# }

# ## Now to save it in a result matrix
# man_mat[i,1]<- data_mat_Train[i,1]
# man_mat[i,2]<- least_k_dist[1,1]
# man_mat[i,3]<- least_k_dist[1,2]
# man_mat[i,4]<- least_k_dist[2,1]
# man_mat[i,5]<- least_k_dist[2,2]
# man_mat[i,6]<- least_k_dist[3,1]
# man_mat[i,7]<- least_k_dist[3,2]
# man_mat[i,8]<- least_k_dist[4,1]
# man_mat[i,9]<- least_k_dist[4,2]
# man_mat[i,10]<- least_k_dist[5,1]
# man_mat[i,11]<- least_k_dist[5,2]
# }
# return(man_mat)
# }

# man_mat<-manhattan_income()


#Load xls lib for exporting distances to csv files 
library(xlsx)
# write.csv(eu_mat,file="euclid_income.csv")
# write.csv(man_mat,file="manhattan_income.csv")

write.csv(eu_mat,file="euclid_income_HW2.csv")