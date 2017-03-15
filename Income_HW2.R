
## Reading the income dataset from file
cat("\n","Select the Income Training dataset","\n")
income_train_df<-read.csv(file.choose())
#income_train_df<-income_train_df[sample(nrow(income_train_df), 125,replace = FALSE), ]

cat("\n","Select the Income Test dataset","\n")      
income_test_df<-read.csv(file.choose())


size<-dim(income_train_df)
income_train_row<-size[1]
print(income_train_row)
incomeincome_train_row_train_col<-size[2]


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

#result <- maxOccurance(newInc_Train_df$race)



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
newInc_Train_df <- temp_df[,!(names(temp_df) %in% drops)]

#--------------------------------------------------------------

#discretization of  age 

age_vec<-vector(mode ="integer",length = income_train_row)

discretize_age<-function(){
  
  age_vec <- age<-cut(income_train_df$age,seq(0,100,10),labels=c(1:10))
  return(age_vec)
}

age_vec <- discretize_age();

#Binding age to newInc
newInc_Train_df <- cbind(newInc_Train_df,age_vec)

#Binding workclass to newInc
newInc_Train_df$workclass <- income_train_df$workclass


#normalize fnwgt using normalization
nfnlwgt<-vector(mode ="integer",length = income_train_row)

#Binding education_cat to newInc
newInc_Train_df$education_cat <- income_train_df$education_cat

#Binding marital_status to newInc
newInc_Train_df$marital_status <- income_train_df$marital_status

#Binding occupation to newInc
newInc_Train_df$occupation <- income_train_df$occupation

#Binding relationship to newInc
newInc_Train_df$relationship <- income_train_df$relationship

#Binding race to newInc
newInc_Train_df$race <- income_train_df$race

#Binding gender to gender
newInc_Train_df$gender <- income_train_df$gender

#normalize capital gain-loss using net

net_capital<-vector(mode ="integer",length = income_train_row)

find_zscore_net_capital<-function(){
  
  # min_cap<-max(income_train_df[["capital_loss"]])
  #  max_cap<-max(income_train_df[["capital_gain"]])
  
  for(j in 1:income_train_row){
    net_capital[j]=income_train_df[["capital_gain"]][j]-income_train_df[["capital_loss"]][j]
    #    print(net_capital[j])
  }
  
  mean_cap<-mean(net_capital)
  sd_cap<-sd(net_capital)
  
  for(j in 1:income_train_row){
    net_capital[j]=(net_capital[j]-mean_cap)/sd_cap
  }
  
  return(net_capital)
}

net_capital<-find_zscore_net_capital()

#Binding net capital to newInc
newInc_Train_df <- cbind(newInc_Train_df,net_capital)


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
newInc_Train_df <- cbind(newInc_Train_df,hour_per_week)

#Binding native_country to newInc
newInc_Train_df$native_country <- income_train_df$native_country

#Binding native_country to newInc
newInc_Train_df$class <- income_train_df$class

#----------------------------------------------------



convert_TrainDF_to_matrix<-function()
{
  
  size<-dim(newInc_Train_df)
  income_train_row<-size[1]
  income_train_col<-size[2]
  print(income_train_row)
  print(income_train_col)
  
  data_mat_Train<-matrix(0L,nrow=income_train_row,ncol=income_train_col+1)
  
  colnames(data_mat_Train)<-c("transID","age","workclass","education_cat","marital_status","occupation","relationship","race","gender","net_capital","hour_per_week","native_country","class")
  
  y<-rownames(newInc_Train_df)
  y<-as.numeric(y)
  data_mat_Train[,1]<-cbind(y)
  y<-as.integer(factor(newInc_Train_df$age))
  data_mat_Train[,2]<-cbind(y)
  y<-as.integer(factor(newInc_Train_df$workclass))
  data_mat_Train[,3]<-cbind(y)
  data_mat_Train[,4]<-cbind(newInc_Train_df$education_cat)
  y<-as.integer(factor(newInc_Train_df$marital_status))
  data_mat_Train[,5]<-cbind(y)
  y<-as.integer(factor(newInc_Train_df$occupation))
  data_mat_Train[,6]<-cbind(y)
  y<-as.integer(factor(newInc_Train_df$relationship))
  data_mat_Train[,7]<-cbind(y)
  y<-as.integer(factor(newInc_Train_df$race))
  data_mat_Train[,8]<-cbind(y)
  y<-as.integer(factor(newInc_Train_df$gender))
  data_mat_Train[,9]<-cbind(y)
  data_mat_Train[,10]<-cbind(newInc_Train_df$net_capital)
  data_mat_Train[,11]<-cbind(newInc_Train_df$hour_per_week)
  y<-as.integer(factor(newInc_Train_df$native_country))
  data_mat_Train[,12]<-cbind(y)
  y<-as.integer(factor(newInc_Train_df$class))
  data_mat_Train[,13]<-cbind(y)
  
  return(data_mat_Train)
}

data_mat_Train <- convert_TrainDF_to_matrix()



#Normalize income Test dataset based on Train dataset parameters



size<-dim(income_test_df)
income_test_row<-size[1]
income_test_col<-size[2]


# Find missing values for Test dataset wrt to Training set 
find_missing_Test<-function(){
  
  for(i in 1:length(income_test_df$workclass))
  {
    if(income_test_df[["workclass"]][i]==" ?"){
      income_test_df[["workclass"]][i]=maxOccurance(income_train_df$workclass)
    }
  }
  
  for(i in 1:length(income_test_df$occupation))
  {
    if(income_test_df[["occupation"]][i]==" ?"){
      income_test_df[["occupation"]][i]=maxOccurance(income_train_df$occupation)
    }
  }
  
  
  for(i in 1:length(income_test_df$native_country))
  {
    if(income_test_df[["native_country"]][i]==" ?"){
      income_test_df[["native_country"]][i]=maxOccurance(income_train_df$native_country)
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
# newInc_Test_df <- cbind(newInc_Test_df,age_vec)
newInc_Test_df$age_vec<-cbind(age_vec_test)

#Binding workclass to newInc
newInc_Test_df$workclass <- income_test_df$workclass

#Binding education_cat to newInc
newInc_Test_df$education_cat <- income_test_df$education_cat

#Binding marital_status to newInc
newInc_Test_df$marital_status <- income_test_df$marital_status

#Binding occupation to newInc
newInc_Test_df$occupation <- income_test_df$occupation

#Binding relationship to newInc
newInc_Test_df$relationship <- income_test_df$relationship

#Binding race to newInc
newInc_Test_df$race <- income_test_df$race

#Binding gender to gender
newInc_Test_df$gender <- income_test_df$gender

#normalize capital gain-loss using net

net_cap_test<-vector(mode ="integer",length = income_test_row)

find_zscore_net_capital_Test<-function(){
  
  #      min_cap<-max(income_train_df[["capital_loss"]])
  #      max_cap<-max(income_train_df[["capital_gain"]])
  
  for(j in 1:income_test_row){
    net_cap_test[j]=income_test_df[["capital_gain"]][j]-income_test_df[["capital_loss"]][j]
    #    print(net_capital[j])
  }
  
  for(j in 1:income_test_row){
    net_cap_test[j]=(net_cap_test[j]-netCap_mean)/netCap_sd
  }
  
  return(net_cap_test)
}

net_cap_test<-find_zscore_net_capital_Test()

#Binding net capital to newInc
#newInc_Test_df <- cbind(newInc_Train_df,net_cap_test)
newInc_Test_df$net_capital <- cbind(net_cap_test)

#normalize hour_per_week based on divided factor 10 

hour_per_week_Test<-vector(mode ="integer",length = income_test_row)


zscore_normalize_hour_per_week_Test<-function(){
  
  mean_hour_per_week<-mean(income_train_df$hour_per_week)
  sd_hour_per_week<-mean(income_train_df$hour_per_week)
  
  for(j in 1:income_test_row){
    hour_per_week_Test[j]=(income_test_df[["age"]][j]-hpw_mean)/hpw_sd
  }
  return(hour_per_week_Test)
}

hour_per_week_Test<-zscore_normalize_hour_per_week_Test()

#Binding hour_per_week to newInc
#newInc_Test_df <- cbind(newInc_Test_df,hour_per_week_Test)
newInc_Test_df$hour_per_week <- cbind(hour_per_week_Test)

#Binding native_country to newInc
newInc_Test_df$native_country <- income_test_df$native_country

#Binding class to newInc 
newInc_Test_df$class <- income_test_df$class


convert_TestDF_to_matrix<-function()
{
  
  size<-dim(newInc_Test_df)
  income_test_row<-size[1]
  income_test_col<-size[2]
  print(income_test_row)
  print(income_test_col)
  
  data_mat_Test<-matrix(0L,nrow=income_test_row,ncol=income_test_col+1)
  
  colnames(data_mat_Test)<-c("transID","age","workclass","education_cat","marital_status","occupation","relationship","race","gender","net_capital","hour_per_week","native_country","class")
  
  y<-rownames(newInc_Test_df)
  y<-as.numeric(y)
  data_mat_Test[,1]<-cbind(y)
  y<-as.integer(factor(newInc_Test_df$age_vec))
  data_mat_Test[,2]<-cbind(y)
  y<-as.integer(factor(newInc_Test_df$workclass))
  data_mat_Test[,3]<-cbind(y)
  data_mat_Test[,4]<-cbind(newInc_Test_df$education_cat)
  y<-as.integer(factor(newInc_Test_df$marital_status))
  data_mat_Test[,5]<-cbind(y)
  y<-as.integer(factor(newInc_Test_df$occupation))
  data_mat_Test[,6]<-cbind(y)
  y<-as.integer(factor(newInc_Test_df$relationship))
  data_mat_Test[,7]<-cbind(y)
  y<-as.integer(factor(newInc_Test_df$race))
  data_mat_Test[,8]<-cbind(y)
  y<-as.integer(factor(newInc_Test_df$gender))
  data_mat_Test[,9]<-cbind(y)
  data_mat_Test[,10]<-cbind(newInc_Test_df$net_capital)
  data_mat_Test[,11]<-cbind(newInc_Test_df$hour_per_week)
  y<-as.integer(factor(newInc_Test_df$native_country))
  data_mat_Test[,12]<-cbind(y)
  y<-as.integer(factor(newInc_Test_df$class))
  data_mat_Test[,13]<-cbind(y)
  
  return(data_mat_Test)
}

data_mat_Test <- convert_TestDF_to_matrix()

##------------------------------------------------------------
##Predictor

predict_class_type<-function(class_type){
  temp <- table(as.vector(class_type))
  z<-names(temp)[temp==max(temp)]
  ds<-as.integer(z)
  if(length(ds)>1)
    return (ds[1])
  else
    return (ds)
}

##------------------------------------------------------------

# Calculate Performance metrics for range of Knn values -    

knn<-5
max_knn<-35
perf_size<-(max_knn-knn)/2+1

man_error_rate<-vector(mode ="numeric",length = perf_size)
man_accuracy<-vector(mode ="numeric",length = perf_size)
man_FNR<-vector(mode ="numeric",length = perf_size)
man_TNR<-vector(mode ="numeric",length = perf_size)
man_TPR<-vector(mode ="numeric",length = perf_size)
man_FPR<-vector(mode ="numeric",length = perf_size)
man_Precision<-vector(mode ="numeric",length = perf_size)
man_Recall<-vector(mode ="numeric",length = perf_size)
man_F_measure<-vector(mode ="numeric",length = perf_size)

eu_error_rate<-vector(mode ="numeric",length = perf_size)
eu_accuracy<-vector(mode ="numeric",length = perf_size)
eu_FNR<-vector(mode ="numeric",length = perf_size)
eu_TNR<-vector(mode ="numeric",length = perf_size)
eu_TPR<-vector(mode ="numeric",length = perf_size)
eu_FPR<-vector(mode ="numeric",length = perf_size)
eu_Precision<-vector(mode ="numeric",length = perf_size)
eu_Recall<-vector(mode ="numeric",length = perf_size)
eu_F_measure<-vector(mode ="numeric",length = perf_size)

count<-1
while(knn<= max_knn)
{		
  #Distance Calculation
  
  #Euclid distance for income data
  
  eu_mat_Test <- matrix(0L,nrow = income_test_row,ncol = 4,byrow = T)
  colnames(eu_mat_Test) <- c("Transaction ID","Actual","Predicted","Posterior Probability")
  
  age_levels <- nlevels(newInc_Train_df$age)
  education_cat_levels <- max(newInc_Train_df$education_cat)
  
  euclid_income<-function()
  {
    #First data point with all others
    #for(i in 1:income_train_row)
    for(i in 1:income_test_row)
    {	
      class_type<-vector(mode ="integer",length = knn)
      least_k_dist<-matrix(nrow=knn,ncol=3,byrow=T)
      least_k_dist[,1]<-0
      least_k_dist[,2]<-100
      least_k_dist[,3]<-0
      
      
      #least_k_dist<-matrix(c(1,100,0,2,100,0,3,100,0,4,100,0,5,100,0),nrow=5,ncol=3,byrow=T)
      # least_k_dist<-round(least_k_dist,digits=3) 
      
      colnames(least_k_dist) <- c("index","euclid distance","class")
      for(j in 1:income_train_row)
      {
        
        dist_income<-vector(mode ="numeric",length = 12)
        
        dist_income[1] <- (((data_mat_Test[i,2]-data_mat_Train[j,2])/age_levels)^2)
        
        if(data_mat_Test[i,3] != data_mat_Train[j,3])
          dist_income[2]=1
        
        dist_income[3] <- (((data_mat_Test[i,4] - data_mat_Train[j,4])/education_cat_levels)^2)
        
        if(data_mat_Test[i,5] != data_mat_Train[j,5])
          dist_income[4]=1
        
        if(data_mat_Test[i,6] != data_mat_Train[j,6])
          dist_income[5]=1
        
        if(data_mat_Test[i,7] != data_mat_Train[j,7])
          dist_income[6]=1
        
        if(data_mat_Test[i,8] != data_mat_Train[j,8])
          dist_income[7]=1
        
        if(data_mat_Test[i,9] != data_mat_Train[j,9])
          dist_income[8]=1
        
        dist_income[9] <- ((data_mat_Test[i,10] - data_mat_Train[j,10])^2) 
        
        dist_income[10] <- ((data_mat_Test[i,11] - data_mat_Train[j,11])^2) 
        
        if(data_mat_Test[i,12] != data_mat_Train[j,12])
          dist_income[11]=1
        
        dist_income<-round(dist_income,digits=3)
        
        d <- sum(dist_income)^0.5
        
        d<-round(d,digits=3)
        
        if(least_k_dist[knn,2]>d)
        {
          least_k_dist[knn,2]=d
          least_k_dist[knn,1]=data_mat_Train[j,1];
          least_k_dist[knn,3]=data_mat_Train[j,13]
          least_k_dist<-least_k_dist[order(least_k_dist[,2]),]
        }
      }
      
      #print(least_k_dist)
      ##class_type[p]<- data_mat_Train[["class"]][sortedMat[p,1]]
      
      ## Now to save it in a result matrix
      
      eu_mat_Test[i,1]<- data_mat_Test[i,1]
      eu_mat_Test[i,2]<- data_mat_Test[i,13]
	  
	  c1sum <- 0
       c2sum <- 0
	   
      
      for(p in 1:knn)
      {
        class_type[p]<- least_k_dist[p,3] ## This contains the transaction id but not the index
		d <- least_k_dist[p,2];
              if(d==0)
                d<-0.0005
              if(least_k_dist[p,3]-1==0)
                c1sum <- c2sum + (1/(d^2))
              else
                c2sum <- c2sum + (1/(d^2))
      }
      pred_class<-predict_class_type(class_type)
	  
	      if(pred_class-1==0)
              eu_mat_Test[i,4] <- c1sum/(c1sum+c2sum)
          else
              eu_mat_Test[i,4] <- c2sum/(c1sum+c2sum)
            
            eu_mat_Test[i,3]<- pred_class
    }
    
    return(eu_mat_Test)
  }
  
  eu_mat_Test<-euclid_income()
  
  # ##--------------------------------------------------------------------------
  # #Manhattan distance for income data
  
  man_mat_Test <- matrix(0L,nrow = income_test_row,ncol = 4,byrow = T)
  colnames(man_mat_Test) <- c("Transaction ID","Actual","Predicted","Posterior Probability")
  
  age_levels <- nlevels(newInc_Train_df$age)			
  education_cat_levels <- max(newInc_Train_df$education_cat)
  
  manhattan_income<-function()
  {
    #First data point with all others
    for(i in 1:income_test_row)
    {	
      class_type<-vector(mode ="numeric",length = knn)	
      least_k_dist<-matrix(nrow=knn,ncol=3,byrow=T)			
      least_k_dist[,1]<-0			
      least_k_dist[,2]<-100			
      least_k_dist[,3]<-0			
      
      colnames(least_k_dist) <- c("index","manhattan distance","class")
      for(j in 1:income_train_row)
      {
        dist_income<-vector(mode ="integer",length = 12)
        
        dist_income[1] <- abs(data_mat_Test[i,2]-data_mat_Train[j,2])/age_levels
        
        if(data_mat_Test[i,3] != data_mat_Train[j,3])
          dist_income[2]=1
        
        dist_income[3] <- abs(data_mat_Test[i,4] - data_mat_Train[j,4])/education_cat_levels
        
        
        if(data_mat_Test[i,5] != data_mat_Train[j,5])
          dist_income[4]=1
        
        if(data_mat_Test[i,6] != data_mat_Train[j,6])
          dist_income[5]=1
        
        if(data_mat_Test[i,7] != data_mat_Train[j,7])
          dist_income[6]=1
        
        if(data_mat_Test[i,8] != data_mat_Train[j,8])
          dist_income[7]=1
        
        if(data_mat_Test[i,9] != data_mat_Train[j,9])
          dist_income[8]=1
        
        dist_income[9] <- abs(data_mat_Test[i,10] - data_mat_Train[j,10])
        
        dist_income[10] <- abs(data_mat_Test[i,11] - data_mat_Train[j,11])
        
        if(data_mat_Test[i,12] != data_mat_Train[j,12])
          dist_income[11]=1
        
        dist_income<-round(dist_income,digits=3)
        
        d <- sum(dist_income)
        
        d<-round(d,digits=3)
        
        if(least_k_dist[knn,2]>d)
        {
          least_k_dist[knn,2]=d
          least_k_dist[knn,1]=data_mat_Train[j,1];
          least_k_dist[knn,3]=data_mat_Train[j,13]
          least_k_dist<-least_k_dist[order(least_k_dist[,2]),]
        }
      }
      #print(least_k_dist)
      ## Now to save it in a result matrix
      man_mat_Test[i,1]<- data_mat_Test[i,1]
      man_mat_Test[i,2]<- data_mat_Test[i,13]
	  
	  c1sum <- 0
      c2sum <- 0
	  
      for(p in 1:knn)			
      {			
        class_type[p]<- least_k_dist[p,3] ## This contains the transaction id but not the index		
		d <- least_k_dist[p,2];
             if(d==0)
                d<-0.0005
             if(least_k_dist[p,3]-1==0)
                c1sum <- c2sum + (1/(d^2))
             else
                c2sum <- c2sum + (1/(d^2))
      }			
      pred_class<-predict_class_type(class_type)

			if(pred_class-1==0)
              man_mat_Test[i,4] <- c1sum/(c1sum+c2sum)
            else
              man_mat_Test[i,4] <- c2sum/(c1sum+c2sum)	  
			  
      man_mat_Test[i,3]<- pred_class
      
      #print(man_mat_Test)
    }
    return(man_mat_Test)
  }
  
  man_mat_Test<-manhattan_income()
  
  
  
  #Calculation of the confusion matrix
  actual_eu <- eu_mat_Test[,2]-1
  predicted_eu <- eu_mat_Test[,3]-1
  
  eu_confusion_matrix <- table(actual_eu,predicted_eu)
  
  eu_accuracy[count]<-sum(diag(eu_confusion_matrix))/sum(eu_confusion_matrix)
  eu_error_rate[count]<-1-eu_accuracy[count]
  eu_TPR[count]<-eu_confusion_matrix[1,1]/sum(eu_confusion_matrix[1,])
  eu_TNR[count]<-eu_confusion_matrix[2,2]/sum(eu_confusion_matrix[2,])
  eu_FPR[count]<-eu_confusion_matrix[2,1]/sum(eu_confusion_matrix[2,])
  eu_FNR[count]<-eu_confusion_matrix[1,2]/sum(eu_confusion_matrix[1,])
  eu_Precision[count]<-eu_confusion_matrix[1,1]/sum(eu_confusion_matrix[,1])
  eu_Recall[count]<-eu_confusion_matrix[1,1]/sum(eu_confusion_matrix[1,])
  eu_F_measure[count]<-(eu_confusion_matrix[1,1]*2)/(sum(eu_confusion_matrix[1,])+sum(eu_confusion_matrix[,1]))
  
  actual_manhat <- man_mat_Test[,2]-1
  predicted_manhat <- man_mat_Test[,3]-1
  
  man_confusion_matrix <- table(actual_manhat,predicted_manhat)
  
  man_accuracy[count]<-sum(diag(man_confusion_matrix))/sum(man_confusion_matrix)
  man_error_rate[count]<-1-man_accuracy[count]
  
  man_TPR[count]<-man_confusion_matrix[1,1]/sum(man_confusion_matrix[1,])
  man_TNR[count]<-man_confusion_matrix[2,2]/sum(man_confusion_matrix[2,])
  man_FPR[count]<-man_confusion_matrix[2,1]/sum(man_confusion_matrix[2,])
  man_FNR[count]<-man_confusion_matrix[1,2]/sum(man_confusion_matrix[1,])
  man_Precision[count]<-man_confusion_matrix[1,1]/sum(man_confusion_matrix[,1])
  man_Recall[count]<-man_confusion_matrix[1,1]/sum(man_confusion_matrix[1,])
  man_F_measure[count]<-(man_confusion_matrix[1,1]*2)/(sum(man_confusion_matrix[1,])+sum(man_confusion_matrix[,1]))
  
  count<-count+1
  
  knn<-knn+2
}
#Load xls lib for exporting distances to csv files 

k<-seq(5,max_knn,by=2)

euclidean_performance <-data.frame(k,eu_TPR,eu_FPR,eu_Precision,eu_accuracy,eu_error_rate,eu_Recall)
plot(eu_FPR,eu_TPR,xlim=c(0.7,1),ylim=c(0.7,1),xlab = "FPR",ylab = "TPR",main = "Euclidean ROC curve",sub="FPR vs TPR",col="red")
abline(a=0,b=1,col="blue")
plot(k,eu_accuracy,xlab = "K",ylab = "Accuracy",main = "Euclidean KNN",col="red",type="o")

manhattan_performance <-data.frame(k,man_TPR,man_FPR,man_Precision,man_accuracy,man_error_rate,man_Recall)
plot(man_FPR,man_TPR,xlim=c(0.7,1),ylim=c(0.7,1),xlab = "FPR",ylab = "TPR",main = "Manhattan ROC curve",sub="FPR vs TPR",col="red")
abline(a=0,b=1,col="blue")
plot(k,man_accuracy,xlab = "K",ylab = "Accuracy",main = "Manhattan KNN",col="red",type="o")


income_result_eu_df<-data.frame(eu_mat_Test)
income_result_eu_df$Actual<-as.factor(income_result_eu_df$Actual)
income_result_eu_df$Predicted<-as.factor(income_result_eu_df$Predicted)
levels(income_result_eu_df$Actual)<-c("<=50K",">50K")
levels(income_result_eu_df$Predicted)<-c("<=50K",">50K")

income_result_man_df<-data.frame(man_mat_Test)
income_result_man_df$Actual<-as.factor(income_result_man_df$Actual)
income_result_man_df$Predicted<-as.factor(income_result_man_df$Predicted)
levels(income_result_man_df$Actual)<-c("<=50K",">50K")
levels(income_result_man_df$Predicted)<-c("<=50K",">50K")

library(xlsx)
write.csv(income_result_eu_df,file="income_euclid_KNN.csv")
write.csv(income_result_man_df,file="income_manhattan_KNN.csv")
