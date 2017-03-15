knn <- 10

## Reading the iris dataset from file
cat("\n","Select the Iris Training dataset","\n")
iris_train_df<-read.csv(file.choose())

size<-dim(iris_train_df)
iris_train_row<-size[1]
iris_train_col<-size[2]

cat("\n","Select the Iris Test dataset","\n")
iris_test_df<-read.csv(file.choose())

size<-dim(iris_test_df)
iris_test_row<-size[1]
iris_test_col<-size[2]

predict_class_type<-function(class_type){
  temp <- table(as.vector(class_type))
  z<-names(temp)[temp==max(temp)]
  ds<-as.integer(z)
  if(length(ds)>1)
    return (ds[1])
  else
    return (ds)
}


knn<-3  
max_knn<-35
perf_size<-(max_knn-knn)/2+1

man_error_rate<-vector(mode ="numeric",length = perf_size)
man_accuracy<-vector(mode ="numeric",length = perf_size)

eu_error_rate<-vector(mode ="numeric",length = perf_size)
eu_accuracy<-vector(mode ="numeric",length = perf_size)

count<-1
while(knn<= max_knn)
{
  euclid_iris<-function()
  {
    eu_mat <- matrix(0L,nrow = iris_test_row,ncol = 4,byrow = T)
    colnames(eu_mat) <- c("Transaction ID","Actual class","Predicted class","Posterior Prob")
    
    for(i in 1:iris_test_row)
    {	
      mat <- matrix(,nrow=iris_train_row,ncol=3,byrow=T)
      colnames(mat) <- c("index","euclid distance","class")
      class_type<-vector(mode ="integer",length = knn)
      for(j in 1:iris_train_row)
      {
        k1 <- ((iris_test_df[[1]][i] - iris_train_df[[1]][j])^2) 
        k2 <- ((iris_test_df[[2]][i] - iris_train_df[[2]][j])^2) 
        k3 <- ((iris_test_df[[3]][i] - iris_train_df[[3]][j])^2) 
        k4 <- ((iris_test_df[[4]][i] - iris_train_df[[4]][j])^2) 
        
        d <- (k1+k2+k3+k4)^0.5
        d<-round(d,digits=3)
        
        mat[j,1] <- j
        mat[j,2] <- d
        mat[j,3] <- as.numeric(iris_train_df[[5]][j])
      }
      y <- order(mat[,2])
      sortedMat <- mat[y,]
      
      ## Now to save it in a result matrix
      eu_mat[i,1]<- i
      
      eu_mat[i,2]<- iris_test_df[["class"]][i]
      c1sum<-0
      c2sum<-0
      c3sum<-0
      for(p in 1:knn)
      {
        class_type[p]<- iris_train_df[["class"]][sortedMat[p,1]]
        d <- sortedMat[p,2];
        if(d==0)
          d<-0.0005
        if(sortedMat[p,3]==1)
          c1sum <- c2sum + (1/(d^2))
        else if(sortedMat[p,3]==2)
          c2sum <- c2sum + (1/(d^2))
        else
          c3sum <- c3sum + (1/(d^2))
      }
      pred_class<-predict_class_type(class_type)
      eu_mat[i,3]<- pred_class
      
      if(pred_class==1)
        eu_mat[i,4] <- c1sum/(c1sum+c2sum+c3sum)
      else if(pred_class==2)
        eu_mat[i,4] <- c2sum/(c1sum+c2sum+c3sum)
      else
        eu_mat[i,4] <- c3sum/(c1sum+c2sum+c3sum)
    }
    return(eu_mat)
  }
  eu_mat <- euclid_iris()
  
  Manhattan_iris<-function()
  {
    man_mat <- matrix(0L,nrow = iris_test_row,ncol = 4,byrow = T)
    colnames(man_mat) <- c("Transaction ID","Actual class","Predicted class","Posterior Probability")
    
    for(i in 1:iris_test_row)
    {	
      mat <- matrix(,nrow=iris_train_row,ncol=3,byrow=T)
      colnames(mat) <- c("index","Manhattan distance","class")
      class_type<-vector(mode ="integer",length = knn)
      for(j in 1:iris_train_row)
      {
        k1 <- abs((iris_test_df[[1]][i] - iris_train_df[[1]][j])) 
        k2 <- abs((iris_test_df[[2]][i] - iris_train_df[[2]][j])) 
        k3 <- abs((iris_test_df[[3]][i] - iris_train_df[[3]][j])) 
        k4 <- abs((iris_test_df[[4]][i] - iris_train_df[[4]][j])) 
        
        d <- (k1+k2+k3+k4)
        d<-round(d,digits=3)
        
        mat[j,1] <- j
        mat[j,2] <- d
        mat[j,3] <- as.numeric(iris_train_df[[5]][j])
      }
      y <- order(mat[,2])
      sortedMat <- mat[y,]
      
      ## Now to save it in a result matrix
      man_mat[i,1]<- i
      man_mat[i,2]<- iris_test_df[["class"]][i]
      
      c1sum <- 0
      c2sum <- 1
      c3sum <- 2
      for(p in 1:knn)
      {
        class_type[p]<- iris_train_df[["class"]][sortedMat[p,1]]
        d <- sortedMat[p,2];
        if(d==0)
          d<-0.0005
        if(sortedMat[p,3]==1)
          c1sum <- c2sum + (1/(d^2))
        else if(sortedMat[p,3]==2)
          c2sum <- c2sum + (1/(d^2))
        else
          c3sum <- c3sum + (1/(d^2))
      }
      pred_class<-predict_class_type(class_type)
      man_mat[i,3]<- pred_class
      if(pred_class==1)
        man_mat[i,4] <- c1sum/(c1sum+c2sum+c3sum)
      else if(pred_class==2)
        man_mat[i,4] <- c2sum/(c1sum+c2sum+c3sum)
      else
        man_mat[i,4] <- c3sum/(c1sum+c2sum+c3sum)
    }
    return(man_mat)
  }
  
  man_mat <- Manhattan_iris()
  
  #Calculation of the confusion matrix
  actual_eu <- eu_mat[,2]
  predicted_eu <- eu_mat[,3]
  
  eu_confusion_matrix <- table(actual_eu,predicted_eu)
  eu_accuracy[count] <- sum(diag(eu_confusion_matrix))/sum(eu_confusion_matrix)
  eu_error_rate[count] <- 1-eu_accuracy[count]
  
  actual_man <- man_mat[,2]
  predicted_man <- man_mat[,3]
  
  man_confusion_matrix <- table(actual_man,predicted_man)
  man_accuracy[count] <- sum(diag(man_confusion_matrix))/sum(man_confusion_matrix)
  man_error_rate[count] <- 1-man_accuracy[count]
  
  
  
  count<-count+1
  
  knn<-knn+2
}

k<-seq(3,max_knn,by=2)

euclidean_performance <-data.frame(k,eu_accuracy,eu_error_rate)

manhattan_performance <-data.frame(k,man_accuracy,man_error_rate)


iris_result_eu_df<-data.frame(eu_mat)

iris_result_eu_df$Actual.class<-as.factor(iris_result_eu_df$Actual.class)
iris_result_eu_df$Predicted.class<-as.factor(iris_result_eu_df$Predicted.class)
levels(iris_result_eu_df$Actual.class)<-c("Iris-setosa","Iris-versicolor","Iris-virginica")
levels(iris_result_eu_df$Predicted.class)<-c("Iris-setosa","Iris-versicolor","Iris-virginica")

iris_result_man_df<-data.frame(man_mat)

iris_result_man_df$Actual.class<-as.factor(iris_result_man_df$Actual.class)
iris_result_man_df$Predicted.class<-as.factor(iris_result_man_df$Predicted.class)
levels(iris_result_man_df$Actual.class)<-c("Iris-setosa","Iris-versicolor","Iris-virginica")
levels(iris_result_man_df$Predicted.class)<-c("Iris-setosa","Iris-versicolor","Iris-virginica")

plot(k,eu_accuracy,xlab = "K",ylab = "Accuracy",main = "Euclidean KNN",col="red",type="o")
plot(k,eu_accuracy,xlab = "K",ylab = "Accuracy",main = "Manhattan KNN",col="red",type="o")

#Load xls lib for exporting distances to csv files 
library(xlsx)

write.csv(iris_result_eu_df,file="iris_euclidean_KNN.csv")
write.csv(iris_result_man_df,file="iris_manhattan_KNN.csv")


