cat("\n","Select the Wine Clustering dataset","\n")
wine_df<-read.csv(file.choose())

k<-5

size<-dim(wine_df)
wine_row<-size[1]
wine_col<-size[2]

wine_class_vector<-as.vector(wine_df$class)
wine_df<-wine_df[-c(14)]
wine_df_mean<-colMeans(wine_df)
wine_df$class<-cbind(wine_class_vector)
wine_df_sd<-vector(mode ="numeric",length = wine_col-1)
outlier_index_vec<-vector(mode ="integer",length = wine_row)

find_wine_sd<-function()
{
  for(i in 1:(wine_col-1))
  {
    wine_df_sd[i]<-sd(wine_df[[i]])
  }
  return (wine_df_sd)
}

wine_df_sd<-find_wine_sd()

#Compute all rows that need to be removed

count<-0
for(i in 1:wine_row)
{
  
  for(j in 2:(wine_col-2))
  {
    if(wine_df[[j]][i]>(wine_df_mean[j]+3*wine_df_sd[j]) | wine_df[[j]][i]<(wine_df_mean[j]-3*wine_df_sd[j]))
    {
      outlier_index_vec[count] <- i;
      count<-count+1
    }
  }
}

wine_df <- wine_df[-outlier_index_vec,]


wine_org_df<-wine_df
wine_df<-wine_df[-c(13,14)]

size<-dim(wine_df)
wine_row<-size[1]
wine_col<-size[2]

normalize_cols<-function()
{
  wine_mat<-as.matrix(wine_df) 
  wine_col_min<-apply(wine_mat,2,min)
  wine_col_max<-apply(wine_mat,2,max)
  
  for(i in 1:wine_row)
  {
    for(j in 2:wine_col)
    {
      wine_df[[j]][i]<- (wine_df[[j]][i]-wine_col_min[j])/(wine_col_max[j]-wine_col_min[j])
    }
  }
  return (wine_df)
}

wine_df<-normalize_cols()


wine_mat<-as.matrix(wine_df)

centroid<-wine_mat[sample(1:wine_col, k, replace=FALSE), ]
centroid<-centroid[,-c(1)]
size_centroid<-dim(centroid)
prev_centroid<-centroid


dist_mat<-matrix(0,nrow=wine_row,ncol=k,byrow = T)
temp_mat<-matrix(0,nrow=wine_row,ncol=k,byrow = T)

cluster_mat<-matrix(0,nrow=wine_row,ncol=2)
cluster_mat[,1]<-wine_mat[,1]
temp_cluster_mat<-matrix(0,nrow=wine_row,ncol=2)
temp_cluster_mat[,1]<-wine_mat[,1]


euclid_distance <- function()
{
  dist_row<-vector(mode ="numeric",length = wine_col-1)
  for(j in 1:wine_row)
  {
    
    for(i in 1:k)
    {
      dist_row[1]<-(centroid[i,1]-wine_df[j,2])^2
      dist_row[2]<-(centroid[i,2]-wine_df[j,3])^2
      dist_row[3]<-(centroid[i,3]-wine_df[j,4])^2
      dist_row[4]<-(centroid[i,4]-wine_df[j,5])^2
      dist_row[5]<-(centroid[i,5]-wine_df[j,6])^2
      dist_row[6]<-(centroid[i,6]-wine_df[j,7])^2
      dist_row[7]<-(centroid[i,7]-wine_df[j,8])^2
      dist_row[8]<-(centroid[i,8]-wine_df[j,9])^2
      dist_row[9]<-(centroid[i,9]-wine_df[j,10])^2
      dist_row[10]<-(centroid[i,10]-wine_df[j,11])^2
      dist_row[11]<-(centroid[i,11]-wine_df[j,12])^2
      temp_mat[j,i]<-sum(dist_row)^0.5
    }
  }
  return (temp_mat)
}


find_cluster<- function()
{
  for(i in 1:wine_row)
  {
    temp_cluster_mat[i,2]<-which.min(temp_mat[i,])
  }
  return (temp_cluster_mat)
}

find_centroid<-function()
{
  size<-dim(wine_df)
  
  wine_row<-size[1]
  wine_col<-size[2]
  
  wine_df$cluster_id<-cbind(cluster_mat[,2])
  
  for(i in 2:wine_col-1)
  {
    y<-aggregate(wine_df[[i]] ~ cluster_id, wine_df, mean)
    
    for(j in 1:k)
    {
      centroid[j,i-1]<-y[[2]][j]
    }
    
  }
  return (centroid)
}

count<-0
while(count<=100)
{
  count<-count+1
  temp_mat<-euclid_distance()
  temp_cluster_mat<-find_cluster()
  #centroid<-find_centroid()
  
  if(identical(temp_cluster_mat,cluster_mat))
  {
    cat("\n","clustering process done","\n")
   # wine_df$class <-wine_org_df$class
    wine_df$quality<-wine_org_df$quality
    wine_df$clusterpredictor<-cbind(cluster_mat[,2])
    break;
  }
  else
  {
    cluster_mat<-temp_cluster_mat
    centroid<-find_centroid()
    
  }
  
}




compute_euclid<-function(ind_cluster,centroid,i)
{
  
  ind_cluster_mat<-as.matrix(ind_cluster)
  print(head(ind_cluster_mat))
  size<-dim(ind_cluster)
  dist<-0
  ind_row<-size[1]
  ind_col<-size[2]
  for(m in 1:ind_row)
  {
    #cat("\n","m value is ")
    #print(m)
    #distance<-0
    for(j in 2:(ind_col-2))
    {
      temp<-((centroid[i,j-1])-(ind_cluster_mat[m,j]))^2
      # print(temp)
      dist<-dist+temp
      # print(distance)
    }
  }
  # print(dist)
  return (dist) 
}

compute_SSE<-function(wine_df,k){
  sh<-split(wine_df,wine_df$clusterpredictor)
  str(sh)
  sse_cluster<-vector(mode ="numeric",length = k)
  
  
  for(i in 1:k)
  {
    ind_cluster<-sh[[i]]
    print(head(ind_cluster[i]))
    sse_cluster[i]<- compute_euclid(ind_cluster,centroid,i)
  }
  return (sse_cluster)
}

sse_cluster<-vector(mode ="numeric",length = k)
sse_cluster<-compute_SSE(wine_df,k)
total_sse_cluster<-sum(sse_cluster)

compute_SSB<-function(wine_df,k){
  
  sh<-split(wine_df,wine_df$clusterpredictor)
  ssb_cluster_size<-vector(mode ="numeric",length = k)
  size_entire_df<-dim(wine_df)
  col_size<-size_entire_df[2]
  
  mean_pt<-colMeans(wine_df)
  total_ssb<-0
  
  for(i in 1:k)
  {
    ind_cluster<-sh[[i]]
    size<-dim(ind_cluster)
    ssb_cluster_size[i]<-size[1]
  }
  
  for(i in 1:k)
  {
    for(j in 2:(col_size-2))
    {
      total_ssb<-total_ssb+ssb_cluster_size[i]*(centroid[i,j-1]-mean_pt[j])^2
      print(total_ssb)
    }
    
  }
  return(total_ssb) 
}

final_ssb<-compute_SSB(wine_df,k)




compute_silhouette<-function(each_cluster,each_cluster_sil_coeff,i,j,k)
{
  target_cluster_mat<-as.matrix(each_cluster[[i]])
  
  for(a in 1:k) 
  {
    no_rows<-dim(each_cluster[[a]])[1]
    no_cols<-dim(each_cluster[[a]])[2]
    each_cluster_mat<-as.matrix(each_cluster[[a]])
    for(b in 1:no_rows)
    {
      #each_cluster_mat<-as.matrix(each_cluster[[a]])
      dist<-0
      total_dist<-0
      for(c in 2:(no_cols-2))
      {
        dist<-dist+ (each_cluster_mat[b,c]-target_cluster_mat[j,c])^2
      }
      
      total_dist<-total_dist+(dist)^0.5
      
    }
    #print(total_dist)
    each_cluster_sil_coeff[a,j]<-total_dist/no_rows
    
  }
  return (each_cluster_sil_coeff)
  
}

cluster_silh_coeff<-list()

silhouette_coeff<-function()
{
  sh<-split(wine_df,wine_df$clusterpredictor)
  each_cluster_size<-vector(mode ="numeric",length = k)
  each_cluster<-list()
  for(i in 1:k)
  {
    each_cluster[[i]]<-sh[[i]]
    size<-dim(each_cluster[[i]])
    each_cluster_size[i]<-size[1]
    
  }
  
  
  # each_cluster_sil_coeff <- matrix(0L,nrow = k,ncol = each_cluster_size[1],byrow = T)
  for(m in 1:k)
  { 
    each_cluster_sil_coeff <- matrix(0L,nrow = k,ncol = each_cluster_size[m],byrow = T)
    for(j in 1:each_cluster_size[m])
    {
      each_cluster_sil_coeff<-compute_silhouette(each_cluster,each_cluster_sil_coeff,m,j,k) 
    }
    
    cluster_silh_coeff[[m]]<-each_cluster_sil_coeff
    #print(cluster_silh_coeff[[1]])
  }
  return (cluster_silh_coeff)
}

was<-silhouette_coeff()


silhoutte_values_cluster<-list()

compute_silh_values<-function(was)
{
  each_cluster<-list()
  each_cluster_size<-vector(mode ="numeric",length = k)
  
  for(i in 1:k)
  {
    each_cluster[[i]]<-was[[i]]
    size<-dim(each_cluster[[i]])
    each_cluster_size[i]<-size[2]
    
  }
  
  for(m in 1:k)
  {
    each_cluster_mat<-as.matrix(each_cluster[[m]])
    a<-vector(mode ="numeric",length = each_cluster_size[m])
    b<-vector(mode ="numeric",length = each_cluster_size[m]) 
    
    silh_values<-vector(mode ="numeric",length = each_cluster_size[m])
    
    for(j in 1:each_cluster_size[m])
    {
      a[j]<-each_cluster_mat[m,j]
      without_a<-each_cluster_mat[,j]
      without_a<-without_a[-c(m)]
      b[j]<-min(without_a)
      
    }
    
    
    for(p in 1:each_cluster_size[m])
    {
      #silh_values[p]<-1-(a[p]/b[p]) 
      silh_values[p]<-(b[p]-a[p])/max(b[p],a[p])
    }
    
    silhoutte_values_cluster[[m]]<-silh_values
    
  }
  return (silhoutte_values_cluster)
  
}


silhoutte_values_cluster<-compute_silh_values(was)



avg_silhuotte<-function(silhoutte_values_cluster,k)
{
  avg_silh_values<-vector(mode ="numeric",length = k)
  
  for(i in 1:k)
  {
    avg_silh_values[i]<-sum(silhoutte_values_cluster[[i]])/length(silhoutte_values_cluster[[i]])
  }
  
  return (avg_silh_values)
  
}

avg_silh_values<-avg_silhuotte(silhoutte_values_cluster,k)


avg_silhuotte_entire_set<-function(avg_silh_values,k)
{
  total<-0
  total_len<-0
  for(i in 1:k)
  {
    total<-total+sum(silhoutte_values_cluster[[i]])
  }
  
  for(i in 1:k)
  {
    total_len<-total_len+length(silhoutte_values_cluster[[i]])
  }
  
  return(total/total_len)
  
}

total_silhuotte<-avg_silhuotte_entire_set(avg_silh_values,k)

library(cluster)
#clusplot(wine_df,wine_df$quality,color = TRUE,shade = TRUE,labels = 4,lines = 0,col.p = "dark red", main = "Wine Clusters")
#clusplot(wine_df,wine_df$clusterpredictor,color = TRUE,shade = TRUE,labels = 4,lines = 0,col.p = "dark green",add = TRUE)

#cluster_mat<-cluster_mat[,-c(1)]
cluster_df<-as.data.frame(cluster_mat)
colnames(cluster_df)<-c("ID","cluster")
cluster_df$ID<-NULL

write.csv(cluster_df,file="wine_cluster.csv")
