cat("\n","Select the Clustering dataset","\n")
twodimeasy_df<-read.csv(file.choose())

cat("\n","Select the no of clusters","\n")
k<-4

size<-dim(twodimeasy_df)

twodim_row<-size[1]
twodim_col<-size[2]

twodim_mat<-as.matrix(twodimeasy_df)
twodim_mat<-twodim_mat[,-c(4)]

#centroid_dummy<-twodim_mat
#centroid<-matrix(data = 0,nrow=k,ncol=twodim_col-2)
centroid<-twodim_mat[sample(1:twodim_col, k, replace=FALSE), ]
centroid<-centroid[,-c(1)]

if(k==4)
{
  centroid[1,1] <-0.3335188
    centroid[1,2]<-0.7745047
    centroid[2,1]<-0.2767239
    centroid[2,2]<-0.2767239
    centroid[3,1]<-0.3464067
    centroid[3,2]<-0.7976954
    centroid[4,1]<-0.2960329
    centroid[4,2]<-0.8453815
}

dist_mat<-matrix(0,nrow=twodim_row,ncol=k,byrow = T)
temp_mat<-matrix(0,nrow=twodim_row,ncol=k,byrow = T)

cluster_mat<-matrix(0,nrow=twodim_row,ncol=2)
cluster_mat[,1]<-twodim_mat[,1]
temp_cluster_mat<-matrix(0,nrow=twodim_row,ncol=2)
temp_cluster_mat[,1]<-twodim_mat[,1]



euclid_distance <- function()
{
  dist_row<-vector(mode ="numeric",length = 2)
    for(j in 1:twodim_row)
  {
   
    for(i in 1:k)
    {
      dist_row[1]<-(centroid[i,1]-twodim_mat[j,2])^2
      dist_row[2]<-(centroid[i,2]-twodim_mat[j,3])^2
      temp_mat[j,i]<-sum(dist_row)^0.5
    }
    }
  return (temp_mat)
}


find_cluster<- function()
{
  for(i in 1:twodim_row)
  {
    temp_cluster_mat[i,2]<-which.min(temp_mat[i,])
  }
  return (temp_cluster_mat)
}

find_centroid<-function()
{
  size<-dim(twodimeasy_df)
  
  twodim_row<-size[1]
  twodim_col<-size[2]
  
  twodimeasy_df$cluster_id<-cbind(cluster_mat[,2])

  for(i in 2:twodim_col-1)
  {
    y<-aggregate(twodimeasy_df[[i]] ~ cluster_id, twodimeasy_df, mean)
    
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
  
  if(identical(temp_cluster_mat,cluster_mat))
  {
    cat("\n","clustering process done","\n")
    twodimeasy_df$clusterpredictor<-cbind(cluster_mat[,2])
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
  #print(head(ind_cluster_mat))
  size<-dim(ind_cluster)
  dist<-0
  ind_row<-size[1]
  ind_col<-size[2]
 for(m in 1:ind_row)
 {
   #distance<-0
   for(j in 2:(ind_col-3))
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

compute_SSE<-function(twodimeasy_df,k){
    sh<-split(twodimeasy_df,twodimeasy_df$clusterpredictor)
    str(sh)
  sse_cluster<-vector(mode ="numeric",length = k)
  
  
  for(i in 1:k)
  {
    ind_cluster<-sh[[i]]
   # print(head(ind_cluster[i]))
   sse_cluster[i]<- compute_euclid(ind_cluster,centroid,i)
  }
  return (sse_cluster)
}

sse_cluster<-vector(mode ="numeric",length = k)
sse_cluster<-compute_SSE(twodimeasy_df,k)
total_sse_cluster<-sum(sse_cluster)



size_entire_df<-dim(twodimeasy_df)
col_size<-size_entire_df[2]

centroid_true<-matrix(0,nrow=k,ncol=col_size)
compute_true_centroid<-function(twodimeasy_df,k)
{
  sh<-split(twodimeasy_df,twodimeasy_df$cluster)
  centroid_true<-matrix(0,nrow=k,ncol=col_size)
  
  
  for(i in 1:k)
  {
    ind_cluster<-sh[[i]]
    a<-colMeans(ind_cluster)
    centroid_true[i,]<- a
  }
  
  return (centroid_true)
}

#centroid_true<-compute_true_centroid(twodimeasy_df,k)
centroid_true<-centroid_true[,-c(1,4,5)]



compute_euclid_true<-function(ind_cluster,centroid_true,i)
{
  
  ind_cluster_mat<-as.matrix(ind_cluster)
  #print(head(ind_cluster_mat))
  size<-dim(ind_cluster)
  dist<-0
  ind_row<-size[1]
  ind_col<-size[2]
  for(m in 1:ind_row)
  {
    #distance<-0
    for(j in 2:(ind_col-3))
    {
      temp<-((centroid_true[i,j-1])-(ind_cluster_mat[m,j]))^2
      # print(temp)
      dist<-dist+temp
      # print(distance)
    }
  }
  # print(dist)
  return (dist) 
}

compute_SSE_true<-function(twodimeasy_df,k){
  sh<-split(twodimeasy_df,twodimeasy_df$cluster)
  str(sh)
  sse_cluster_true<-vector(mode ="numeric",length = k)
  
  
  for(i in 1:k)
  {
    ind_cluster<-sh[[i]]
    #print(head(ind_cluster[i]))
    sse_cluster_true[i]<- compute_euclid_true(ind_cluster,centroid_true,i)
  }
  return (sse_cluster_true)
}

sse_cluster_true<-vector(mode ="numeric",length = k)
#sse_cluster_true<-compute_SSE_true(twodimeasy_df,k)
total_true_sse_cluster<-sum(sse_cluster_true)




compute_SSB<-function(twodimeasy_df,k){
  
  sh<-split(twodimeasy_df,twodimeasy_df$clusterpredictor)
  ssb_cluster_size<-vector(mode ="numeric",length = k)
  size_entire_df<-dim(twodimeasy_df)
  col_size<-size_entire_df[2]
  
  mean_pt<-colMeans(twodimeasy_df)
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

final_ssb<-compute_SSB(twodimeasy_df,k)




compute_SSB_true<-function(twodimeasy_df,k){
  
  sh<-split(twodimeasy_df,twodimeasy_df$cluster)
  ssb_cluster_size<-vector(mode ="numeric",length = k)
  size_entire_df<-dim(twodimeasy_df)
  col_size<-size_entire_df[2]
  
  mean_pt<-colMeans(twodimeasy_df)
  total_ssb<-0
  centroid_true<-matrix(0,nrow=k,ncol=col_size)
  
  
  for(i in 1:k)
  {
    ind_cluster<-sh[[i]]
    a<-colMeans(ind_cluster)
    centroid_true[i,]<- a
    size<-dim(ind_cluster)
    ssb_cluster_size[i]<-size[1]
  }
  
  centroid_true<-centroid_true[,-c(1,4,5)]
  
  for(i in 1:k)
  {
    for(j in 2:(col_size-2))
    {
      total_ssb<-total_ssb+ssb_cluster_size[i]*(centroid_true[i,j-1]-mean_pt[j])^2
     # print(total_ssb)
    }
    
  }
  return(total_ssb) 
}

#final_ssb_true<-compute_SSB_true(twodimeasy_df,k)



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
  sh<-split(twodimeasy_df,twodimeasy_df$clusterpredictor)
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
clusplot(twodimeasy_df,twodimeasy_df$cluster,color = TRUE,shade = TRUE,labels = 4,lines = 0,col.p = "dark red", main = "TwoDim Clusters")
clusplot(twodimeasy_df,twodimeasy_df$clusterpredictor,color = TRUE,shade = TRUE,labels = 4,lines = 0,col.p = "dark green",add = TRUE)

cluster_df<-as.data.frame(cluster_mat)
colnames(cluster_df)<-c("ID","cluster")
cluster_df$ID<-NULL

write.csv(cluster_df,file="TwoDim_cluster.csv")

