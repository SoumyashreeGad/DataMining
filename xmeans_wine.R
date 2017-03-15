cat("\n","Select the Wine Clustering dataset","\n")
wine_xmeans_df<-read.csv(file.choose())


size<-dim(wine_xmeans_df)
wine_row<-size[1]
wine_col<-size[2]

cat("\n","Select the original Wine dataset","\n")
wine_entire_df<-read.csv(file.choose())

clusterpredictor<-wine_xmeans_df$Cluster
wine_xmeans_df<-wine_xmeans_df[-c(13)]


wine_xmeans_df$quality<-wine_entire_df$quality
a<-as.integer(factor(clusterpredictor))

wine_xmeans_df$clusterpredictor<-cbind(a)

k<-4



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
  sh<-split(wine_xmeans_df,wine_xmeans_df$clusterpredictor)
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
#sse_cluster<-compute_SSE(wine_df,k)


compute_SSB<-function(wine_df,k){
  
  sh<-split(wine_xmeans_df,wine_xmeans_df$clusterpredictor)
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

#final_ssb<-compute_SSB(wine_df,k)




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
  sh<-split(wine_xmeans_df,wine_xmeans_df$clusterpredictor)
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

