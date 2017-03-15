#income_df<-read.csv(file.choose())

size<-dim(income_df)
income_row<-size[1]
income_col<-size[2]

print(income_row)
print(income_col)

#normalize fnwgt using normalization
nfnlwgt<-vector(mode ="integer",length = income_row)

normalize_fnlwgt<-function(){
  min_fnl<-min(income_df[["fnlwgt"]])
  max_fnl<-max(income_df[["fnlwgt"]])
  
  print(min_fnl)
  print(max_fnl)
  size_fnl<-length(income_df[["fnlwgt"]])

  
  
  for(j in 1:income_row){
    nfnlwgt[j]<-(income_df[["fnlwgt"]][j]-min_fnl)/(max_fnl-min_fnl)
    print(nfnlwgt[j])
  }
  return(nfnlwgt)
}


#normalize capital gain-loss using net

net_capital<-vector(mode ="integer",length = income_row)

find_net_capital<-function(){
  min_fnl<-min(income_df[["fnlwgt"]])
  max_fnl<-max(income_df[["fnlwgt"]])
  
  print(min_fnl)
  print(max_fnl)
  size_fnl<-length(income_df[["fnlwgt"]])
  
  
  
  for(j in 1:income_row){
   
    net_capital[j]=income_df[["capital_gain"]][j]-income_df[["capital_loss"]][j]
    print(net_capital[j])
  }
  return(net_capital)
}


net_capital<-factor_hour_per_week()

head(net_capital,20)

#normalize hour_per_week based on divided factor 10 

norm_hour_per_week<-vector(mode ="integer",length = income_row)

factor_hour_per_week<-function(){
   for(j in 1:income_row){
    norm_hour_per_week[j]<-income_df[["hour_per_week"]][j]/10
  }
  return(norm_hour_per_week)
}

norm_hour_per_week<-factor_hour_per_week()

head(norm_hour_per_week,20)

find_mode<-function(){
  mode_attr[1]<-income_df$workclass[3]
  mode_attr[1]
  
}

find_mode()


find_missing<-function(){
  
  for(i in 1:length(income_df$workclass))
  {
    if(income_df[["workclass"]][i]==" ?"){
      income_df[["workclass"]][i]=income_df$workclass[3]
    }
  }
  
  for(i in 1:length(income_df$occupation))
  {
    if(income_df[["occupation"]][i]==" ?"){
      income_df[["occupation"]][i]=income_df$occupation[2]
    }
  }
  
  
  for(i in 1:length(income_df$native_country))
  {
    if(income_df[["native_country"]][i]==" ?"){
      income_df[["native_country"]][i]=income_df$native_country[39]
    }
  }
  
  return(income_df)
    
}

income_df<-find_missing()
