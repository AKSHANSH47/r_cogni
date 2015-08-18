
facty<-function(n){
  if(floor(n)!=n){
    print("natural number please")
    print(paste("using",floor(n),"instead of",n))
    n = floor(n)
  }
  
  for(i in 1:n){
    fa<<-fa*i
  }
  return(fa)
  
}

 facty(10.9)
 
 
 asd=function(a){
   
                    return(a)
 }
 asd(21)
 
 # use a function to output class,type and number of missing
 k3<-list()
 k4<-list()
 k2<-list()
 k5<-list()
 k6<-list()
 k7<-list()
 ak<-data.frame()
 calc_rest<-function(a){
   for(i in 1:NCOL(a)){
     
       
         k4[i]<<-names(a[i])
         k5[i]<<-class((a[[i]]))
         k6[i]<<-sum((is.na(a[[i]])))
    }
   ak<<-data.frame( names = unlist(k4),classes = unlist(k5),missing = unlist(k6)) 
   return(ak)
   
 }
   
calc_rest(train) 
#use a function to output few common population parameters
k13<-list()
k14<-list()
k15<-list()
k16<-list()
k17<-list()
k31<-list()
aks<-data.frame()
 
 calc_rest2<-function(a){
   for(i in 1:NCOL(a)){
     if(is.integer(a[[i]])|is.numeric(a[[i]])){
       k13[i]<<-names(a[i])
         k14[i]<<-class(a[[i]])
         k15[i]<<-median((a[[i]]))
         k16[i]<<-min((a[[i]]))
         k17[i]<<-max((a[[i]]))
         k31[i]<<-quantile(a[[i]],c(0.75))}
     
     
     
   }
   aks<<-data.frame( names = unlist(k13),classes = unlist(k14),median = unlist(k15),maximum= unlist(k17),minimum = unlist(k16),percentil = unlist(k31)) 
   return(ak)
   
 }