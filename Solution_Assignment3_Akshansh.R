
# use a function to output class,type and number of missing
k3<-list()
k4<-list()
k2<-list()
k5<-list()
k6<-list()
k7<-list()
ak1<-data.frame()
ak2<-data.frame()
calc_rest<-function(a){
  for(i in 1:NCOL(a)){
    
    
    k4[i]<<-names(a[i])
    k5[i]<<-class((a[[i]]))
    k6[i]<<-sum((is.na(a[[i]])))
  }
  ak1<<-data.frame( names = unlist(k4),classes = unlist(k5),missing = unlist(k6))
 
  return(ak1)
  
}

#names classes missing
#1     Price numeric       0
#2   Mileage integer       0
#3      Make  factor       0
#4     Model  factor       0
#5      Trim  factor       0
#6      Type  factor       0
#7  Cylinder integer       0
#8     Liter numeric       0
#9     Doors integer       0
#10   Cruise integer       0
#11    Sound integer       0
#12  Leather integer       0
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

#     names classes   median  maximum  minimum percentil
#1    Price numeric 17803.28 70755.47 8638.931   25704.4
#2  Mileage integer 21266.00 50387.00  266.000   25368.5
#3 Cylinder integer     6.00     8.00    4.000       6.0
#4    Liter numeric     2.80     6.00    1.600       3.8
#5    Doors integer     4.00     4.00    2.000       4.0
#6   Cruise integer     1.00     1.00    0.000       1.0
#7    Sound integer     1.00     1.00    0.000       1.0
#8  Leather integer     1.00     1.00    0.000       1.0

