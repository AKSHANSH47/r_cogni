Car_Data<-read.table("Assignment-11.csv",sep =",",header = T)

#split into train and test dataset
smp_size <- floor(0.80 * nrow(Car_Data))
 
 train_ind <- sample(seq_len(nrow(Car_Data)), size = smp_size)
train <- Car_Data[train_ind, ]
test <- Car_Data[-train_ind, ]

#Q1,Q2,Q3
hist(Car_Data$Price)
 hist(Car_Data$Mileage)
 hist(train$Price)
 hist(train$Mileage)
 hist(train$Cylinder)
 hist(train$Liter)
 hist(train$Doors)
 hist(train$Cruise)
 hist(train$Sound)
 hist(train$Leather)
 c1<-table(train$Make)
 barplot(c1,main= "make of cars")
 c2<-table(train$Model)
 barplot(c1,main= "model of cars")
 c3<-table(train$Trim)
 barplot(c3,main= "model of cars")
 barplot(c3,main= "trim of cars")
 c4<-table(train$Type)
 barplot(c4,main= "type of cars")
 boxplot(train$Price~train$Mileage)
 boxplot(train$Price~train$Cylinder)
 #calculate n
 k1<-rep(NA,12)
   calc_n<-function(a){
       k1[a]<-NROW(na.omit(train[a]))
       return( k1[a])
    }
   for(i in 1:NCOL(train)){k1[i]<-calc_n(i)}
   #calculate NAs
k2<-rep(NA,12)
   calc_na<-function(a){
     k2[a]<-sum(is.na(train[a]))
     return( k2[a])
   }
   for(i in 1:NCOL(train)){k2[i]<-calc_na(i)}
   #calculate sum
   k3<-rep(NA,12)
   calc_sum<-function(a){
     if(is.integer(train[[a]])|is.numeric(train[[a]])){
     k3[a]<-sum((train[a]))
     return( k3[a])
     }
     else{
       return("no numeric type")
     }
   }
   for(i in 1:NCOL(train)){k3[i]<-calc_sum(i)}
   #calculate mean
   k4<-rep(NA,12)
   calc_mean<-function(a){
     if(is.integer(train[[a]])|is.numeric(train[[a]])){
       k4[a]<-mean((train[[a]]))
       return( k4[a])
     }
     else{
       return("no numeric type")
     }
   }
   for(i in 1:NCOL(train)){k4[i]<-calc_mean(i)}
   #calculate median,min,max
   ak<-data.frame()
   k5<-list()
   k6<-list()
   k7<-list()
   calc_rest<-function(a){
     for(i in 1:NCOL(a)){
     if(is.integer(a[[i]])|is.numeric(a[[i]])){
       k3[i]<
       k4[i]<-names(a[i])
       k5[i]<-median((a[[i]]))
       k6[i]<-min((a[[i]]))
       k7[i]<-max((a[[i]]))
       k3[i]<-quantile(a[[i]],0.75)}
       else{
         return("not numeric type")
       }
       for(j in 1:NCOL(a))
     ak<-data.frame( k4[j,i],k5[j,i],k6[j,i],k7[j,i],k3[j,i])
       return(ak)
     }
     
   }
   for(i in 1:NCOL(train)){print( calc_rest(i))}
   
   #p1 to p100
   for(i in 1:NCOL(train)){
   quantile(train[[i]],seq.int(.01,1,by=0.01))
   }
   library("usdm")
   model = lm(Price ~ ., train)
   
   Rsq = summary(model)$r.squared
   
   vif = 1/(1 - Rsq)3
   
   
   d <- data.frame(x1=rnorm(train$Price),
                   x2=rnorm(train$),
                   x3=rnorm(10))
   M <- cor(d) # get correlations
   
   library('corrplot') #package corrplot
   corrplot(M, method = "circle") #plot matrix