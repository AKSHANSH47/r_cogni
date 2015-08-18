credit<-read.table("credit_approval.DATA",header =F,sep = ",")

credit<-read.table("credit_approval.DATA",header =F,sep = ",",na.strings = "?")
 table(credit$V1)

 
 ?
 summaryBy(credit$V2+credit$V3,credit$V4+credit$V5) 
 num_vars<-c(names(credit[,]))

 xyz<-setdiff((names(credit),num_vars))
 b = paste(num_vars,collapse = "+")
 a = paste(xyz,collapse = "+")
 
 my_formula <- paste((b,a,sep ="-"))
 my_formula <- as.formula(paste((b,a,sep ="-")))
 
 
 summaryBy(my_formula,credit,na.rm =T,FUN = mean)
 
 
 
 
 
 pollutantmean <- function(directory,pollutant, id=1:332 ) {
   #set the path
   path = paste(getwd(),directory,sep = "/")
   
   #get the file List in that directory
   fileList = list.files(path)
   
   #extract the file names and store as numeric for comparison
   file.names = as.numeric(sub("\\.csv$","",fileList))
   
   #select files to be imported based on the user input or default
   selected.files = fileList[match(id,file.names)]
   
   #import data
   Data = lapply(file.path(path,selected.files),read.csv)
   
   #convert into data frame
   Data = do.call(rbind.data.frame,Data)
  
   mean(Data[,pollutant],na.rm=TRUE)
   
 }
 
 
 
 
 complete <- function(directory, id =1:332) {
   #set the path
   path = paste(getwd(),directory,sep = "/")
   
   #get the file List in that directory
   fileList = list.files(path)
   
   #extract the file names and store as numeric for comparison
   file.names = as.numeric(sub("\\.csv$","",fileList))
   
   #select files to be imported based on the user input or default
   selected.files = fileList[match(id,file.names)]
   
   #import data
   Data = lapply(file.path(path,selected.files),read.csv)
   
   #convert into data frame
   Data = do.call(rbind.data.frame,Data)
   Data = rbind.data.frame(Data,id)
   a<-data.frame(subset(Data,complete.cases(Data)==TRUE))
   
  
  
   ak<-data.frame(table(a$ID))
   colnames(ak)<-(c("id","nobs"))
  
   return(ak)
   
   
   
 }
 result  = data.frame(ID = numeric(), non_miss = numeric())
 invisible(sapply(1: length(unique(Data$ID)), function(x){
                    temp = na.omit(Data[which(as.numeric(Data$ID)==x), ])
                    result[x, 1] <<- x
                    result[x, 2]<<- nrow(temp)
 }))
 
 
 
 corr <- function(directory, threshold=0,id=1:332) {
   
   #set the path
   path = paste(getwd(),directory,sep = "/")
   
   #get the file List in that directory
   fileList = list.files(path)
   
   #extract the file names and store as numeric for comparison
   file.names = as.numeric(sub("\\.csv$","",fileList))
   
   #select files to be imported based on the user input or default
   selected.files = fileList[match(id,file.names)]
   
   #import data
   Data = lapply(file.path(path,selected.files),read.csv)
   
   #convert into data frame
   Data = do.call(rbind.data.frame,Data)
   Data = rbind.data.frame(Data,id)
   a<-data.frame(subset(Data,complete.cases(Data)==TRUE))
   
   
   
   ak<-data.frame(table(a$ID))
   
   colnames(ak)=(c("id","nobs"))
   asd<-data.frame()
   kk=(split(a,a$ID))
   
   for(i in 1:nrow(ak)){
     
     ailo=data.frame(kk[i])
     
     if((ak[i,2])>threshold ){
       
       y = cor(ailo[,c(2,3)])
       asd=rbind(asd,y[2])
       
     }
     }
   
   a=nrow(asd)
   if(a==0){
     return(c(0))
   }
   colnames(asd)= c("correlation")
   return(asd)
   
   
   
 }
 
 
 
 asd<-data.frame()