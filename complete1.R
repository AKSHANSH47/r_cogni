complete1 <- function(directory, id =1:332) {
  #set the path
  path = paste(getwd(),directory,sep = "/")
  
  #get the file List in that directory
  fileList = list.files(path,full.names = TRUE)
  
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
  
  ak<-data.frame()
  
  
  ak<-data.frame(table(a$ID))
  colnames(ak)<-(c("id","nobs"))
  ak$id <- as.numeric(as.character(ak$id))
  freq <- rep(NA, 332)
  
   freq[ak$id] <- ak$nobs
   freqdf <- as.data.frame(cbind(var = 1:332, freq))
  
  
  return(freqdf)
  
  
  
}


