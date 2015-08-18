
ran_sam<-sample(1:nrow(sam),sample(1:0.1*nrow(sam),1))
sam[ran_sam,"NAme"]<-NA
mll<-rep(NA,NCOL(iris))
newa= 
function(df)
{
  
  

  rew=
  function(x){
    
     
    ran_sam<-sample(1:length(df[,x]),size=sample(1:(0.1*length(df[,x])),1))
    df[ran_sam,x]<<-NA
  }

  sapply(names(df), rew)
  print(sapply(df,function(x)length(which(is.na(x)))))
  
  return(df)
  
}
arjjr<-rep(NA,ncol(iris))
newa2=
  function(dff)
  {for(i in 1:ncol(dff)){
    dff[is.na(dff[,i]), i] <- mean(dff[,i], na.rm = TRUE)
  }
    
    
    sapply(dff)
       
    
 return(dff)
  }

data("iris")
iris = newa(iris)

irisw=sapply(names(iris), function(x)mean(iris,na.rm =T))
irisw
df= replace(df,df[is.na(df)],irisw)



x <- 1:10
if(x > 5) {
  x <- 0
}
