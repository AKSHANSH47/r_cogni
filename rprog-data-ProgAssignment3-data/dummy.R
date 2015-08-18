data(iris)
library(randomForest)
library(rpart)
data(iris)
library(doBy)
dummy<-function(data,var){

  if(!is.numeric(data$var)){
    a<-c(table(data[var]))
   
      for (i in 1:(length(a)-1))
      {
        data[which(names(a[i])==data[var]),paste(var,names(a[i]),sep = "_")] = 1
        data[which(names(a[i])!=data[var]),paste(var,names(a[i]),sep = "_")] = 0
      }
      
  
    return(data)
    
  }
  else{
    print("numeric variable")
  }
}
num_vars<-c(names(iris[,]))

xyz<-setdiff(names(iris),"Species")
b = paste("iris",xyz,sep = "$",collapse = "+")
a = paste("iris$Species",b,sep = "~")

my_formula <- paste(b,a,sep ="-")
my_formula <- as.formula(a)




