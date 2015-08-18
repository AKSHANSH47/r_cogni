ls()
#getting the directory
getwd()
#setting the directory
setwd("C:/Users/akshansh.sinha/Documents/r_folder")

#reading the data from csv file
my_data<-read.table("Input_data1.csv",sep = ",",header = TRUE)

#dimension of the table
dim(my_data)
#names of variables 
names(my_data)
#types of variables
class(my_data$ResortName)
class(my_data$RoomID)
class(my_data$ArrivalDate)
class(my_data$ResStatus)
class(my_data$Roomtype_Holi)
class(my_data$No_of_days_holidayed)

#summary of data frame
summary(my_data)

#type and variable conversion
my_data$ResortName_F<-as.factor(my_data$ResortName)
 class(my_data$ResortName_F)
 
 
 my_data$ResortName_N<-as.numeric(my_data$ResortName_F)
class(my_data$ResortName_N)
  

  my_data$ResortName_C<-as.character(my_data$ResortName_N)
  class(my_data$ResortName_C)

  #converting and adding data type to the table
  my_data$ResortName_F<-as.numeric(my_data$ResortName_C)
  
  #comparing data type factor and numeric
  my_data$conversion_f_2_n<-ifelse(my_data$ResortName_F==my_data$ResortName_N,1,0)
  
  #comparing datat type numeric and character
  my_data$conversion_n_2_c<-ifelse(my_data$ResortName_N==my_data$ResortName_C,0,1)
  
  #sum of conversion_f_2_c and conversion_n_2_c
  sum(my_data$conversion_f_2_n)
  
  sum(my_data$conversion_n_2_c)
  #R implicitly converts data types to higher(through coercion)
  
  #change every 5th element in Resortname to "Gurgaon"
  i<-seq.int(1,12319)
  my_data$ResortName<-ifelse(i%%5==0,"Gurgaon",my_data$ResortName)
#calculate the sum of "no_of_days_holidayed" for each value of "Roomtype_Holi"
  for(i in 1:nrow(my_data)){
    if(my_data$Roomtype_Holi[i]=="1BR")
    aS[1]<-aS[1]+my_data$No_of_days_holidayed[i]
    else if(my_data$Roomtype_Holi[i]=="HU")
    aS[2]<-aS[2]+my_data$No_of_days_holidayed[i]
    else if(my_data$Roomtype_Holi[i]=="STU")
    aS[3]<-aS[3]+my_data$No_of_days_holidayed[i]
    else if(my_data$Roomtype_Holi[i]=="2BR")
    aS[4]<-aS[4]+my_data$No_of_days_holidayed[i]}

#using table function count occurence of different variables in ResStatus
  table(my_data$ResStatus)
  
#average no of holidays
  mean(my_data$No_of_days_holidayed)
  
  #maximum occuring Roomtype_holi
  max(table(my_data$Roomtype_Holi))
  
 (table(my_data$Roomtype_Holi))
  
  #average maximum occuring variable
  
  
 q<- table(my_data$Roomtype_Holi)
  for(i in 1:nrow(q))if(q[i]==max(q))k<-i
  for(i in 1:nrow(my_data))if(my_data$Roomtype_Holi[i]==names(q[k]))sumer<-sumer+my_data$No_of_days_holidayed[i]
  sumer<-sumer/q[k]
  
  
  #above question in one line
  mean(my_data$No_of_days_holidayed[which(my_data$Roomtype_Holi == names(which.max(table(my_data$Roomtype_Holi))))])
  