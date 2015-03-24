mydata = read.csv("/home/dufourau/3A/micro/data_final.csv",stringsAsFactors=FALSE) 
#Add a column to store the previous beta 
mydata$betaPrec<- 0
mydata$AIMPrec <- 0
currentStockNumber<-mydata$stock_number[1];
currentYear<- mydata$year[1];
currentMonth<- mydata$month[1];
mydata$betaPrec[1] <- NA;
mydta$AIMPrec[1] <- NA;
precAIM<-mydata$AIM[1];
precBeta<-mydata$beta[1];
#Loop over all rows
for(i in 2:nrow(mydata)-1){
  mydata$stock_number[i];
  #Same asset
  if(mydata$stock_number[i]==currentStockNumber){
    
    #Check if the date are fit
    if((currentYear == mydata$year[i]-1 && currentMonth == 12) || (currentYear == mydata$year[i] && currentMonth == mydata$month[i]-1) ){
      
      mydata$betaPrec[i]= precBeta;
      mydta$AIMPrec[i]= precAIM;
    
    }else{
      mydata$betaPrec[i]= NA;
      mydta$AIMPrec[i]= NA;
      
    }
  #New asset
  }else{
      currentStockNumber= mydata$stock_number[i];
  }  
  
  precBeta<-mydata$beta[i];
  precAIM<-mydata$AIM[i];
  currentYear<- mydata$year[i];
  currentMonth<- mydata$month[i];
}

print("Write in data.csv");
write.csv(mydata,file="/home/dufourau/3A/micro/data.csv");


  