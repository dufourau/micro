mydata = read.csv("/home/dufourau/3A/micro/data.csv",stringsAsFactors=FALSE) 


nextStockNumber<-mydata$stock_number[1];


regVector<-NULL;


#Loop over all 
for(i in min(mydata$year):max(mydata$year)){
  #Loop over all month
  for(j in 1:12){
    rentaVector<-NULL;
    aimVector<-NULL;
    betaVector<-NULL;
    #Loop over all rows
    for(z in 2:nrow(mydata)-1){
      
      
        if(mydata$year[z]== i && mydata$month[z] == j && !is.na(mydata$betaPrec[z])){
          betaVector<-c(betaVector,mydata$betaPrec[z]);
          #aimVector<-c(aimVector,mydata$AIM[z]);
          rentaVector<-c(rentaVector,as.numeric(mydata$return_rf[z]));
        }
      
      
      
    }
    if(!is.null(rentaVector) &&  !is.null(betaVector)){
      #Regression between r and beta
      reg<- lm(rentaVector~betaVector);
      regVector<-c(regVector,summary(reg)$coefficients[2])
    }
    
  }
}

t.test(regVector, mu=0);




