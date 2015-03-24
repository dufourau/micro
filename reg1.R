mydata = read.csv("/home/dufourau/3A/micro/data.csv",stringsAsFactors=FALSE) 


nextStockNumber<-mydata$stock_number[1];

rentaVector<-NULL;
aimVector<-NULL;
betaVector<-NULL;

#Loop over all yearsmin(mydata$year)
for(i in 1990:1990){
  #Loop over all month
  for(j in 3:3){
    #Loop over all rows
    for(z in 2:nrow(mydata)-1){
      
      
        if(mydata$year[z]== i && mydata$month[z] == j && !is.na(mydata$betaPrec[z])){
          betaVector<-c(betaVector,mydata$betaPrec[z]);
          #aimVector<-c(aimVector,mydata$AIM[z]);
          rentaVector<-c(rentaVector,mydata$return_rf[z]);
        }
      
      
      
    }
    reg<- lm(rentaVector~betaVector);
    summary(reg);
    
  }
}

