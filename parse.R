dataFilename=paste(getwd(),"/data_final.csv", sep="")
dataFilenameFacteurs=paste(getwd(),"/facteurs.csv", sep="")
mydata = read.csv(dataFilename,stringsAsFactors=FALSE) 
dataFacteurs = read.csv(dataFilenameFacteurs,stringsAsFactors=FALSE) 
#Add a column to store the previous beta 
mydata$betaPrec <- NA
mydata$AIMPrec <- NA
mydata$PINPrec <- NA
mydata$BTMPrec <- NA
mydata$LogCapMarketPrec <- NA
#mydata$fsrb <- 0
#mydata$turnover <- 0

currentStockNumber<-mydata$stock_number[1];
currentYear<- mydata$year[1];
currentMonth<- mydata$month[1];

#mydata$betaPrec[1] <- NA;
#mydata$AIMPrec[1] <- NA;
#mydata$PINPrec[1] <- NA;
#mydata$BTMPrec[1] <- NA;
#mydata$LogCapMarketPrec[1] <- NA;

mydata[["rf"]] = 0;

precBeta<-mydata$beta[1];
precAIM<-mydata$AIM[1];
precPIN<-mydata$pin[1];
precBTM<-mydata$log_btm[1];
precLogCapMarket<-mydata$log_market_cap[1];
#Loop over all rows
for(i in 2:nrow(mydata)-1){
  
  mydata$stock_number[i];
  # Get corresponding rf
  indFacteur = intersect(which(dataFacteurs$year ==currentYear),which(dataFacteurs$month ==currentMonth))
  rf= as.numeric(dataFacteurs$Risk.Free.Return[indFacteur]);
  mydata$rf[i] = rf;
  
  #Same asset
  if(mydata$stock_number[i]==currentStockNumber){
    
    #Check if the date are fit
    if((currentYear == mydata$year[i]-1 && currentMonth == 12) 
       || (currentYear == mydata$year[i] && currentMonth == mydata$month[i]-1) ){
      
      mydata$betaPrec[i]= precBeta; 
      mydata$AIMPrec[i]= precAIM;
      mydata$PINPrec[i]= precPIN;
      mydata$BTMPrec[i]= precBTM;
      if (precLogCapMarket != "."){
        mydata$LogCapMarketPrec[i]= precLogCapMarket;
      }
      
    }
    #New asset
  }else{
    currentStockNumber= mydata$stock_number[i];
  }  
  
  precBeta<-mydata$beta[i];
  precAIM<-mydata$AIM[i];
  precPIN<-mydata$pin[i];
  precBTM<-mydata$log_btm[i];
  precLogCapMarket<-mydata$log_market_cap[i];
  currentYear<- mydata$year[i];
  currentMonth<- mydata$month[i];
}

print("Write in data.csv");
write.csv(mydata,file=paste(getwd(),"/data.csv", sep=""));