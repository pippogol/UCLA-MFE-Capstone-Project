###Data Preprocessing
###Construct our X and Y 

ptm<-proc.time()

#Load required Libraries

library(xts)
library(lubridate)
library(data.table)
library(gdata)

#Set Path to Data (sample for the moment)
setwd("~/Academic/Universities/UCLA/Fall_Term/Applied Finance Project/Before_Mid/data_sample")

#Create our own way of rollapplying adding NAs to Moving averages... (will be needed for the next function)
our_rollapply <- function(data,width,FUN) {
  return( c(rep(NA, width-1), rollapply(data=data, width=width, FUN=FUN)))
}

#Create Function that creates all individual features explained in the paper by calling all csv files of different commodities one by one. 
#Correlations will be calculated when all files are uploaded.

data_manip <- function(file1) {
      file1 <- as.data.table(file1)
      file1$X.TIMESTAMP <- ymd_hms(file1$X.TIMESTAMP)
      file1[, c("OPEN", "HIGH", "LOW", "LAST", "VWAP") := .(na.locf(OPEN), na.locf(HIGH), na.locf(LOW), na.locf(LAST), na.locf(VWAP))]
      
      #Create Returns
      file1[, Ret := LAST/shift(LAST)-1]
      
    
      #Create Lags
      lag <- as.data.table(shift(file1$LAST, c(1:100),give.names = T))
      
      #Create Moving Averages
      lista<-list(data=file1$LAST,FUN=mean)
      ALLPOSSCOMBS<-c(5:100)
      MAliste<-mapply(our_rollapply,width=ALLPOSSCOMBS, MoreArgs = lista)
      MAdt<-as.data.table(MAliste)
      
      #Add names if you like at this point (saved at the end as a comment)
      
      #Combine this data to a big data table
      file1 <- cbind(file1, MAdt)
      file1 <- cbind(file1, lag)
      
      #Filter for the hours chosen for the analysis
      file1[, hour := hour(X.TIMESTAMP)]
      file1 <- file1[hour >= 10 & hour < 16]
      
      #return the data table
      return(file1)
}


#Read all csv files that contain the data:

#Get Paths
filenames <- list.files(path = "~/Academic/Universities/UCLA/Fall_Term/Applied Finance Project/Before_Mid/data_sample", pattern="*.csv", full.names=TRUE)

#Create our own read function in order to be able to avoid first row when we apply the function in multiple csvs.
our_read <- function(x) {
      read.csv(x, skip = 1, header = T, stringsAsFactors = F)
}

#Apply the function to all csv files given and create a list for each commodity
futures_list <- lapply(filenames, our_read)

#Create Input Features
X_variables <- lapply(futures_list, data_manip)

proc.time() -ptm


#Names of all comodities

#Create Function to find all names 
substrRight <- function(x, n){
  substr(x, nchar(x)-5, nchar(x)-4)
}

Commodities_Names<-substrRight(filenames)

names(X_variables)<-Commodities_Names

#Get all Returns to find correlations
invisible((lapply(names(X_variables),function(x) assign(x,X_variables[[x]]$Ret,.GlobalEnv))))

##Make this automatic instead of hand inputted
b<-c()
for (i in 1:length(Commodities_Names)){
  a<-Commodities_Names[i]
  if (i==1) {b=a}
  else{
  b<-paste0(b,",",a)}
}
noquote(b)

allret<-rbind(noquote(b))
list(noquote(b))
rbind()

###

allret<-cbind(ES,NQ,TN,UB,YM,ZB,ZC,ZF,ZL,ZM,ZN,ZS,ZT,ZW)

#Create Data.Frames of Commodities
invisible(lapply(names(X_variables),function(x) assign(x,X_variables[[x]],.GlobalEnv)))

#Rolling Correlation Function
manycor <- function(x, n){
 cor(x[1:n,])
}

#Apply that function to returns
lista1<-list(x=allret)
difcor<-(2:dim(allret)[1])
AllCor<-mapply(manycor,n=difcor, MoreArgs = lista1, SIMPLIFY = FALSE)

#Create Function for upper triange values
upTr<- function(x){
  upper.tri(x)*x
}

#Get Upper Triangle Values
UpperCor<-lapply(AllCor,upTr)
#Unlist them
UpperCorVector<-unlist(UpperCor)
#Delete lower Triangle Values (all of them are 0)
UpperCorVector<-UpperCorVector[UpperCorVector!=0]

#Combine X Matrix
Xalm<-as.data.table(X_variables)

#Take out not needed Values
we<-matrix(rep(c(0:9,207),length(Commodities_Names)),ncol=length(Commodities_Names))
takeout<-c(matrix(rep(seq(1,2912,208)),nrow=11,ncol=length(Commodities_Names),byrow=T)+we)
Xalm<-Xalm[, !takeout, with=FALSE]
X<-t(Xalm)
#colnames(X)<-paste0(t,(1:10))  #fix it

#Output csv
setwd("~/Academic/Universities/UCLA/Fall_Term/Applied Finance Project/Before_Mid")
write.csv(X,file="X.csv")
