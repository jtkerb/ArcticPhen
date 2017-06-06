#rm(list=ls())


#setwd("/Users/jtkerb/Google Drive/PhenoCamExtracts")
setwd("/Users/jtkerb/Dropbox/PhenoCamPaper")
MOD<-read.table('CamNet_MOD250_NDVI_DOY_EVI_QA_2000-2016.txt', header=TRUE)

#misc. packages
library(rgdal)
library(raster)
library(gimms)
library(rasterVis)
library(stringr)
library(phenex)
library(tidyr)
library(dplyr)

summary(MOD)

?read.csv

par(mfrow=c(1,1))
plot(MOD$NDVI~ MOD$DayOfYear, cex=1.2)
MODHQ<-filter(MOD, SummaryQA==0)
points(MODHQ$NDVI~MODHQ$DayOfYear, pch=16, col=2)
MODHQ1<-filter(MOD, SummaryQA==1)
points(MODHQ1$NDVI~MODHQ1$DayOfYear, pch=16, col=3)
MODHQ2<-filter(MOD, SummaryQA==2)
points(MODHQ2$NDVI~MODHQ2$DayOfYear, pch=16, col=4)

#head(MOD)
#max(MOD$longitude)

#camloc<-read.csv("CameraLocations.csv")
#head(camloc)
#semi_join(MOD, camloc, by="longitude")

#---------------------------------------------------------

#summary(MOD$year)

#need to subset MOD to site in a loop
#par(mfrow=c(7,6))
#par(mar=c(2,2,1,1))
#MOD.all<-NULL
#MOD.greenup.date.05<-NULL
#MOD.greenup.date.50<-NULL
#MOD.greenup.date.95<-NULL
#max.NDVI.date<-NULL
#for(i in 2013:2015){
#  MODyr<-MOD[MOD$year==i,]
#  if(leapYears(i)==TRUE){
#    MOD.for.model<-rep('NA', 366)
#    MOD.for.model[MODyr$DayOfYear+1]<-MODyr$NDVI
#    plot(MODyr$NDVI~MODyr$DayOfYear+1,pch=16, ylim=c(0,0.1))
#    abline(v=150, lty=2)
#  } else {
#    MOD.for.model<-rep('NA', 365)
#    MOD.for.model[MODyr$DayOfYear]<-MODyr$NDVI
#    plot(MODyr$NDVI~MODyr$DayOfYear,pch=16, ylim=c(0,0.1))
#    abline(v=150, lty=2)
# }
# ndvi.list<-modelNDVI(ndvi.values=MOD.for.model, year.int=i, correction='none',method="DLogistic", MARGIN=2)
#  ndvi<-ndvi.list[[1]]
#  model<-modelledValues(ndvi)
#  points(model, pch=16, col=2, cex=0.5)
#  greenup05<-phenoPhase(ndvi.list[[1]],phase="greenup",method="local", threshold=0.05)
#  abline(v=greenup05, col=2)
#  MOD.greenup.date.05[i-2012]<-greenup05
#  text(50,0.4, i)

#  greenup50<-phenoPhase(ndvi.list[[1]],phase="greenup",method="local", threshold=0.50)
#  abline(v=greenup50, col=3)
#  MOD.greenup.date.50[i-2012]<-greenup50

#  greenup95<-phenoPhase(ndvi.list[[1]],phase="greenup",method="local", threshold=0.95)
#  abline(v=greenup95, col=4)
#  MOD.greenup.date.95[i-2012]<-greenup95

#}

### ------------------------------------------
summary(MOD)
#MOD<-MOD[MOD$uid==1,]

#MOD QA meaning

MOD<-MOD[MOD$SummaryQA<=2,]
MOD<-na.omit(MOD)
MOD$NDVI[MOD$NDVI<0]<-0



par(mfrow=c(1,1))
plot(MOD$NDVI~MOD$DayOfYear, pch=16)

#need to subset MOD to site in a loop
par(mfrow=c(4,5))
par(mar=c(2,2,1,1))
MOD.all<-NULL
MOD.greenup.date.05<-NULL
MOD.greenup.date.50<-NULL
MOD.greenup.date.95<-NULL
MOD.greenup.dates.05 <- as.data.frame(setNames(replicate(length(unique(MOD$uid)),numeric(0), simplify = F), unique(MOD$uid)), header=TRUE)
MOD.greenup.dates.50 <- as.data.frame(setNames(replicate(length(unique(MOD$uid)),numeric(0), simplify = F), unique(MOD$uid)))
MOD.greenup.dates.95 <- as.data.frame(setNames(replicate(length(unique(MOD$uid)),numeric(0), simplify = F), unique(MOD$uid)))
#max.NDVI.dates<- as.data.frame(setNames(replicate(length(unique(MOD$uid)),numeric(0), simplify = F), unique(MOD$uid)))
#NULL

for(i in 2000:2016){
  MODyr<-MOD[MOD$year==i,]
  MODyr<-na.omit(MODyr)
  plot(NULL, ylim=c(0,0.8), xlim=c(0,366))
  text(50,0.4, i)
  for(j in 1:length(unique(MOD$uid))){
    MODyr.site=MODyr[MODyr$uid==j,]
    if(leapYears(i)==TRUE){
      MOD.for.model<-rep('NA', 366)
      MOD.for.model[MODyr.site$DayOfYear+1]<-MODyr.site$NDVI
      #points(MODyr.site$NDVI~MODyr.site$DayOfYear+1,pch=16, cex=0.5)
      #abline(v=150, lty=2)
    } else {
      MOD.for.model<-rep('NA', 365)
      MOD.for.model[MODyr.site$DayOfYear]<-MODyr.site$NDVI
      #points(MODyr.site$NDVI~MODyr.site$DayOfYear,pch=16, cex=0.5)
      #abline(v=150, lty=2)
    }
    ndvi.list<-modelNDVI(ndvi.values=MOD.for.model, year.int=i, correction='none',method="DLogistic", MARGIN=2)
    ndvi<-ndvi.list[[1]]
    model<-modelledValues(ndvi)
    #points(model, pch=16, col=2, cex=0.5)
    greenup05<-phenoPhase(ndvi.list[[1]],phase="greenup",method="local", threshold=0.05)
    #abline(v=greenup05, col=2)
    
    MOD.greenup.date.05[j]<-greenup05
    summary(MOD)
    #text(50,0.4, i)
    
    greenup50<-phenoPhase(ndvi.list[[1]],phase="greenup",method="local", threshold=0.50)
    #abline(v=greenup50, col=3)
    MOD.greenup.date.50[j]<-greenup50
    
    greenup95<-phenoPhase(ndvi.list[[1]],phase="greenup",method="local", threshold=0.95)
    #abline(v=greenup95, col=4)
    MOD.greenup.date.95[j]<-greenup95
    
    
    
  }  
  MOD.greenup.dates.05 = rbind(MOD.greenup.dates.05,MOD.greenup.date.05)
  MOD.greenup.dates.50 = rbind(MOD.greenup.dates.50,MOD.greenup.date.50) 
  MOD.greenup.dates.95 = rbind(MOD.greenup.dates.95,MOD.greenup.date.95) 
}

MOD05<-MOD.greenup.dates.05
colnames(MOD05)<-paste("uid",unique(MOD$uid),sep="")
yrs<-(2000:2016)
MOD05<-cbind(MOD05,yrs)
MOD.05<-gather(MOD05, yrs, d05, -yrs)

MOD50<-MOD.greenup.dates.50
colnames(MOD50)<-paste("uid",unique(MOD$uid),sep="")
yrs<-(2000:2016)
MOD50<-cbind(MOD50,yrs)
MOD.50<-gather(MOD50, yrs, d50, -yrs)

MOD95<-MOD.greenup.dates.95
colnames(MOD95)<-paste("uid",unique(MOD$uid),sep="")
yrs<-(2000:2016)
MOD95<-cbind(MOD95,yrs)
MOD.95<-gather(MOD95, yrs, d95, -yrs)



par(mfrow=c(1,1))
points(MOD.05$yrs,MOD.05$d05,ylim=c(40,250), pch=16)
points(MOD.50$yrs,MOD.50$d50, pch=16, col=2)
points(MOD.95$yrs,MOD.95$d95, pch=16, col=3)

yrs<-(2000:2016)

MOD.05avg<-tapply(MOD.05$d05, MOD.05$yrs, mean)
MOD.05std<-tapply(MOD.05$d05, MOD.05$yrs, sd)
points(MOD.05avg~yrs, ylim=c(40,250), col=2, cex=1.2)
points(MOD.05avg~yrs, ylim=c(40,280), col=2, cex=0.6)
arrows(yrs, MOD.05avg-MOD.05std, yrs, MOD.05avg+MOD.05std, length=0.05, angle=90, code=3)

MOD.50avg<-tapply(MOD.50$d50, MOD.50$yrs, mean)
MOD.50std<-tapply(MOD.50$d50, MOD.50$yrs, sd)
points(MOD.50avg~yrs, ylim=c(40,280), col=3, cex=1.2)
points(MOD.50avg~yrs, ylim=c(40,280), col=3, cex=0.6)
arrows(yrs, MOD.50avg-MOD.50std, yrs, MOD.50avg+MOD.50std, length=0.05, angle=90, code=3)

MOD.95avg<-tapply(MOD.95$d95, MOD.95$yrs, mean)
MOD.95std<-tapply(MOD.95$d95, MOD.95$yrs, sd)
points(MOD.95avg~yrs, ylim=c(40,280), col=4, cex=1.2)
points(MOD.95avg~yrs, ylim=c(40,280), col=4, cex=0.6)
arrows(yrs, MOD.95avg-MOD.95std, yrs, MOD.95avg+MOD.95std, length=0.05, angle=90, code=3)





summary(lm(MOD.05avg[-1]~yrs[-1]))
abline(lm(MOD.05avg[-1]~yrs[-1]))
abline(lm(MOD.05avg~yrs), col=2)


######## GIMMS STUFF

camsats<-read.table("/Users/jtkerb/Dropbox/PhenocamPaper/SatData/camsats.txt", sep="\t")
plot(camsats$meanNDVI~camsats$doy,pch=16)


par(mfrow=c(7,6))
par(mar=c(2,2,1,1))
GIMMS.all<-NULL
GIMMS.greenup.date.05<-NULL
GIMMS.greenup.date.50<-NULL
GIMMS.greenup.date.95<-NULL
max.NDVI.date<-NULL
for(i in 1982:2013){
  GIMMSyr<-camsats[camsats$year==i,]
  if(leapYears(i)==TRUE){
    GIMMS.for.model<-rep('NA', 366)
    GIMMS.for.model[GIMMSyr$doy+1]<-GIMMSyr$meanNDVI
    plot(GIMMSyr$meanNDVI~GIMMSyr$doy+1,pch=16, ylim=c(0,0.8))
    abline(v=150, lty=2)
  } else {
    GIMMS.for.model<-rep('NA', 365)
    GIMMS.for.model[GIMMSyr$doy]<-GIMMSyr$meanNDVI
    plot(GIMMSyr$meanNDVI~GIMMSyr$doy,pch=16, ylim=c(0,0.8))
    abline(v=150, lty=2)
  }
  ndvi.list<-modelNDVI(ndvi.values=GIMMS.for.model, year.int=i, correction='none',method="DLogistic", MARGIN=2)
  ndvi<-ndvi.list[[1]]
  model<-modelledValues(ndvi)
  points(model, pch=16, col=2, cex=0.5)
  greenup05<-phenoPhase(ndvi.list[[1]],phase="greenup",method="local", threshold=0.05)
  abline(v=greenup05, col=2)
  GIMMS.greenup.date.05[i-1981]<-greenup05
  text(50,0.4, i)
  
  greenup50<-phenoPhase(ndvi.list[[1]],phase="greenup",method="local", threshold=0.50)
  abline(v=greenup50, col=3)
  GIMMS.greenup.date.50[i-1981]<-greenup50
  
  greenup95<-phenoPhase(ndvi.list[[1]],phase="greenup",method="local", threshold=0.95)
  abline(v=greenup95, col=4)
  GIMMS.greenup.date.95[i-1981]<-greenup95
  
}

GIMMS.years<-unique(camsats$year)

plot(GIMMS.years,GIMMS.greenup.date.05, pch=16, col=2, ylim=c(70,220))
summary(lm(GIMMS.greenup.date.05~GIMMS.years))
plot(unique(camsats$year),GIMMS.greenup.date.50, pch=16, col=3, ylim=c(70,220))
summary(lm(GIMMS.greenup.date.50~GIMMS.years))
abline(lm(GIMMS.greenup.date.50~GIMMS.years))
plot(unique(camsats$year),GIMMS.greenup.date.95, pch=16, col=4, ylim=c(70,220))
summary(lm(GIMMS.greenup.date.95~GIMMS.years))


plot(unique(camsats$year),GIMMS.greenup.date.05, pch=16, col=2, ylim=c(40,250), xlim=c(1982,2016))
points(unique(camsats$year),GIMMS.greenup.date.50, pch=16, col=3, ylim=c(70,220))
points(unique(camsats$year),GIMMS.greenup.date.95, pch=16, col=4, ylim=c(70,220))
abline(lm(GIMMS.greenup.date.50~GIMMS.years))








