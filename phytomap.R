###############################################################################
###############################################################################
#Code for producing the map of the phytoma article (2019)
###############################################################################
###############################################################################

#loading the packages necessary for the analysis
library(rgdal)
library(rgeos)
library(plotrix)
library(mapplots)
library(RColorBrewer)


###############################################################################
#loading the data
###############################################################################

#load geographical data
load("data/departe.RData")

#load the resistance results
ven_moni<-read.table("data/venturia_monito.txt",header=TRUE,sep="\t",
                     colClasses=c("factor","character","character",
                                  "factor","factor","factor","factor",
                                  "factor","factor","factor","factor"))

#extract the department coordinates
ind_list<-departe$INSEE_DEP
coorddep<-data.frame("longitude"=departe@polygons[1][[1]]@labpt[1],
                     "latitude"=departe@polygons[1][[1]]@labpt[2])
for (i in 2:length(ind_list)){
  coorddep<-rbind(coorddep, 
                  cbind("longitude"=departe@polygons[i][[1]]@labpt[1],
                        "latitude"=departe@polygons[i][[1]]@labpt[2]))
}
coorddep<-cbind("dep_ID"=ind_list,coorddep)


###############################################################################
#building the subfiles
###############################################################################

QoIdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                        ven_moni$QoI,exclude="")),
               "Resistant"=table(ven_moni$dptmt,
                                 ven_moni$QoI,exclude="")[,1],
               "Sensible"=table(ven_moni$dptmt,
                                ven_moni$QoI,exclude="")[,2],
               "Total"=rowSums(table(ven_moni$dptmt,
                                     ven_moni$QoI,exclude="")))

APdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                       ven_moni$AP,exclude="")),
              "Resistant"=table(ven_moni$dptmt,
                                ven_moni$AP,exclude="")[,1],
              "Sensible"=table(ven_moni$dptmt,
                               ven_moni$AP,exclude="")[,2],
              "Total"=rowSums(table(ven_moni$dptmt,
                                    ven_moni$AP,exclude="")))

CAPdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                        ven_moni$captane,exclude="")),
               "Resistant"=table(ven_moni$dptmt,
                                 ven_moni$captane,exclude="")[,1],
               "Sensible"=table(ven_moni$dptmt,
                                ven_moni$captane,exclude="")[,2],
               "Total"=rowSums(table(ven_moni$dptmt,
                                     ven_moni$captane,exclude="")))

DITdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                        ven_moni$dithianon,exclude="")),
               "Sensible"=table(ven_moni$dptmt,
                                ven_moni$dithianon,exclude="")[,1],
               "Total"=rowSums(table(ven_moni$dptmt,
                                     ven_moni$dithianon,exclude="")))

BOSdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                        ven_moni$boscalid,exclude="")),
               "Resistant"=table(ven_moni$dptmt,
                                 ven_moni$boscalid,exclude="")[,1],
               "Sensible"=table(ven_moni$dptmt,
                                ven_moni$boscalid,exclude="")[,2],
               "Total"=rowSums(table(ven_moni$dptmt,
                                     ven_moni$boscalid,exclude="")))

DODdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                        ven_moni$dodine,exclude="")),
               "Resistant"=table(ven_moni$dptmt,
                                 ven_moni$dodine,exclude="")[,2],
               "Suspect"=table(ven_moni$dptmt,
                               ven_moni$dodine,exclude="")[,1],
               "Sensible"=table(ven_moni$dptmt,
                                ven_moni$dodine,exclude="")[,3],
               "Total"=rowSums(table(ven_moni$dptmt,
                                     ven_moni$dodine,exclude="")))

TEBdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                        ven_moni$tebuconazole,exclude="")),
               "Resistant"=table(ven_moni$dptmt,
                                 ven_moni$tebuconazole,exclude="")[,1],
               "Sensible"=table(ven_moni$dptmt,
                                ven_moni$tebuconazole,exclude="")[,2],
               "Total"=rowSums(table(ven_moni$dptmt,
                                     ven_moni$tebuconazole,exclude="")))



###############################################################################
#producing the figure
###############################################################################

op<-par(mfrow=c(2,4))
par(mar=c(0,0,0,0))

#SDHI
data2map<-merge(BOSdata,coorddep,by="dep_ID")
data2map<-data2map[order(as.numeric(as.character(data2map$Total)),
                         decreasing=TRUE),]
colovec<-c(brewer.pal(9,"Oranges")[5],brewer.pal(9,"Blues")[6])
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec,lty=1,border="grey60",lwd=0.1,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*16000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,col="black",font=2,
     labels=as.character(data2map$Total),cex=1.5)
text(x=310000,y=7050000,"boscalide",font=2,cex=3,xpd=NA)


#QoI
data2map<-merge(QoIdata,coorddep,by="dep_ID")
data2map<-data2map[order(as.numeric(as.character(data2map$Total)),
                         decreasing=TRUE),]
colovec<-c(brewer.pal(9,"Reds")[6],brewer.pal(9,"Blues")[6])
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec,lty=1,border="grey60",lwd=0.1,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*13000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,col="black",font=2,
     labels=as.character(data2map$Total),cex=1.5)
text(x=310000,y=7050000,"krésoxim-méthyl",font=2,cex=3,xpd=NA)


#IDM
data2map<-merge(TEBdata,coorddep,by="dep_ID")
data2map<-data2map[order(as.numeric(as.character(data2map$Total)),
                         decreasing=TRUE),]
colovec<-c(brewer.pal(9,"Reds")[6],brewer.pal(9,"Blues")[6])
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec,lty=1,border="grey60",lwd=0.1,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*16000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,col="black",font=2,
     labels=as.character(data2map$Total),cex=1.5)
text(x=310000,y=7050000,"tébuconazole",font=2,cex=3,xpd=NA)


#Anilinopyrimidines
data2map<-merge(APdata,coorddep,by="dep_ID")
data2map<-data2map[order(as.numeric(as.character(data2map$Total)),
                         decreasing=TRUE),]
colovec<-c(brewer.pal(9,"Reds")[6],brewer.pal(9,"Blues")[6])
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec,lty=1,border="grey60",lwd=0.1,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*16000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,col="black",font=2,
     labels=as.character(data2map$Total),cex=1.5)
text(x=310000,y=7050000,"cyprodinil",font=2,cex=3,xpd=NA)


#Phtalimides
data2map<-merge(CAPdata,coorddep,by="dep_ID")
data2map<-data2map[order(as.numeric(as.character(data2map$Total)),
                         decreasing=TRUE),]
colovec<-c(brewer.pal(9,"Oranges")[6],brewer.pal(9,"Blues")[6])
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec,lty=1,border="grey60",lwd=0.1,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*16000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,col="black",font=2,
     labels=as.character(data2map$Total),cex=1.5)
text(x=310000,y=7050000,"captane",font=2,cex=3,xpd=NA)


#Guanidines
data2map<-merge(DODdata,coorddep,by="dep_ID")
data2map<-data2map[order(as.numeric(as.character(data2map$Total)),
                         decreasing=TRUE),]
colovec<-c(brewer.pal(9,"Oranges")[5],brewer.pal(9,"Reds")[6],
           brewer.pal(9,"Blues")[6])
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Suspect))),
                 (as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec,lty=1,border="grey60",lwd=0.1,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*16000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,col="black",font=2,
     labels=as.character(data2map$Total),cex=1.5)
text(x=310000,y=7050000,"dodine",font=2,cex=3,xpd=NA)


#Quinones
data2map<-merge(DITdata,coorddep,by="dep_ID")
data2map<-data2map[order(as.numeric(as.character(data2map$Total)),
                         decreasing=TRUE),]
colovec<-c(brewer.pal(9,"Reds")[6],brewer.pal(9,"Blues")[6])
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Sensible))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec[2],lty=1,border="grey60",lwd=0.1,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*16000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,col="black",font=2,
     labels=as.character(data2map$Total),cex=1.5)
text(x=310000,y=7050000,"dithianon",font=2,cex=3,xpd=NA)

par(op)


#export pdf 15 x 7 inches


###############################################################################
#END
###############################################################################