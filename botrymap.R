###############################################################################
###############################################################################
#Basic code for plotting map of France
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
botfraise<-read.table("data/botryfraise.txt",header=TRUE,sep="\t",
                     colClasses=c("factor","character","character"))

bottomate<-read.table("data/botrytomate.txt",header=TRUE,sep="\t",
                      colClasses=c("factor","character","character"))

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
#Fraise Iprodione figure
###############################################################################

#producing the map
ImiR1data<-cbind("dep_ID"=row.names(table(botfraise$dptmt,
                                          botfraise$ImiR1,exclude="")),
                 "Resistant"=table(botfraise$dptmt,
                                   botfraise$ImiR1,exclude="")[,1],
                 "Sensible"=table(botfraise$dptmt,
                                  botfraise$ImiR1,exclude="")[,2],
                 "Total"=rowSums(table(botfraise$dptmt,
                                       botfraise$ImiR1,exclude="")))

data2map<-merge(ImiR1data,coorddep,by="dep_ID")

colovec<-c(brewer.pal(9,"Reds")[7],brewer.pal(9,"Blues")[8])
png(file=paste("output/","ImiR1_fraise_spa",".png",sep=""),
    width=4,height=4,units="in",res=300)
op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec,lty=0,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*15000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,
     labels=as.character(data2map$Total),cex=0.7)
par(op)
dev.off()

#producing the barplot
datXyear<-t(prop.table(table(botfraise$year,botfraise$ImiR1,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(botfraise$year,botfraise$ImiR1,exclude=""))
png(file=paste("output/","ImiR1_fraise_temp",".png",sep=""),
    width=8,height=7,units="in",res=300)
temp<-barplot(datXyear,col=colovec,las=1,font=2,main="Dicarboximide",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
dev.off()


###############################################################################
#Fraise QoI figure
###############################################################################

#producing the map
QoI.Rdata<-cbind("dep_ID"=row.names(table(botfraise$dptmt,
                                          botfraise$QoI.R,exclude="")),
                 "Resistant"=table(botfraise$dptmt,
                                   botfraise$QoI.R,exclude="")[,1],
                 "Sensible"=table(botfraise$dptmt,
                                  botfraise$QoI.R,exclude="")[,2],
                 "Total"=rowSums(table(botfraise$dptmt,
                                       botfraise$QoI.R,exclude="")))

data2map<-merge(QoI.Rdata,coorddep,by="dep_ID")

colovec<-c(brewer.pal(9,"Reds")[7],brewer.pal(9,"Blues")[8])
png(file=paste("output/","QoI.R_fraise_spa",".png",sep=""),
    width=4,height=4,units="in",res=300)
op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec,lty=0,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*15000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,
     labels=as.character(data2map$Total),cex=0.7)
par(op)
dev.off()

#producing the barplot
datXyear<-t(prop.table(table(botfraise$year,botfraise$QoI.R,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(botfraise$year,botfraise$QoI.R,exclude=""))
png(file=paste("output/","QoI.R_fraise_temp",".png",sep=""),
    width=8,height=7,units="in",res=300)
temp<-barplot(datXyear,col=colovec,las=1,font=2,main="QoI",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
dev.off()


###############################################################################
#Fraise SDHI figure
###############################################################################

#producing the map
CarRdata<-cbind("dep_ID"=row.names(table(botfraise$dptmt,
                                          botfraise$CarR,exclude="")),
                 "Resistant"=table(botfraise$dptmt,
                                   botfraise$CarR,exclude="")[,1],
                 "Sensible"=table(botfraise$dptmt,
                                  botfraise$CarR,exclude="")[,2],
                 "Total"=rowSums(table(botfraise$dptmt,
                                       botfraise$CarR,exclude="")))

data2map<-merge(CarRdata,coorddep,by="dep_ID")

colovec<-c(brewer.pal(9,"Reds")[7],brewer.pal(9,"Blues")[8])
png(file=paste("output/","CarR_fraise_spa",".png",sep=""),
    width=4,height=4,units="in",res=300)
op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec,lty=0,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*15000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,
     labels=as.character(data2map$Total),cex=0.7)
par(op)
dev.off()

#producing the barplot
datXyear<-t(prop.table(table(botfraise$year,botfraise$CarR,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(botfraise$year,botfraise$CarR,exclude=""))
png(file=paste("output/","CarR_fraise_temp",".png",sep=""),
    width=8,height=7,units="in",res=300)
temp<-barplot(datXyear,col=colovec,las=1,font=2,main="SDHI",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
dev.off()




###############################################################################
#Tomate Iprodione figure
###############################################################################

#producing the map
ImiR1data<-cbind("dep_ID"=row.names(table(bottomate$dptmt,
                                          bottomate$ImiR1,exclude="")),
                 "Resistant"=table(bottomate$dptmt,
                                   bottomate$ImiR1,exclude="")[,1],
                 "Sensible"=table(bottomate$dptmt,
                                  bottomate$ImiR1,exclude="")[,2],
                 "Total"=rowSums(table(bottomate$dptmt,
                                       bottomate$ImiR1,exclude="")))

data2map<-merge(ImiR1data,coorddep,by="dep_ID")

colovec<-c(brewer.pal(9,"Reds")[7],brewer.pal(9,"Blues")[8])
png(file=paste("output/","ImiR1_tomate_spa",".png",sep=""),
    width=4,height=4,units="in",res=300)
op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec,lty=0,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*15000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,
     labels=as.character(data2map$Total),cex=0.7)
par(op)
dev.off()

#producing the barplot
datXyear<-t(prop.table(table(bottomate$year,bottomate$ImiR1,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(bottomate$year,bottomate$ImiR1,exclude=""))
png(file=paste("output/","ImiR1_tomate_temp",".png",sep=""),
    width=8,height=7,units="in",res=300)
temp<-barplot(datXyear,col=colovec,las=1,font=2,main="Dicarboximide",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
dev.off()


###############################################################################
#Tomate Anilino figure
###############################################################################

#producing the map
AniRdata<-cbind("dep_ID"=row.names(table(bottomate$dptmt,
                                          bottomate$AniR1,exclude="")),
                 "Resistant"=table(bottomate$dptmt,
                                   bottomate$AniR1,exclude="")[,1],
                 "Sensible"=table(bottomate$dptmt,
                                  bottomate$AniR1,exclude="")[,2],
                 "Total"=rowSums(table(bottomate$dptmt,
                                       bottomate$AniR1,exclude="")))

data2map<-merge(AniRdata,coorddep,by="dep_ID")

colovec<-c(brewer.pal(9,"Reds")[7],brewer.pal(9,"Blues")[8])
png(file=paste("output/","AniR1_tomate_spa",".png",sep=""),
    width=4,height=4,units="in",res=300)
op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec,lty=0,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*15000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,
     labels=as.character(data2map$Total),cex=0.7)
par(op)
dev.off()

#producing the barplot
datXyear<-t(prop.table(table(bottomate$year,bottomate$AniR1,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(bottomate$year,bottomate$AniR1,exclude=""))
png(file=paste("output/","AniR1_tomate_temp",".png",sep=""),
    width=8,height=7,units="in",res=300)
temp<-barplot(datXyear,col=colovec,las=1,font=2,main="Anilino-pyrimidine",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
dev.off()


###############################################################################
#Tomate hydroxyanilide figure
###############################################################################

#producing the map
HydR3data<-cbind("dep_ID"=row.names(table(bottomate$dptmt,
                                         bottomate$HydR3,exclude="")),
                "Resistant"=table(bottomate$dptmt,
                                  bottomate$HydR3,exclude="")[,1],
                "Sensible"=table(bottomate$dptmt,
                                 bottomate$HydR3,exclude="")[,2],
                "Total"=rowSums(table(bottomate$dptmt,
                                      bottomate$HydR3,exclude="")))

data2map<-merge(HydR3data,coorddep,by="dep_ID")

colovec<-c(brewer.pal(9,"Reds")[7],brewer.pal(9,"Blues")[8])
png(file=paste("output/","HydR3_tomate_spa",".png",sep=""),
    width=4,height=4,units="in",res=300)
op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec,lty=0,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*15000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,
     labels=as.character(data2map$Total),cex=0.7)
par(op)
dev.off()

#producing the barplot
datXyear<-t(prop.table(table(bottomate$year,bottomate$HydR3,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(bottomate$year,bottomate$HydR3,exclude=""))
png(file=paste("output/","HydR3_tomate_temp",".png",sep=""),
    width=8,height=7,units="in",res=300)
temp<-barplot(datXyear,col=colovec,las=1,font=2,main="Hydroxyanilide",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
dev.off()


###############################################################################
#Tomate SDHI figure
###############################################################################

#producing the map
CarRdata<-cbind("dep_ID"=row.names(table(bottomate$dptmt,
                                          bottomate$CarR,exclude="")),
                 "Resistant"=table(bottomate$dptmt,
                                   bottomate$CarR,exclude="")[,1],
                 "Sensible"=table(bottomate$dptmt,
                                  bottomate$CarR,exclude="")[,2],
                 "Total"=rowSums(table(bottomate$dptmt,
                                       bottomate$CarR,exclude="")))

data2map<-merge(CarRdata,coorddep,by="dep_ID")

colovec<-c(brewer.pal(9,"Reds")[7],brewer.pal(9,"Blues")[8])
png(file=paste("output/","CarR_tomate_spa",".png",sep=""),
    width=4,height=4,units="in",res=300)
op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=colovec,lty=0,
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*15000),
         labels=NA)
text(x=data2map$longitude,y=data2map$latitude,
     labels=as.character(data2map$Total),cex=0.7)
par(op)
dev.off()

#producing the barplot
datXyear<-t(prop.table(table(bottomate$year,bottomate$CarR,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(bottomate$year,bottomate$CarR,exclude=""))
png(file=paste("output/","CarR_tomate_temp",".png",sep=""),
    width=8,height=7,units="in",res=300)
temp<-barplot(datXyear,col=colovec,las=1,font=2,main="SDHI",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
dev.off()
