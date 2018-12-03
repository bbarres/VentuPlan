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


###############################################################################
#loading the data
###############################################################################

#load geographical data
load("data/departe.RData")

#load the resistance results
ven_moni<-read.table("data/venturia_monito.txt",header=TRUE,sep="\t",
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
#QoI figures
###############################################################################

#producing the map
QoIdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                        ven_moni$QoI,exclude="")),
               "Resistant"=table(ven_moni$dptmt,
                                 ven_moni$QoI,exclude="")[,1],
               "Sensible"=table(ven_moni$dptmt,
                                ven_moni$QoI,exclude="")[,2],
               "Total"=rowSums(table(ven_moni$dptmt,
                                     ven_moni$QoI,exclude="")))

data2map<-merge(QoIdata,coorddep,by="dep_ID")

op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=c("red","blue"),
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*8000),
         labels=NA)
par(op)

#producing the barplot
datXyear<-t(prop.table(table(ven_moni$year,ven_moni$QoI,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(ven_moni$year,ven_moni$QoI,exclude=""))
temp<-barplot(datXyear,col=c("red","blue"),las=1,font=2,main="QoI",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[6],103,paste("n=",totalyear[6],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[7],103,paste("n=",totalyear[7],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[8],103,paste("n=",totalyear[8],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[9],103,paste("n=",totalyear[9],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[10],103,paste("n=",totalyear[10],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[11],103,paste("n=",totalyear[11],sep=""),font=3,cex=1,xpd=TRUE)


###############################################################################
#AP figures
###############################################################################

#producing the map
APdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                       ven_moni$AP,exclude="")),
               "Resistant"=table(ven_moni$dptmt,
                                 ven_moni$AP,exclude="")[,1],
               "Sensible"=table(ven_moni$dptmt,
                                ven_moni$AP,exclude="")[,2],
               "Total"=rowSums(table(ven_moni$dptmt,
                                     ven_moni$AP,exclude="")))

data2map<-merge(APdata,coorddep,by="dep_ID")

op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=c("red","blue"),
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*8000),
         labels=NA)
par(op)

#producing the barplot
datXyear<-t(prop.table(table(ven_moni$year,ven_moni$AP,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(ven_moni$year,ven_moni$AP,exclude=""))
temp<-barplot(datXyear,col=c("red","blue"),las=1,font=2,main="AP",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[6],103,paste("n=",totalyear[6],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[7],103,paste("n=",totalyear[7],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[8],103,paste("n=",totalyear[8],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[9],103,paste("n=",totalyear[9],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[10],103,paste("n=",totalyear[10],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[11],103,paste("n=",totalyear[11],sep=""),font=3,cex=1,xpd=TRUE)


###############################################################################
#captane figures
###############################################################################

#producing the map
CAPdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                        ven_moni$captane,exclude="")),
              "Resistant"=table(ven_moni$dptmt,
                                ven_moni$captane,exclude="")[,1],
              "Sensible"=table(ven_moni$dptmt,
                               ven_moni$captane,exclude="")[,2],
              "Total"=rowSums(table(ven_moni$dptmt,
                                    ven_moni$captane,exclude="")))

data2map<-merge(CAPdata,coorddep,by="dep_ID")

op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                  (as.numeric(as.character(data2map$Sensible)))),
         col=c("red","blue"),
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*8000),
         labels=NA)
par(op)

#producing the barplot
datXyear<-t(prop.table(table(ven_moni$year,ven_moni$captane,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(ven_moni$year,ven_moni$captane,exclude=""))
temp<-barplot(datXyear,col=c("red","blue"),las=1,font=2,main="captane",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[6],103,paste("n=",totalyear[6],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[7],103,paste("n=",totalyear[7],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[8],103,paste("n=",totalyear[8],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[9],103,paste("n=",totalyear[9],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[10],103,paste("n=",totalyear[10],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[11],103,paste("n=",totalyear[11],sep=""),font=3,cex=1,xpd=TRUE)


###############################################################################
#dithianon figures
###############################################################################

#producing the map
DITdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                        ven_moni$dithianon,exclude="")),
               "Sensible"=table(ven_moni$dptmt,
                                ven_moni$dithianon,exclude="")[,1],
               "Total"=rowSums(table(ven_moni$dptmt,
                                     ven_moni$dithianon,exclude="")))

data2map<-merge(DITdata,coorddep,by="dep_ID")

op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Sensible))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=c("blue"),
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*8000),
         labels=NA)
par(op)

#producing the barplot
datXyear<-t(prop.table(table(ven_moni$year,ven_moni$dithianon,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(ven_moni$year,ven_moni$dithianon,exclude=""))
temp<-barplot(datXyear,col=c("blue"),las=1,font=2,main="dithianon",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[6],103,paste("n=",totalyear[6],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[7],103,paste("n=",totalyear[7],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[8],103,paste("n=",totalyear[8],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[9],103,paste("n=",totalyear[9],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[10],103,paste("n=",totalyear[10],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[11],103,paste("n=",totalyear[11],sep=""),font=3,cex=1,xpd=TRUE)


###############################################################################
#boscalid figures
###############################################################################

#producing the map
BOSdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                        ven_moni$boscalid,exclude="")),
               "Resistant"=table(ven_moni$dptmt,
                                 ven_moni$boscalid,exclude="")[,1],
               "Sensible"=table(ven_moni$dptmt,
                                ven_moni$boscalid,exclude="")[,2],
               "Total"=rowSums(table(ven_moni$dptmt,
                                     ven_moni$boscalid,exclude="")))

data2map<-merge(BOSdata,coorddep,by="dep_ID")

op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=c("red","blue"),
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*8000),
         labels=NA)
par(op)

#producing the barplot
datXyear<-t(prop.table(table(ven_moni$year,ven_moni$boscalid,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(ven_moni$year,ven_moni$boscalid,exclude=""))
temp<-barplot(datXyear,col=c("red","blue"),las=1,font=2,main="boscalid",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[6],103,paste("n=",totalyear[6],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[7],103,paste("n=",totalyear[7],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[8],103,paste("n=",totalyear[8],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[9],103,paste("n=",totalyear[9],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[10],103,paste("n=",totalyear[10],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[11],103,paste("n=",totalyear[11],sep=""),font=3,cex=1,xpd=TRUE)


###############################################################################
#dodine figures
###############################################################################

#producing the map
DODdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                        ven_moni$dodine,exclude="")),
               "Resistant"=table(ven_moni$dptmt,
                                 ven_moni$dodine,exclude="")[,1],
               "Sensible"=table(ven_moni$dptmt,
                                ven_moni$dodine,exclude="")[,2],
               "Total"=rowSums(table(ven_moni$dptmt,
                                     ven_moni$dodine,exclude="")))

data2map<-merge(DODdata,coorddep,by="dep_ID")

op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=c("red","blue"),
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*8000),
         labels=NA)
par(op)

#producing the barplot
datXyear<-t(prop.table(table(ven_moni$year,ven_moni$dodine,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(ven_moni$year,ven_moni$dodine,exclude=""))
temp<-barplot(datXyear,col=c("red","blue"),las=1,font=2,main="dodine",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[6],103,paste("n=",totalyear[6],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[7],103,paste("n=",totalyear[7],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[8],103,paste("n=",totalyear[8],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[9],103,paste("n=",totalyear[9],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[10],103,paste("n=",totalyear[10],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[11],103,paste("n=",totalyear[11],sep=""),font=3,cex=1,xpd=TRUE)


###############################################################################
#tebuconazole figures
###############################################################################

#producing the map
TEBdata<-cbind("dep_ID"=row.names(table(ven_moni$dptmt,
                                        ven_moni$tebuconazole,exclude="")),
               "Resistant"=table(ven_moni$dptmt,
                                 ven_moni$tebuconazole,exclude="")[,1],
               "Sensible"=table(ven_moni$dptmt,
                                ven_moni$tebuconazole,exclude="")[,2],
               "Total"=rowSums(table(ven_moni$dptmt,
                                     ven_moni$tebuconazole,exclude="")))

data2map<-merge(TEBdata,coorddep,by="dep_ID")

op<-par(mar=c(0,0,0,0))
plot(departe)
draw.pie(x=data2map$longitude,y=data2map$latitude,
         z=cbind((as.numeric(as.character(data2map$Resistant))),
                 (as.numeric(as.character(data2map$Sensible)))),
         col=c("red","blue"),
         radius=(sqrt(as.numeric(as.character(data2map$Total)))*8000),
         labels=NA)
par(op)

#producing the barplot
datXyear<-t(prop.table(table(ven_moni$year,ven_moni$tebuconazole,exclude=""),
                       margin=1)*100)
totalyear<-rowSums(table(ven_moni$year,ven_moni$tebuconazole,exclude=""))
temp<-barplot(datXyear,col=c("red","blue"),las=1,font=2,main="tebuconazole",
              cex.main=2)
text(temp[1],103,paste("n=",totalyear[1],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[2],103,paste("n=",totalyear[2],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[3],103,paste("n=",totalyear[3],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[4],103,paste("n=",totalyear[4],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[5],103,paste("n=",totalyear[5],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[6],103,paste("n=",totalyear[6],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[7],103,paste("n=",totalyear[7],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[8],103,paste("n=",totalyear[8],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[9],103,paste("n=",totalyear[9],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[10],103,paste("n=",totalyear[10],sep=""),font=3,cex=1,xpd=TRUE)
text(temp[11],103,paste("n=",totalyear[11],sep=""),font=3,cex=1,xpd=TRUE)


###############################################################################
#END
###############################################################################
