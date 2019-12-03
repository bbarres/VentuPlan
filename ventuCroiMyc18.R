##############################################################################/
##############################################################################/
#Analysis of the mycelial growth of the monospores of the 2018 Plan de Surv
##############################################################################/
##############################################################################/

#loading the packages necessary for the analysis
library(drc)
library(plotrix)
library(gdata)

#loading the data (the dataset only comprise one SA: dodine)
dodmycgro<-read.table("data/ventuCroissMyc18.txt",header=TRUE,sep=";")


##############################################################################/
#Regression analysis####
##############################################################################/

#some individual never reach an inhibition of 50%, event for the highest 
#tested concentration. 
dod_rez<-as.character(dodmycgro[dodmycgro$dose==30 & dodmycgro$rslt_03>50,
                                "ech_id"])
REZdod<-data.frame("sample_ID"=dod_rez,"ED50"=30)
#we limit the dataset to the sample that reach somehow a IC of 50%
dod.dat<-dodmycgro[!(dodmycgro$ech_id %in% dod_rez),]
dod.dat<-drop.levels(dod.dat)
for (i in 1: dim(table(dod.dat$ech_id))[1]) {
  temp.m1<-drm(rslt_03~dose,
               data=dod.dat[dod.dat$ech_id==names(table(dod.dat$ech_id))[i],],
               fct=LL.4())
  plot(temp.m1,main=names(table(dod.dat$ech_id))[i])
  temp<-ED(temp.m1,50,type="absolute")
  tempx<-data.frame("sample_ID"=names(table(dod.dat$ech_id))[i],
                    "ED50"=temp[1])
  REZdod<-rbind(REZdod,tempx)
}

#computing the mean ED50 for the reference strains
refval<-mean(REZdod[startsWith(as.character(REZdod$sample_ID),
                               pattern="13",trim=TRUE),"ED50"])

REZdod$ED50[REZdod$ED50>30]<-30
plot(REZdod$ED50[order(REZdod$ED50)]/refval,main="dodine",xlab="Souches ID",
     ylab="FR",las=1)
abline(refval/refval,0,col="green4",lwd=2)
abline((10*refval)/refval,0,col="red",lwd=2)
REZdod$FR<-REZdod$ED50/refval
#export to pdf 10 x 6 inches
write.table(REZdod,file="output/REZdodcroimyc18.txt",quote=FALSE,sep="\t",
            row.names=FALSE)

hist((REZdod$ED50[order(REZdod$ED50)])/refval,main="dodine",xlab="FR Classes",
     breaks=c(0,10,20,30,40,50,60,70,80),
     las=1,col=heat.colors(8)[8:1],ylim=c(0,30))
abline(v=10,col="red",lwd=3)
#export to pdf 4.5 x 9 inches


##############################################################################/
#END
##############################################################################/