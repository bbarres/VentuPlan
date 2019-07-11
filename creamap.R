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

