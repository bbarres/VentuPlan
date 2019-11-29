###############################################################################
###############################################################################
#Code for producing a map of
###############################################################################
###############################################################################

#loading the packages necessary for the analysis
library(rgdal)
library(rgeos)
library(plotrix)
library(mapplots)
library(RColorBrewer)


###############################################################################
#loading and preparing the data
###############################################################################


#load geographical data
load("data/departe.RData")

#load the resistance results
databrute <- read.table(
  "data/ambroisie_result2019.txt",
  header = TRUE,
  sep = "\t",
  colClasses = c("character", "character", "character",
                 "character", "factor")
)

#counting the number of sampling according to their status
datacount <- cbind(
  "dep_ID" = row.names(table(
    databrute$dptmt,
    databrute$iALS_status, exclude =
      ""
  )),
  "Resistant" = table(databrute$dptmt,
                      databrute$iALS_status, exclude = "")[, 1],
  "Sensible" = table(databrute$dptmt,
                     databrute$iALS_status, exclude = "")[, 2],
  "Total" = rowSums(table(
    databrute$dptmt,
    databrute$iALS_status, exclude = ""
  ))
)
#export the count data by departement
write.table(
  datacount,
  file = "output/datacount.txt",
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)

#extract the department coordinates
ind_list <- departe$INSEE_DEP
coorddep <- data.frame(
  "longitude" = departe@polygons[1][[1]]@labpt[1],
  "latitude" = departe@polygons[1][[1]]@labpt[2]
)
for (i in 2:length(ind_list)) {
  coorddep <- rbind(
    coorddep,
    cbind(
      "longitude" = departe@polygons[i][[1]]@labpt[1],
      "latitude" = departe@polygons[i][[1]]@labpt[2]
    )
  )
}
coorddep <- cbind("dep_ID" = ind_list, coorddep)


###############################################################################
#producing the map
###############################################################################

#combining the information of the geographic
data2map <- merge(datacount, coorddep, by = "dep_ID")
data2map <- data2map[order(as.numeric(as.character(data2map$Total)),
                           decreasing = TRUE), ]

#defining the colors of the pies
colovec <- c(brewer.pal(9, "Reds")[6], brewer.pal(9, "Blues")[7])

#actual plotting
op <- par(mar = c(0, 0, 0, 0))
plot(departe)
draw.pie(
  x = data2map$longitude,
  y = data2map$latitude,
  z = cbind((as.numeric(
    as.character(data2map$Resistant)
  )),
  (as.numeric(
    as.character(data2map$Sensible)
  ))),
  col = colovec,         #colors of the pie
  lty = 1,               #line type of the pie
  border = "grey60",     #color of the border of the pie
  lwd = 0.1,             #control the width of the border
  radius = 35000, #(sqrt(as.numeric(as.character(data2map$Total))) * 16000), #this number control the radius of the pies
  labels = NA,
  scale=FALSE # oui ou non le radius dépend des effectifs
)

#writing the number of samples for each departement
text(
  x = data2map$longitude,
  y = data2map$latitude,
  col = "black",
  font = 2,
  labels = as.character(data2map$Total),
  cex = 1.5              #size of the text
)
par(op)

#export the map to a pdf file 7 x 7 inches (for examples)


###############################################################################
#END
###############################################################################