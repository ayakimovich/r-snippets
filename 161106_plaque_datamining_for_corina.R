library(gdata) 
library(ggplot2)
library(reshape2)

read_file <- "/Users/ayakimovich/Dropbox (LMCB)/analysis_for_corina/deltaF11_33_Spots.csv"
data_table = read.table(read_file, sep=",", header=T)[-1]
#outTable <- data_table
#outTable$Distance <- 0
#outTable$Length <- 0
#outTable <- outTable[0,]
for(iTrackID in unique(data_table$ID)){
  selectedData <- subset(data_table, ID == iTrackID)
  track_length <- max(selectedData$FRAME)-min(selectedData$FRAME)
  track_length_vector <- rep(track_length, track_length+1)
  #print(track_length)
  eucledean_distance_vect <- c(0)
  for(iFrame in (min(selectedData$FRAME)+1):max(selectedData$FRAME)){
    coordinates1 <- as.vector(as.matrix(subset(selectedData, FRAME == iFrame-1, select=c(POSITION_X, POSITION_Y))))
    coordinates2 <- as.vector(as.matrix(subset(selectedData, FRAME == iFrame, select=c(POSITION_X, POSITION_Y))))
    #print(cbind(coordinates1,coordinates2))
    
    #compute eucleadian distances
    eucledean_distance <- dist(rbind(coordinates1,coordinates2))
    eucledean_distance_vect <- rbind(eucledean_distance_vect, eucledean_distance[1])
  }
  #print(eucledean_distance_vect)
  selectedData$Distance <- eucledean_distance_vect
  selectedData$Length <-track_length_vector
  outTable <- rbind(outTable, selectedData)
}