library(gdata)

# user defined read and write folder

measurements_file <- "R:\\Data\\150109_particle_to_gfp\\c_optimized\\Nuclei.csv"
new_file <- "R:\\Data\\150109_particle_to_gfp\\c_optimized\\Nuclei_filtered.csv"

center_x <- 500
center_y <- 500

# body of the program

# read the data file
measurements <- read.table(measurements_file, sep=",", header=TRUE)

selectedData <- data.frame(measurements$Metadata_TimePoint,measurements$Metadata_Trajectory, measurements$Metadata_Well) #, measurements$Location_Center_X, measurements$Location_Center_Y


duplicate_list_all_occurances <- duplicated(selectedData) | duplicated(selectedData,  fromLast = TRUE)
unique_duplicates <- unique(selectedData[duplicate_list_all_occurances, ])


# create a data frame without duplicates
new_measurements <- measurements[!duplicate_list_all_occurances, ]

# iterate through duplicates and add only selected

for (iRow in 1:dim(unique_duplicates)[1]) {

    querryWell <- unique_duplicates$measurements.Metadata_Well[iRow]
    querryTrajectory <- unique_duplicates$measurements.Metadata_Trajectory[iRow]
    querryTimePoint <- unique_duplicates$measurements.Metadata_TimePoint[iRow]
  
  
    # obtain information about all the objects involved in the duplicate
    
    selectDuplicate <- data.frame()
    selectDuplicate <- rbind(selectDuplicate, subset(measurements, Metadata_Well == querryWell & Metadata_Trajectory == querryTrajectory & Metadata_TimePoint == querryTimePoint))
    

    # select the rows to delete in the subset and delete the rows farthest away from the center
    # in 2D the farthest away is the object with the largest sum of distances (x and y) from the center
    closestObject_idx <- which.min(abs(selectDuplicate$Location_Center_X - center_x) + abs(selectDuplicate$Location_Center_Y - center_y))
      
    new_measurements <- rbind (new_measurements, selectDuplicate[closestObject_idx,])

}

# save csv
write.table(new_measurements, file =  new_file, append = FALSE, quote = TRUE, sep = ",", eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)