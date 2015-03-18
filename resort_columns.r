library(gdata)

# user defined read and write folder

sorting_file <- "R:\\Data\\150109_particle_to_gfp\\c_optimized\\particle_number_sorted_trajecories.csv"
measurements_file <- "R:\\Data\\150109_particle_to_gfp\\c_optimized\\Nuclei_filtered.csv"
save_file <- "R:\\Data\\150109_particle_to_gfp\\c_optimized\\resorted_intensity.csv"

# read the files into the dataframes
sorting <- read.table(sorting_file, sep=";", header=TRUE)
measurements <- read.table(measurements_file, sep=",", header=TRUE)


# main forloop iterating through each row of the sorting file to make a separte column out of it

select_data <- data.frame()
values <- data.frame(matrix(NA, nrow = 15, ncol = dim(sorting)[1]))

final_vector <- NULL

for (iRow in 1:dim(sorting)[1]) {
    querry_trj <- as.integer(sorting$Metadata_Trajectory[iRow])
    querry_well <- as.character(sorting$Metadata_Well[iRow])
    select_data <- subset(measurements, measurements$Metadata_Trajectory == querry_trj & measurements$Metadata_Well == querry_well)
    select_data <- select_data[order(select_data$Metadata_TimePoint),]
    #values <- cbind(values, c(querry_trj, querry_well, as.vector(select_data$Intensity_MeanIntensity_GFP)))
    final_vector <- rbind(final_vector, c(as.character(querry_trj), as.character(querry_well), as.character(as.vector(select_data$Intensity_MeanIntensity_GFP))))
    #write.table(c(querry_trj, querry_well, as.vector(select_data$Intensity_MeanIntensity_GFP)), file = save_file, append=T, row.names=F, col.names=F,  sep=",")
}

write.table(final_vector, file = save_file, append=F, row.names=F, col.names=F,  sep=",")
