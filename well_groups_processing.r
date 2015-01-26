library(gdata)

# user defined read and write folder

measurements_file <- "R:\\Data\\150105_Cell_cycle\\fixed-results\\KNIMEout_fucci_score.csv"

treatment_doses_files <- c("R:\\Data\\150105_Cell_cycle\\live-results\\RO-3306_doses.csv",
"R:\\Data\\150105_Cell_cycle\\live-results\\Reservatol_doses.csv")



# define groups
doses <- c("02","03","04","05","06","07","08","09","10","11")
replica <- rbind(c("B","C","D"),c("E","F","G"))



# determine number of Groups and Replica

nGroups <- dim(replica)[1]
nReplica <- dim(replica)[2]
nDoses <- length(doses)


#check if doses match dimensions
if (nGroups != length(treatment_doses_files)){
  error('doses file dosent match number of groups')
}

dosesGroupped <- array (0, dim=c(1,nGroups))

measurements <- read.table(measurements_file, sep=",", header=TRUE)


#add empty new columns

measurements["sum_counts"] <- NA
measurements["yellow_percent"] <- NA
measurements["green_percent"] <- NA
measurements["red_percent"] <- NA
measurements["white_percent"] <- NA

# compute new values
measurements$sum_counts <- measurements$yellow_count + measurements$green_count + measurements$red_count + measurements$white_count
measurements$yellow_percent <- measurements$yellow_count / measurements$sum_counts
measurements$green_percent <- measurements$green_count / measurements$sum_counts
measurements$red_percent <- measurements$red_count / measurements$sum_counts 
measurements$white_percent <- measurements$white_count / measurements$sum_counts

dosesArray <- list()
for (iGroup in 1:nGroups){
  dosesGroupped <- data.frame(treatment  = character(nDoses), sum_counts_mean = numeric(nDoses) , sum_counts_sd = numeric(nDoses),
                              yellow_percent_mean = numeric(nDoses), yellow_percent_sd = numeric(nDoses),
                              green_percent_mean = numeric(nDoses), green_percent_sd = numeric(nDoses),
                              red_percent_mean = numeric(nDoses), red_percent_sd = numeric(nDoses),
                              white_percent_mean = numeric(nDoses), white_percent_sd = numeric(nDoses), yellow_dapi_mean = numeric(nDoses), 
                              yellow_dapi_sd = numeric(nDoses), red_dapi_mean = numeric(nDoses), red_dapi_sd = numeric(nDoses),
                              green_dapi_mean = numeric(nDoses), green_dapi_sd = numeric(nDoses), white_dapi_mean = numeric(nDoses),
                              white_dapi_sd = numeric(nDoses))

  dosesGroupped["treatment"] <-  read.table(treatment_doses_files[iGroup], sep=",", header=FALSE)
    
#     

  for (iDose in 1:nDoses){
    selectData <- data.frame()
    for (iReplica in 1:nReplica){
      selectData <- rbind(selectData, subset(measurements, Column_Index == as.numeric(doses[iDose]) & Row_Index == replica[iGroup,iReplica]))
    }

    dosesGroupped["sum_counts_mean"][iDose,1] <- mean(selectData$sum_counts)
    dosesGroupped["sum_counts_sd"][iDose,1] <- sd(selectData$sum_counts)
    dosesGroupped["yellow_percent_mean"][iDose,1] <- mean(selectData$yellow_percent)
    dosesGroupped["yellow_percent_sd"][iDose,1] <- sd(selectData$yellow_percent)
    dosesGroupped["green_percent_mean"][iDose,1] <- mean(selectData$green_percent)
    dosesGroupped["green_percent_sd"][iDose,1] <- sd(selectData$green_percent)
    dosesGroupped["red_percent_mean"][iDose,1] <- mean(selectData$red_percent)
    dosesGroupped["red_percent_sd"][iDose,1] <- sd(selectData$red_percent)
    dosesGroupped["white_percent_mean"][iDose,1] <- mean(selectData$white_percent)
    dosesGroupped["white_percent_sd"][iDose,1] <- sd(selectData$white_percent)
    dosesGroupped["yellow_dapi_mean"][iDose,1] <- mean(selectData$mean_int_dapi_yellow)
    dosesGroupped["yellow_dapi_sd"][iDose,1] <- sd(selectData$mean_int_dapi_yellow)
    dosesGroupped["red_dapi_mean"][iDose,1] <- mean(selectData$mean_int_dapi_red)
    dosesGroupped["red_dapi_sd"][iDose,1] <- sd(selectData$mean_int_dapi_red)
    dosesGroupped["green_dapi_mean"][iDose,1] <- mean(selectData$mean_int_dapi_green)
    dosesGroupped["green_dapi_sd"][iDose,1] <- sd(selectData$mean_int_dapi_green)
    dosesGroupped["white_dapi_mean"][iDose,1] <- mean(selectData$mean_int_dapi_white)
    dosesGroupped["white_dapi_sd"][iDose,1] <- sd(selectData$mean_int_dapi_white)

    rm(selectData)
  }
  dosesArray[[iGroup]] <- dosesGroupped

# save csv
    write.table(dosesGroupped, file =  paste(substr(treatment_doses_files[iGroup], 1, nchar(treatment_doses_files[iGroup])-4), "_results.csv"), append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE)

# plot & save the data
    
    #data_mean <- table(dosesGroupped$sum_counts_mean, dosesGroupped$yellow_percent_mean, dosesGroupped$green_percent_mean, dosesGroupped$red_percent_mean, dosesGroupped$white_percent_mean)
    
    #library(ggplot2)
    
    #barplot(data_mean)#, aes("dose", fill=cut)) + geom_bar(position="dodge") + opts(title=substr(treatment_doses_files[iGroup], 1, nchar(treatment_doses_files[iGroup])-9))

}




