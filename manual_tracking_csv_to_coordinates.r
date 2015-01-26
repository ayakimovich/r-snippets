library(gdata)

# user defined read and write folder

read_folder <- "D:\\AY\\140915_Particles_to_gfp_model\\tracking_csv\\"

write_folder <- "D:\\AY\\140915_Particles_to_gfp_model\\tracking_cordinates\\"

# get all the file names in the read folder and put them into an array

file_list <- list.files(path = read_folder, pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

# main loop, which takes the x and y coordinates from every file in the input folder and writes them into respective text files

for (i in 1:length(file_list)){
  #formulate a filename to read
  read_file_path <- paste(read_folder, file_list[i], sep = "")
  
  #read the table from the file
  tracking_table = read.table(read_file_path,
                              sep="\t",
                              header=TRUE)
  #formulate new file names by removing last 4 characters and adding a new ending
  write_file_path_x <- paste(write_folder, substr(file_list[i], 1, nchar(file_list[i])-4), "_x.txt", sep = "")
  write_file_path_y <- paste(write_folder, substr(file_list[i], 1, nchar(file_list[i])-4), "_y.txt", sep = "")
  
  #write the x coordinates from the file into respective new file    
  write.table(tracking_table["X"], file = write_file_path_x, append = FALSE, quote = FALSE, sep = " ",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"),
              fileEncoding = "")
  #write the y coordinates from the file into respective new file
  write.table(tracking_table["Y"], file = write_file_path_y, append = FALSE, quote = FALSE, sep = " ",
              eol = "\n", na = "NA", dec = ".", row.names = FALSE,
              col.names = FALSE, qmethod = c("escape", "double"),
              fileEncoding = "")
}