library(gdata)

# user defined read and write folder

meta_file <- "R:\\Data\\150109_particle_to_gfp\\c_optimized\\cat4_meta.csv"

read_folder <- "V:\\AY\\140915_Particles_to_gfp_model\\all_time_points\\Processed"
write_folder <- "V:\\AY\\140915_Particles_to_gfp_model\\all_time_points\\Processed\\c_cat4"

# read metafile  
  
meta_table = read.table(meta_file, sep=";", header=F)


# copy files fitting the regexp to a folder
for (i in 1:dim(meta_table)[2]){
  print(i)
    if (as.numeric(as.character(meta_table[2,i])) >= 10){
    trajectory = as.character(meta_table[2,i])
  }
  else trajectory = paste('0', as.character(meta_table[2,i]), sep = "")
  
  print(paste("processing: ", as.character(meta_table[3,i]),'_trj', trajectory, sep = ""))
  
  new_dir <- file.path(write_folder, paste(as.character(meta_table[1,i]),"_",as.character(meta_table[3,i]),'_trj', trajectory, "_r", sep = ""))
  
  matches <- list.files(path = read_folder, pattern=paste(as.character(meta_table[3,i]),".*",'_trj', trajectory, ".*", sep = ""), recursive=F, full.names = T)
  dir.create(new_dir)
  file.copy(matches, new_dir)
}