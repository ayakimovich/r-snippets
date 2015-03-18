library(gdata) 
library(ggplot2)
library(reshape2)

read_file <- "R:\\Data\\150109_particle_to_gfp\\b_optimized\\cat1_data.csv"
save_file <- "R:\\Data\\150109_particle_to_gfp\\b_optimized\\cat1.pdf"
number_of_timepoints = 12;

data_table = read.table(read_file, sep=";", header=T)[-1]
data_headers <- rep(as.vector(read.table(read_file, sep=";", header=F)[1,-1]), each = number_of_timepoints)
time_points <- rep(1:number_of_timepoints, times = length(data_headers)/number_of_timepoints)

molten_data = melt(data_table, na.rm = FALSE, value.name = "intensity")

plot_data <- data.frame(intensity = molten_data$intensity, time_points = time_points, particle_number = as.numeric(data_headers), id = molten_data$variable)
#plot_data <- cbind(intensity = molten_data$intensity, time_points = time_points, particle_number = as.numeric(data_headers))


#print(data_table$Children_VirusParticles_Count, data_table[2]);
#print(data_table[2]);

#x = as.vector(data_table[1]);
#y = as.vector(data_table[2]);
#plot(x, y)

#ggplot(data_table, aes(x = gp, y = y)) + geom_point();


#df <- subset(data_table, data_table$Children_VirusParticles_Count)
#time_point = c(1:12);
#intensity = c(0:1);
#y1 = data_table[3];
#x1 = 1:12

#for (i in 1:ncol(data_table)){
  p <- ggplot(plot_data, aes(x = plot_data$time_point, y = plot_data$intensity, group = plot_data$id))
  # add points and lines
  p <- p + geom_point(aes(x = plot_data$time_point, y = plot_data$intensity, colour = plot_data$particle_number), size = I(4), alpha = I(0.4)) + scale_colour_gradient(low="black", high = "magenta")
  p <- p + geom_line(aes(x = plot_data$time_point, y = plot_data$intensity, colour = plot_data$particle_number), size = I(2), alpha = I(0.4)) 
  # remove background and grid
  p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  p <- p + theme(axis.line = element_line(colour = "black", size = 1))
  # change font size
  p <- p + theme(text = element_text(size=20, face = "bold"))
  # set y axis limits and ticks
  p <- p  + scale_y_continuous(limits = c(0, 0.30), breaks= c(0.00, 0.10, 0.20, 0.30)) #  + coord_cartesian(ylim = c(0, 1))
  ggsave(file=save_file, plot=p, width=10, height=6)
  print(p);
  
#}
  #+ geom_point(data = data_table, aes(y = "intensity"))


