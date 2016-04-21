library(gdata) 
library(ggplot2)
library(reshape2)

read_file <- "\\\\130.60.211.239\\greber\\Artur Yakimovich\\Nanolive\\finala_dataset\\infected_hela_atcc_vacv\\03_Statistics\\03_Volume.csv"
save_folder <- "\\\\130.60.211.239\\greber\\Artur Yakimovich\\Nanolive\\finala_dataset\\infected_hela_atcc_vacv\\02_Statistics\\volume_plots\\"
dir.create(save_folder)

volume_table = read.table(read_file, sep=",", header=T, skip=3)
#volume_table$Normalized = (volume_table$Value-min(volume_table$Value))/(max(volume_table$Value)-min(volume_table$Value)) - minmax

volume_table$Normalized = volume_table$Value/volume_table$Value[1]
print(max(volume_table$Normalized)-min(volume_table$Normalized))
for (i in 1:500){
  p <- ggplot(volume_table, aes(x = volume_table$ID, y = volume_table$Normalized))
  p <- p + theme_bw()
  p <- p + geom_line(aes(x = volume_table$Time, y = volume_table$Normalized), size = I(2), alpha = I(0.4))
  #p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  p <- p  + scale_y_continuous(limits = c(0.50, 1.50), breaks= c(0.50, 1.00, 1.50)) #  + coord_cartesian(ylim = c(0, 1))
  p <- p  + scale_x_continuous(limits = c(0.00, 500.00), breaks= c(100, 200, 300, 400, 500))
  p <- p + geom_vline(xintercept=i, linetype="dashed", size = 2, colour = "#cc0000")
  p <- p + labs(y = "realtive volume change", x = "min, post infection")
  p <- p + theme(
                 #panel.border =  element_blank(),
                 #axis.line = element_line(colour = "black", size = 1.5),
                 panel.border = element_rect(colour = "black", size=1.5),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_line(colour = "Grey"),
                 panel.background = element_blank(),
                 plot.margin = unit(c(3,0,3,0), "cm"),
                 text = element_text(size=26, face = "bold"))
  save_file = paste(save_folder, gsub(" ", "",paste("volume_", i, ".tiff"),fixed = TRUE))
  ggsave(file=save_file, plot=p, width=10, height=6,dpi=72)
  print(p)
}