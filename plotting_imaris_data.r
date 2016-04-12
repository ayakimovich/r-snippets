library(gdata) 
library(ggplot2)
library(reshape2)

read_file <- "\\\\130.60.211.239\\greber\\Artur Yakimovich\\Nanolive\\finala_dataset\\hela_atcc_vacv_mock\\03_Statistics\\03_Volume.csv"
save_file <- "\\\\130.60.211.239\\greber\\Artur Yakimovich\\Nanolive\\finala_dataset\\hela_atcc_vacv_mock\\02_Statistics\\02_Volume.pdf"


volume_table = read.table(read_file, sep=",", header=T, skip=3)
#volume_table$Normalized = (volume_table$Value-min(volume_table$Value))/(max(volume_table$Value)-min(volume_table$Value)) - minmax

volume_table$Normalized = volume_table$Value/volume_table$Value[1]


p <- ggplot(volume_table, aes(x = volume_table$ID, y = volume_table$Normalized))
p <- p + geom_line(aes(x = volume_table$Time, y = volume_table$Normalized), size = I(2), alpha = I(0.4))
p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p <- p + theme(axis.line = element_line(colour = "black", size = 1))
p <- p + theme(text = element_text(size=20, face = "bold"))

p <- p  + scale_y_continuous(limits = c(0.30, 1.30), breaks= c(0.30, 1.00, 1.30)) #  + coord_cartesian(ylim = c(0, 1))
#ggsave(file=save_file, plot=p, width=10, height=6)
print(p)
