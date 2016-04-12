library(gdata) 
library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(plyr)









read_folder1 = "\\\\130.60.211.239\\zinkernagel\\160329-15_CI_R\\analysis2\\"
read_folder2 = "\\\\130.60.211.239\\zinkernagel\\160329-15_CI_R\\analysis2\\"
save_file = "\\\\130.60.211.239\\zinkernagel\\160329-15_CI_R\\analysis2\\aggregation.csv"
save_plot_file = "\\\\130.60.211.239\\zinkernagel\\160329-15_CI_R\\analysis2\\aggregation.pdf"
#set the PATH to perl interpreter for xls reader
perl <- "C:\\strawberry\\perl\\bin\\perl5.22.1.exe"



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}





file_list1 = list.files(path = read_folder1, pattern = ".*_binned_data.xls", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
file_list2 = list.files(path = read_folder2, pattern = ".*_binned_data.xls", all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
read_file_path = paste(read_folder1, file_list[1], sep = "")
binned_table_aggregation = read.xls(read_file_path, sheet = 1, header = TRUE, perl = perl)
condition = gsub("binned_data.xls", "", file_list[1])
condition = gsub("Bact", "", condition)
condition = gsub("Vir", "", condition)
binned_table_aggregation$Bact = strtoi(unlist(strsplit(condition, "_"))[1], base = 0L)
binned_table_aggregation$Vir = strtoi(unlist(strsplit(condition, "_"))[2], base = 0L)
binned_table_aggregation$dataset = 1

for (i in 2:length(file_list)){
  read_file_path = paste(read_folder1, file_list1[i], sep = "")
  binned_table = read.xls(read_file_path, sheet = 1, header = TRUE, perl = perl)
  condition = gsub("binned_data.xls", "", file_list[i])
  condition = gsub("Bact", "", condition)
  condition = gsub("Vir", "", condition)
  binned_table$Bact = strtoi(unlist(strsplit(condition, "_"))[1], base = 0L)
  binned_table$Vir = strtoi(unlist(strsplit(condition, "_"))[2], base = 0L)
  binned_table$dataset = 1
  binned_table_aggregation = rbind(binned_table_aggregation,binned_table)
}
for (i in 1:length(file_list2)){
  read_file_path = paste(read_folder2, file_list1[i], sep = "")
  binned_table = read.xls(read_file_path, sheet = 1, header = TRUE, perl = perl)
  condition = gsub("binned_data.xls", "", file_list[i])
  condition = gsub("Bact", "", condition)
  condition = gsub("Vir", "", condition)
  binned_table$Bact = strtoi(unlist(strsplit(condition, "_"))[1], base = 0L)
  binned_table$Vir = strtoi(unlist(strsplit(condition, "_"))[2], base = 0L)
  binned_table$dataset = 2
  binned_table_aggregation = rbind(binned_table_aggregation,binned_table)
}

#add realtive measurements
binned_table_aggregation$green_relative = binned_table_aggregation$green_count/binned_table_aggregation$Total
binned_table_aggregation$red_relative = binned_table_aggregation$red_count/binned_table_aggregation$Total
binned_table_aggregation$yellow_relative = binned_table_aggregation$yellow_count/binned_table_aggregation$Total
binned_table_aggregation$white_relative = binned_table_aggregation$white_count/binned_table_aggregation$Total


#save aggregate csv
write.table(binned_table_aggregation, file = save_file, append = FALSE, quote = FALSE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

# collapse dataframe by condition computing means and sd
plot_table = ddply(binned_table_aggregation, c("Bact", "Vir"), summarize,
      green_mean = mean(green_relative),
      green_sd   = sd(green_relative),
      green_N    = length(green_relative),
      
      red_mean = mean(red_relative),
      red_sd   = sd(red_relative),
      red_N    = length(red_relative),
      
      yellow_mean = mean(yellow_relative),
      yellow_sd   = sd(yellow_relative),
      yellow_N    = length(yellow_relative),
      
      white_mean = mean(white_relative),
      white_sd   = sd(white_relative),
      white_N    = length(white_relative)

      )

#volume_table$Normalized = (volume_table$Value-min(volume_table$Value))/(max(volume_table$Value)-min(volume_table$Value)) - minmax


plot_font_size = 14
oy_lims = c(0.00, 1.0)
oy_breaks = c(0.00, 0.50, 1.00)
ox_breaks = c(0, 1, 2, 3, 4, 5, 6, 7)
error_bar_line_size = 0.75

p1 <- ggplot(plot_table, aes(Vir, green_mean, fill = Vir)) + scale_fill_distiller(palette = "Greys")
limits <- aes(ymax = plot_table$green_mean + plot_table$green_sd, ymin=plot_table$green_mean - plot_table$green_sd) 
p1 <- p1 + scale_x_continuous(breaks = ox_breaks)
p1 <- p1 + geom_bar(stat="identity", position="dodge") + facet_wrap(~ Bact, nrow = 1)
p1 <- p1 + geom_errorbar(limits, stat="identity", position="dodge", width=0.25, size = error_bar_line_size)
p1 <- p1 + theme(panel.background = element_blank())
p1 <- p1 + theme(text = element_text(size=plot_font_size, face = "bold"))
p1 <- p1  + scale_y_continuous(limits = oy_lims, breaks= oy_breaks) #  + coord_cartesian(ylim = c(0, 1))


p2 <- ggplot(plot_table, aes(Vir, red_mean, fill = Vir)) + scale_fill_distiller(palette = "Greys")
limits2 <- aes(ymax = plot_table$red_mean + plot_table$red_sd, ymin=plot_table$red_mean - plot_table$red_sd) 
p2 <- p2 + scale_x_continuous(breaks = ox_breaks)
p2 <- p2 + geom_bar(stat="identity", position="dodge") + facet_wrap(~ Bact, nrow = 1)
p2 <- p2 + geom_errorbar(limits2, stat="identity", position="dodge", width=0.25, size = error_bar_line_size)
p2 <- p2 + theme(panel.background = element_blank())
p2 <- p2 + theme(text = element_text(size=plot_font_size, face = "bold"))
p2 <- p2  + scale_y_continuous(limits = oy_lims, breaks= oy_breaks) #  + coord_cartesian(ylim = c(0, 1))


p3 <- ggplot(plot_table, aes(Vir, yellow_mean, fill = Vir)) + scale_fill_distiller(palette = "Greys")
limits3 <- aes(ymax = plot_table$yellow_mean + plot_table$yellow_sd, ymin=plot_table$yellow_mean - plot_table$yellow_sd) 
p3 <- p3 + scale_x_continuous(breaks = ox_breaks)
p3 <- p3 + geom_bar(stat="identity", position="dodge") + facet_wrap(~ Bact, nrow = 1)
p3 <- p3 + geom_errorbar(limits3, stat="identity", position="dodge", width=0.25, size = error_bar_line_size)
p3 <- p3 + theme(panel.background = element_blank())
p3 <- p3 + theme(text = element_text(size=plot_font_size, face = "bold"))
p3 <- p3  + scale_y_continuous(limits = oy_lims, breaks= oy_breaks) #  + coord_cartesian(ylim = c(0, 1))


p4 <- ggplot(plot_table, aes(Vir, white_mean, fill = Vir)) + scale_fill_distiller(palette = "Greys")
limits4 <- aes(ymax = plot_table$white_mean + plot_table$white_sd, ymin=plot_table$white_mean - plot_table$white_sd) 
p4 <- p4 + scale_x_continuous(breaks = ox_breaks)
p4 <- p4 + geom_bar(stat="identity", position="dodge") + facet_wrap(~ Bact, nrow = 1)
p4 <- p4 + geom_errorbar(limits4, stat="identity", position="dodge", width=0.25, size = error_bar_line_size)
p4 <- p4 + theme(panel.background = element_blank())
p4 <- p4 + theme(text = element_text(size=plot_font_size, face = "bold"))
p4 <- p4  + scale_y_continuous(limits = oy_lims, breaks= oy_breaks) #  + coord_cartesian(ylim = c(0, 1))


pdf(save_plot_file)
multiplot(p1, p2, p3, p4, cols=1)
dev.off()

