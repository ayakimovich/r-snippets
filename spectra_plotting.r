library(gdata) 
library(ggplot2)
library(reshape2)
library(xlsx)
library(rJava)

file_dapi <- "R:\\Google Drive\\!AY&YY&VA\\Autofluorescent-multi-SpectralArtifactRemoval\\figures\\fig1\\spectra\\dapi.xlsx"
file_cfp <- "R:\\Google Drive\\!AY&YY&VA\\Autofluorescent-multi-SpectralArtifactRemoval\\figures\\fig1\\spectra\\cfp.xlsx"
file_gfp <- "R:\\Google Drive\\!AY&YY&VA\\Autofluorescent-multi-SpectralArtifactRemoval\\figures\\fig1\\spectra\\gfp.xlsx"
file_tritc <- "R:\\Google Drive\\!AY&YY&VA\\Autofluorescent-multi-SpectralArtifactRemoval\\figures\\fig1\\spectra\\tritc.xlsx"
file_cy5 <- "R:\\Google Drive\\!AY&YY&VA\\Autofluorescent-multi-SpectralArtifactRemoval\\figures\\fig1\\spectra\\cy5.xlsx"

sheet_name <- "dichro"
#sheet_name <- "excit"
#sheet_name <- "emitt"

save_file <- "R:\\Google Drive\\!AY&YY&VA\\Autofluorescent-multi-SpectralArtifactRemoval\\figures\\fig1\\spectra\\dichroics.pdf"
#save_file <- "R:\\Google Drive\\!AY&YY&VA\\Autofluorescent-multi-SpectralArtifactRemoval\\figures\\fig1\\spectra\\exciters.pdf"
#save_file <- "R:\\Google Drive\\!AY&YY&VA\\Autofluorescent-multi-SpectralArtifactRemoval\\figures\\fig1\\spectra\\emmiters.pdf"

plot_data <- read.xlsx(header=TRUE, file_dapi, sheetName = sheet_name)
plot_data$filter <- "dapi"

table_cfp <- read.xlsx(header=TRUE, file_cfp, sheetName = sheet_name)
table_cfp$filter <- "cfp"
plot_data <- rbind.data.frame(plot_data, table_cfp)

table_gfp <- read.xlsx(header=TRUE, file_gfp, sheetName = sheet_name)
table_gfp$filter <- "gfp"
plot_data <- rbind.data.frame(plot_data, table_gfp)

table_tritc <- read.xlsx(header=TRUE,file_tritc, sheetName = sheet_name)
table_tritc$filter <- "tritc"
plot_data <- rbind.data.frame(plot_data, table_tritc)

table_cy5 <- read.xlsx(header=TRUE, file_cy5, sheetName = sheet_name)
table_cy5$filter <- "cy5"
plot_data <- rbind.data.frame(plot_data, table_cy5)


p <- ggplot()
# remove background and grid
p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
p <- p + xlim(300, 750)
p <- p + theme(axis.line = element_line(colour = "#303030", size = 1))
# change font size
p <- p + theme(text = element_text(size=20, face = "bold"), plot.margin = unit(c(4, 0.5, 4, 0.5), "cm")) #top, right, bottom, left

p <- p + geom_line(data = plot_data, aes(x = wavelength, y = signal, colour = filter), size = I(1.2), alpha = I(0.5))  + scale_colour_manual(values=c("dapi" = "#6666FF", "cfp" = "#3399FF", "gfp" = "#009933", "tritc" = "#FF5533", "cy5" = "#990000"))

ggsave(file=save_file, plot=p, width=10, height=6)
print(p);
