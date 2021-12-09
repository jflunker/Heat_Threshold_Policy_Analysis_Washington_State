
# This code is to make maps showing the total number of days exceeding HI thresholds (80,85,89,95) and 
# maximum temperatures (80,85,89,95) during warm period (May-Sep) and outside of the period (Jan-Apr, Oct-Dec)
# please make sure here we only use data based on the second extraction method (i.e. average of cells within polygons)
# If you want to use data based on the first extraction method (i.e. centroid), you just need to load centroid data.
# If you have any questions on the code, please send an email to Jihoon Jung (climategeo@gmail.com) or John C Flunker (jflunker@uw.edu)

library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(sp)
library(gridExtra)

#################################################################

# to make maps showing the total number of days over HI thresholds (80,85,89,95) 
# during warm period (May-Sep) and outside of the period (Jan-Apr, Oct-Dec)
# based on the average of cells within polygons

#################################################################

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory

# to calculate the total number of days over thresholds 

max_HI_80 = unlist(max_HI_within_cells[,c(4:42)])
max_HI_85 = unlist(max_HI_within_cells[,c(4:42)])
max_HI_89 = unlist(max_HI_within_cells[,c(4:42)])
max_HI_95 = unlist(max_HI_within_cells[,c(4:42)])

max_HI_80[which(max_HI_80 < 80)] = 0
max_HI_80[which(max_HI_80 >= 80)] = 1

max_HI_85[which(max_HI_85 < 85)] = 0
max_HI_85[which(max_HI_85 >= 85)] = 1

max_HI_89[which(max_HI_89 < 89)] = 0
max_HI_89[which(max_HI_89 >= 89)] = 1

max_HI_95[which(max_HI_95 < 95)] = 0
max_HI_95[which(max_HI_95 >= 95)] = 1

max_HI_80_re = matrix(max_HI_80, nrow = nrow(max_HI_within_cells), ncol = ncol(max_HI_within_cells)-3)
max_HI_85_re = matrix(max_HI_85, nrow = nrow(max_HI_within_cells), ncol = ncol(max_HI_within_cells)-3)
max_HI_89_re = matrix(max_HI_89, nrow = nrow(max_HI_within_cells), ncol = ncol(max_HI_within_cells)-3)
max_HI_95_re = matrix(max_HI_95, nrow = nrow(max_HI_within_cells), ncol = ncol(max_HI_within_cells)-3)

max_HI_80_re = cbind(max_HI_within_cells[,1:3], max_HI_80_re)
max_HI_85_re = cbind(max_HI_within_cells[,1:3], max_HI_85_re)
max_HI_89_re = cbind(max_HI_within_cells[,1:3], max_HI_89_re)
max_HI_95_re = cbind(max_HI_within_cells[,1:3], max_HI_95_re)

# to separate the data into warm period (May-Sep) and non-warm period (Jan-Apr, Oct-Dec)

max_HI_80_re_may_sep = max_HI_80_re[which(max_HI_80_re$Month == 5 | max_HI_80_re$Month == 6 | max_HI_80_re$Month == 7 |
                                            max_HI_80_re$Month == 8 | max_HI_80_re$Month == 9),]
max_HI_80_re_others = max_HI_80_re[which(max_HI_80_re$Month == 1 | max_HI_80_re$Month == 2 | max_HI_80_re$Month == 3 | 
                                           max_HI_80_re$Month == 4 | max_HI_80_re$Month == 10 | max_HI_80_re$Month == 11 | 
                                           max_HI_80_re$Month == 12),]
max_HI_80_re_may_sep_colSums = colSums(max_HI_80_re_may_sep)
max_HI_80_re_others_colSums = colSums(max_HI_80_re_others)

max_HI_85_re_may_sep = max_HI_85_re[which(max_HI_85_re$Month == 5 | max_HI_85_re$Month == 6 | max_HI_85_re$Month == 7 |
                                            max_HI_85_re$Month == 8 | max_HI_85_re$Month == 9),]
max_HI_85_re_others = max_HI_85_re[which(max_HI_85_re$Month == 1 | max_HI_85_re$Month == 2 | max_HI_85_re$Month == 3 | 
                                           max_HI_85_re$Month == 4 | max_HI_85_re$Month == 10 | max_HI_85_re$Month == 11 | 
                                           max_HI_85_re$Month == 12),]
max_HI_85_re_may_sep_colSums = colSums(max_HI_85_re_may_sep)
max_HI_85_re_others_colSums = colSums(max_HI_85_re_others)

max_HI_89_re_may_sep = max_HI_89_re[which(max_HI_89_re$Month == 5 | max_HI_89_re$Month == 6 | max_HI_89_re$Month == 7 |
                                            max_HI_89_re$Month == 8 | max_HI_89_re$Month == 9),]
max_HI_89_re_others = max_HI_89_re[which(max_HI_89_re$Month == 1 | max_HI_89_re$Month == 2 | max_HI_89_re$Month == 3 | 
                                           max_HI_89_re$Month == 4 | max_HI_89_re$Month == 10 | max_HI_89_re$Month == 11 | 
                                           max_HI_89_re$Month == 12),]
max_HI_89_re_may_sep_colSums = colSums(max_HI_89_re_may_sep)
max_HI_89_re_others_colSums = colSums(max_HI_89_re_others)

max_HI_95_re_may_sep = max_HI_95_re[which(max_HI_95_re$Month == 5 | max_HI_95_re$Month == 6 | max_HI_95_re$Month == 7 |
                                            max_HI_95_re$Month == 8 | max_HI_95_re$Month == 9),]
max_HI_95_re_others = max_HI_95_re[which(max_HI_95_re$Month == 1 | max_HI_95_re$Month == 2 | max_HI_95_re$Month == 3 | 
                                           max_HI_95_re$Month == 4 | max_HI_95_re$Month == 10 | max_HI_95_re$Month == 11 | 
                                           max_HI_95_re$Month == 12),]
max_HI_95_re_may_sep_colSums = colSums(max_HI_95_re_may_sep)
max_HI_95_re_others_colSums = colSums(max_HI_95_re_others)

max_HI_80_re_may_sep = max_HI_80_re_may_sep_colSums[-c(1,2,3)]
max_HI_80_re_others = max_HI_80_re_others_colSums[-c(1,2,3)]
max_HI_85_re_may_sep = max_HI_85_re_may_sep_colSums[-c(1,2,3)]
max_HI_85_re_others = max_HI_85_re_others_colSums[-c(1,2,3)]
max_HI_89_re_may_sep = max_HI_89_re_may_sep_colSums[-c(1,2,3)]
max_HI_89_re_others = max_HI_89_re_others_colSums[-c(1,2,3)]
max_HI_95_re_may_sep = max_HI_95_re_may_sep_colSums[-c(1,2,3)]
max_HI_95_re_others = max_HI_95_re_others_colSums[-c(1,2,3)]

all_max_HI = cbind(max_HI_80_re_may_sep, max_HI_80_re_others, max_HI_85_re_may_sep, max_HI_85_re_others, 
                   max_HI_89_re_may_sep, max_HI_89_re_others, max_HI_95_re_may_sep, max_HI_95_re_others)

# to merge the extracted data(i.e. all_max_HI) with shapefile for mapping

load("max_temp_within_cells.Rdata") # add path to your directory

name_data = names(extract_temp_all)[-c(1,2,3)]
all_max_HI = data.frame(NAME = names(extract_temp_all)[-c(1,2,3)], all_max_HI)

poly = shapefile("Washington_state.shp") # add path to your directory
poly_re = fortify(poly, region = 'NAME')

all_max_HI_poly <- merge(poly_re, all_max_HI, by.x='id', by.y='NAME')

csv_file_data = read.csv("my_centroid.csv") # add path to your directory
csv_file_data_m = merge(csv_file_data, all_max_HI, by = "NAME")

plot1 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_HI_80_re_may_sep), 
               data=all_max_HI_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max HI 80 from May through September (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_HI_80_re_may_sep, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

plot2 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_HI_85_re_may_sep), 
               data=all_max_HI_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max HI 85 from May through September (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_HI_85_re_may_sep, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

plot3 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_HI_89_re_may_sep), 
               data=all_max_HI_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max HI 89 from May through September (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_HI_89_re_may_sep, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

plot4 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_HI_95_re_may_sep), 
               data=all_max_HI_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max HI 95 from May through September (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_HI_95_re_may_sep, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

tiff("max_HI_map_may_sep.tiff", width = 8000, height = 4000,  res = 300, compression = 'lzw')

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)

dev.off()


##############################################################


plot1 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_HI_80_re_others), 
               data=all_max_HI_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max HI 80 outside May-Sept (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_HI_80_re_others, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

plot2 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_HI_85_re_others), 
               data=all_max_HI_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max HI 85 outside May-Sept (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_HI_85_re_others, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

plot3 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_HI_89_re_others), 
               data=all_max_HI_poly, color='black') + scale_fill_gradient(low='white', high='white') +
  ggtitle("Total number of days exceeding max HI 89 outside May-Sept (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_HI_89_re_others, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

plot4 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_HI_95_re_others), 
               data=all_max_HI_poly, color='black') + scale_fill_gradient(low='white', high='white') +
  ggtitle("Total number of days exceeding max HI 95 outside May-Sept (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_HI_95_re_others, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

tiff("max_HI_map_month_outside.tiff", width = 8000, height = 4000,  res = 300, compression = 'lzw')

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)

dev.off()


#################################################################

# to make maps showing the total number of days over maximum temperature thresholds (80,85,89,95) 
# during warm period (May-Sep) and outside of the period (Jan-Apr, Oct-Dec)
# based on the average of cells within polygons

#################################################################

rm(list = ls())

load("max_temp_within_cells.Rdata") # add path to your directory

# Celsius to Fahrenheit

for (i in 4:length(extract_temp_all)){
  temp_data = as.numeric(extract_temp_all[,i])
  temp_data = (9/5) * temp_data + 32
  extract_temp_all[,i] = round(temp_data, 1)
}

extract_temp_all[,1] = as.numeric(extract_temp_all[,1])
extract_temp_all[,2] = as.numeric(extract_temp_all[,2])
extract_temp_all[,3] = as.numeric(extract_temp_all[,3])

# to calculate the total number of days over thresholds

max_temp_80 = unlist(extract_temp_all[,c(4:42)])
max_temp_85 = unlist(extract_temp_all[,c(4:42)])
max_temp_89 = unlist(extract_temp_all[,c(4:42)])
max_temp_95 = unlist(extract_temp_all[,c(4:42)])

max_temp_80[which(max_temp_80 < 80)] = 0
max_temp_80[which(max_temp_80 >= 80)] = 1

max_temp_85[which(max_temp_85 < 85)] = 0
max_temp_85[which(max_temp_85 >= 85)] = 1

max_temp_89[which(max_temp_89 < 89)] = 0
max_temp_89[which(max_temp_89 >= 89)] = 1

max_temp_95[which(max_temp_95 < 95)] = 0
max_temp_95[which(max_temp_95 >= 95)] = 1

max_temp_80_re = matrix(max_temp_80, nrow = nrow(extract_temp_all), ncol = ncol(extract_temp_all)-3)
max_temp_85_re = matrix(max_temp_85, nrow = nrow(extract_temp_all), ncol = ncol(extract_temp_all)-3)
max_temp_89_re = matrix(max_temp_89, nrow = nrow(extract_temp_all), ncol = ncol(extract_temp_all)-3)
max_temp_95_re = matrix(max_temp_95, nrow = nrow(extract_temp_all), ncol = ncol(extract_temp_all)-3)

max_temp_80_re = cbind(extract_temp_all[,1:3], max_temp_80_re)
max_temp_85_re = cbind(extract_temp_all[,1:3], max_temp_85_re)
max_temp_89_re = cbind(extract_temp_all[,1:3], max_temp_89_re)
max_temp_95_re = cbind(extract_temp_all[,1:3], max_temp_95_re)

# to separate the data into warm period (May-Sep) and non-warm period (Jan-Apr, Oct-Dec)

max_temp_80_re_may_sep = max_temp_80_re[which(max_temp_80_re$Month == 5 | max_temp_80_re$Month == 6 | max_temp_80_re$Month == 7 |
                                                max_temp_80_re$Month == 8 | max_temp_80_re$Month == 9),]
max_temp_80_re_others = max_temp_80_re[which(max_temp_80_re$Month == 1 | max_temp_80_re$Month == 2 | max_temp_80_re$Month == 3 | 
                                               max_temp_80_re$Month == 4 | max_temp_80_re$Month == 10 | max_temp_80_re$Month == 11 | 
                                               max_temp_80_re$Month == 12),]
max_temp_80_re_may_sep_colSums = colSums(max_temp_80_re_may_sep)
max_temp_80_re_others_colSums = colSums(max_temp_80_re_others)

max_temp_85_re_may_sep = max_temp_85_re[which(max_temp_85_re$Month == 5 | max_temp_85_re$Month == 6 | max_temp_85_re$Month == 7 |
                                                max_temp_85_re$Month == 8 | max_temp_85_re$Month == 9),]
max_temp_85_re_others = max_temp_85_re[which(max_temp_85_re$Month == 1 | max_temp_85_re$Month == 2 | max_temp_85_re$Month == 3 | 
                                               max_temp_85_re$Month == 4 | max_temp_85_re$Month == 10 | max_temp_85_re$Month == 11 | 
                                               max_temp_85_re$Month == 12),]
max_temp_85_re_may_sep_colSums = colSums(max_temp_85_re_may_sep)
max_temp_85_re_others_colSums = colSums(max_temp_85_re_others)

max_temp_89_re_may_sep = max_temp_89_re[which(max_temp_89_re$Month == 5 | max_temp_89_re$Month == 6 | max_temp_89_re$Month == 7 |
                                                max_temp_89_re$Month == 8 | max_temp_89_re$Month == 9),]
max_temp_89_re_others = max_temp_89_re[which(max_temp_89_re$Month == 1 | max_temp_89_re$Month == 2 | max_temp_89_re$Month == 3 | 
                                               max_temp_89_re$Month == 4 |max_temp_89_re$Month == 10 | max_temp_89_re$Month == 11 | 
                                               max_temp_89_re$Month == 12),]
max_temp_89_re_may_sep_colSums = colSums(max_temp_89_re_may_sep)
max_temp_89_re_others_colSums = colSums(max_temp_89_re_others)

max_temp_95_re_may_sep = max_temp_95_re[which(max_temp_95_re$Month == 5 | max_temp_95_re$Month == 6 | max_temp_95_re$Month == 7 |
                                                max_temp_95_re$Month == 8 | max_temp_95_re$Month == 9),]
max_temp_95_re_others = max_temp_95_re[which(max_temp_95_re$Month == 1 | max_temp_95_re$Month == 2 | max_temp_95_re$Month == 3 | 
                                               max_temp_95_re$Month == 4 |max_temp_95_re$Month == 10 | max_temp_95_re$Month == 11 | 
                                               max_temp_95_re$Month == 12),]
max_temp_95_re_may_sep_colSums = colSums(max_temp_95_re_may_sep)
max_temp_95_re_others_colSums = colSums(max_temp_95_re_others)

max_temp_80_re_may_sep = max_temp_80_re_may_sep_colSums[-c(1,2,3)]
max_temp_80_re_others = max_temp_80_re_others_colSums[-c(1,2,3)]
max_temp_85_re_may_sep = max_temp_85_re_may_sep_colSums[-c(1,2,3)]
max_temp_85_re_others = max_temp_85_re_others_colSums[-c(1,2,3)]
max_temp_89_re_may_sep = max_temp_89_re_may_sep_colSums[-c(1,2,3)]
max_temp_89_re_others = max_temp_89_re_others_colSums[-c(1,2,3)]
max_temp_95_re_may_sep = max_temp_95_re_may_sep_colSums[-c(1,2,3)]
max_temp_95_re_others = max_temp_95_re_others_colSums[-c(1,2,3)]

all_max_HI = cbind(max_temp_80_re_may_sep, max_temp_80_re_others, max_temp_85_re_may_sep, max_temp_85_re_others, 
                   max_temp_89_re_may_sep, max_temp_89_re_others, max_temp_95_re_may_sep, max_temp_95_re_others)

# to merge the extracted data(i.e. all_max_HI) with shapefile for mapping

load("max_temp_within_cells.Rdata") # add path to your directory

name_data = names(extract_temp_all)[-c(1,2,3)]
all_max_HI = data.frame(NAME = names(extract_temp_all)[-c(1,2,3)], all_max_HI)

poly = shapefile("Washington_state.shp") # add path to your directory
poly_re = fortify(poly, region = 'NAME')

all_max_temp_poly <- merge(poly_re, all_max_HI, by.x='id', by.y='NAME')

csv_file_data = read.csv("my_centroid.csv") # add path to your directory
csv_file_data_m = merge(csv_file_data, all_max_HI, by = "NAME")


plot1 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_temp_80_re_may_sep), 
               data=all_max_temp_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max temperature 80 from May through September (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_temp_80_re_may_sep, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

plot2 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_temp_85_re_may_sep), 
               data=all_max_temp_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max temperature 85 from May through September (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_temp_85_re_may_sep, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

plot3 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_temp_89_re_may_sep), 
               data=all_max_temp_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max temperature 89 from May through September (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_temp_89_re_may_sep, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

plot4 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_temp_95_re_may_sep), 
               data=all_max_temp_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max temperature 95 from May through September (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_temp_95_re_may_sep, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

tiff("max_temp_map_may_sep.tiff", width = 8000, height = 4000,  res = 300, compression = 'lzw')

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)

dev.off()


##############################################################


plot1 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_temp_80_re_others), 
               data=all_max_temp_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max temperature 80 outside May-Sept (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_temp_80_re_others, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

plot2 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_temp_85_re_others), 
               data=all_max_temp_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max temperature 85 outside May-Sept (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_temp_85_re_others, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

plot3 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_temp_89_re_others), 
               data=all_max_temp_poly, color='black') + scale_fill_gradient(low='white', high='white') +
  ggtitle("Total number of days exceeding max temperature 89 outside May-Sept (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_temp_89_re_others, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

plot4 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_temp_95_re_others), 
               data=all_max_temp_poly, color='black') + scale_fill_gradient(low='white', high='white') +
  ggtitle("Total number of days exceeding max temperature 95 outside May-Sept (2002-2020)") +
  geom_text(data=csv_file_data, aes(label = NAME, x = centroidx , y = centroidy + 0.05))  +
  geom_text(data=csv_file_data_m, aes(label = max_temp_95_re_others, x = centroidx , y = centroidy - 0.05)) +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 20, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),) +
  coord_sf()

tiff("max_temp_map_month_outside.tiff", width = 8000, height = 4000,  res = 300, compression = 'lzw')

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)

dev.off()


