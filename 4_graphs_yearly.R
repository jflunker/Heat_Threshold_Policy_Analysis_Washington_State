
# This code is to make yearly graphs showing the total number of days exceeding HI thresholds (80,85,89,95) and maximum temperatures (80,85,89,95)
# please make sure here we only use data based on the second extraction method (i.e. average of cells within polygons)
# If you want to use data based on the first extraction method (i.e. centroid), you just need to load centroid data.
# If you have any questions on the code, please send an email to Jihoon Jung (climategeo@gmail.com) or John C Flunker (jflunker@uw.edu)

library(reshape2)
library(ggplot2)
library(RColorBrewer)

#################################################################

# to make yearly graphs showing the total number of days over HI thresholds (80,85,89,95) 
# based on the average of cells within polygons

#################################################################

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory

# to calculate the total number of days over thresholds at the state level 

max_HI_row_means_80 = rowMeans(max_HI_within_cells[,c(4:42)]) # state average
max_HI_row_means_80[which(max_HI_row_means_80 < 80)] = 0
max_HI_row_means_80[which(max_HI_row_means_80 >= 80)] = 1 # to count the number of days >= 80

max_HI_row_means_85 = rowMeans(max_HI_within_cells[,c(4:42)]) # state average
max_HI_row_means_85[which(max_HI_row_means_85 < 85)] = 0
max_HI_row_means_85[which(max_HI_row_means_85 >= 85)] = 1 # to count the number of days >= 85

max_HI_row_means_89 = rowMeans(max_HI_within_cells[,c(4:42)]) # state average
max_HI_row_means_89[which(max_HI_row_means_89 < 89)] = 0
max_HI_row_means_89[which(max_HI_row_means_89 >= 89)] = 1 # to count the number of days >= 89

max_HI_row_means_95 = rowMeans(max_HI_within_cells[,c(4:42)]) # state average
max_HI_row_means_95[which(max_HI_row_means_95 < 95)] = 0
max_HI_row_means_95[which(max_HI_row_means_95 >= 95)] = 1 # to count the number of days >= 95

max_HI_year_time = cbind(max_HI_within_cells[,1:3], max_HI_row_means_80, max_HI_row_means_85, 
                          max_HI_row_means_89, max_HI_row_means_95)  

# to calculate the total number of days over XX at the yearly level

temp_part3={}
for (year_data in 2020:2002){
  temp_part1 = max_HI_year_time[which(max_HI_year_time$Year == year_data),]
  temp_part2 = cbind(year_data, sum(temp_part1$max_HI_row_means_80), sum(temp_part1$max_HI_row_means_85),
                     sum(temp_part1$max_HI_row_means_89), sum(temp_part1$max_HI_row_means_95))
  temp_part3 = rbind(temp_part2, temp_part3)
}

max_HI_year = data.frame(temp_part3)
colnames(max_HI_year) = c("year", "over80", "over85", "over89", "over95")

max_HI_year_m = melt(max_HI_year, id.vars = "year")

tiff("number_year_HI.tiff", width = 3000, height = 800,  res = 300, compression = 'lzw')

ggplot(max_HI_year_m, aes(x = year, y = value, fill = variable)) +
  geom_bar(stat="identity", position = position_dodge()) +
  theme(legend.position="bottom") +
  scale_x_continuous(breaks = c(2002:2020),
                     labels = c(2002:2020))+
  ggtitle("Number of days exceeding HI thresholds at the state level (2002-2020)") +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 10, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),)+
  scale_fill_brewer(palette = "YlOrRd") 

dev.off()


#################################################################

# to make yearly graphs showing the total number of days over maximum temperatures (80,85,89,95) 
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

# to calculate the total number of days over thresholds at the state level 

max_temp_row_means_80 = rowMeans(extract_temp_all[,c(4:42)]) # state average
max_temp_row_means_80[which(max_temp_row_means_80 < 80)] = 0
max_temp_row_means_80[which(max_temp_row_means_80 >= 80)] = 1 # to count the number of days >= 80

max_temp_row_means_85 = rowMeans(extract_temp_all[,c(4:42)]) # state average
max_temp_row_means_85[which(max_temp_row_means_85 < 85)] = 0
max_temp_row_means_85[which(max_temp_row_means_85 >= 85)] = 1 # to count the number of days >= 85

max_temp_row_means_89 = rowMeans(extract_temp_all[,c(4:42)]) # state average
max_temp_row_means_89[which(max_temp_row_means_89 < 89)] = 0
max_temp_row_means_89[which(max_temp_row_means_89 >= 89)] = 1 # to count the number of days >= 89

max_temp_row_means_95 = rowMeans(extract_temp_all[,c(4:42)]) # state average
max_temp_row_means_95[which(max_temp_row_means_95 < 95)] = 0
max_temp_row_means_95[which(max_temp_row_means_95 >= 95)] = 1 # to count the number of days >= 95

max_temp_year_time = cbind(extract_temp_all[,1:3], max_temp_row_means_80, max_temp_row_means_85, 
                           max_temp_row_means_89, max_temp_row_means_95)  

# to calculate the total number of days over XX at the yearly level

temp_part3={}
for (year_data in 2020:2002){
  temp_part1 = max_temp_year_time[which(max_temp_year_time$Year == year_data),]
  temp_part2 = cbind(year_data, sum(temp_part1$max_temp_row_means_80), sum(temp_part1$max_temp_row_means_85),
                     sum(temp_part1$max_temp_row_means_89), sum(temp_part1$max_temp_row_means_95))
  temp_part3 = rbind(temp_part2, temp_part3)
}

max_temp_year = data.frame(temp_part3)
colnames(max_temp_year) = c("year", "over80", "over85", "over89", "over95")

max_temp_year_m = melt(max_temp_year, id.vars = "year")

tiff("number_year_max_temperature.tiff", width = 3000, height = 800,  res = 300, compression = 'lzw')

ggplot(max_temp_year_m, aes(x = year, y = value, fill = variable)) +
  geom_bar(stat="identity", position = position_dodge()) +
  theme(legend.position="bottom") +
  scale_x_continuous(breaks = c(2002:2020),
                     labels = c(2002:2020))+
  ggtitle("Number of days exceeding max temperature thresholds at the state level (2002-2020)") +
  theme(axis.text.x = element_text(color = "grey20", size = 10, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 10, face = "plain"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),   
        plot.title = element_text(size = 10, face = "bold"),
        panel.background =  element_rect(fill = "white", colour = NA), 
        panel.border =      element_rect(fill = NA, colour="grey50"), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2),
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),)+
  scale_fill_brewer(palette = "YlOrRd") 

dev.off()

#   geom_smooth(method = "lm", se= FALSE, color = "firebrick1",)  +

