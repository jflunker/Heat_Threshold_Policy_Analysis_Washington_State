
# This code is to make summary of max, mean, min temperatures and HI
# please make sure here we only use data based on the second extraction method (i.e. average of cells within polygons)
# If you want to use data based on the first extraction method (i.e. centroid), you just need to load centroid data.
# If you have any questions on the code, please send an email to Jihoon Jung (climategeo@gmail.com) or John C Flunker (jflunker@uw.edu)

rm(list = ls())

library(matrixStats) # to use "colSds" function

load("mean_dpt_within_cells.Rdata") # add path to your directory

# to convert to numeric values
for (i in 1:length(extract_temp_all)){
  extract_temp_all[,i] = as.numeric(extract_temp_all[,i])
}

# Celsius to Fahrenheit
for (i in 4:length(extract_temp_all)){
  extract_temp_all[,i] = (9/5) * extract_temp_all[,i] + 32
}

# to calculate monthly summary (mean and SD) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = extract_temp_all[which(extract_temp_all$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean, temp_data_sd), 1)
  all_data = rbind(part_data, all_data)
}

# to calculate total state average 
total_summary_data = {}
for (total_summary in 12:1){
  
  temp_data = all_data[which(all_data[,1] == total_summary),]
  temp_data_mean = colMeans(temp_data)
  total_summary_data = rbind(temp_data_mean, total_summary_data)
  
}

total_summary_data = round(total_summary_data, 1)

# formatting
total_summary_data  = data.frame(total_summary_data)
total_summary_data$text_data = paste(total_summary_data$temp_data_mean, "(", total_summary_data$temp_data_sd, ")", sep = "")
total_summary_data = total_summary_data[,c(4)]

all_data  = data.frame(all_data)
all_data$text_data = paste(all_data$temp_data_mean, "(", all_data$temp_data_sd, ")", sep = "")
all_data = all_data[,c(4)]

interval_value = ncol(extract_temp_all)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])

all_data = data.frame(colnames(extract_temp_all)[4:42], all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "dew_point_temp_county_summary.csv")

########################################################################

rm(list = ls())

load("max_temp_within_cells.Rdata") # add path to your directory

# to convert to numeric values
for (i in 1:length(extract_temp_all)){
  extract_temp_all[,i] = as.numeric(extract_temp_all[,i])
}

# Celsius to Fahrenheit
for (i in 4:length(extract_temp_all)){
  extract_temp_all[,i] = (9/5) * extract_temp_all[,i] + 32
}

# to calculate monthly summary (mean and SD) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = extract_temp_all[which(extract_temp_all$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean, temp_data_sd), 1)
  all_data = rbind(part_data, all_data)
}

# to calculate total state average 
total_summary_data = {}
for (total_summary in 12:1){
  
  temp_data = all_data[which(all_data[,1] == total_summary),]
  temp_data_mean = colMeans(temp_data)
  total_summary_data = rbind(temp_data_mean, total_summary_data)
  
}

total_summary_data = round(total_summary_data, 1)

# formatting
total_summary_data  = data.frame(total_summary_data)
total_summary_data$text_data = paste(total_summary_data$temp_data_mean, "(", total_summary_data$temp_data_sd, ")", sep = "")
total_summary_data = total_summary_data[,c(4)]

all_data  = data.frame(all_data)
all_data$text_data = paste(all_data$temp_data_mean, "(", all_data$temp_data_sd, ")", sep = "")
all_data = all_data[,c(4)]

interval_value = ncol(extract_temp_all)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])

all_data = data.frame(colnames(extract_temp_all)[4:42], all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "max_temp_county_summary.csv")

########################################################################


rm(list = ls())

load("mean_temp_within_cells.Rdata") # add path to your directory

# to convert to numeric values
for (i in 1:length(extract_temp_all)){
  extract_temp_all[,i] = as.numeric(extract_temp_all[,i])
}

# Celsius to Fahrenheit
for (i in 4:length(extract_temp_all)){
  extract_temp_all[,i] = (9/5) * extract_temp_all[,i] + 32
}

# to calculate monthly summary (mean and SD) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = extract_temp_all[which(extract_temp_all$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean, temp_data_sd), 1)
  all_data = rbind(part_data, all_data)
}

# to calculate total state average 
total_summary_data = {}
for (total_summary in 12:1){
  
  temp_data = all_data[which(all_data[,1] == total_summary),]
  temp_data_mean = colMeans(temp_data)
  total_summary_data = rbind(temp_data_mean, total_summary_data)
  
}

total_summary_data = round(total_summary_data, 1)

# formatting
total_summary_data  = data.frame(total_summary_data)
total_summary_data$text_data = paste(total_summary_data$temp_data_mean, "(", total_summary_data$temp_data_sd, ")", sep = "")
total_summary_data = total_summary_data[,c(4)]

all_data  = data.frame(all_data)
all_data$text_data = paste(all_data$temp_data_mean, "(", all_data$temp_data_sd, ")", sep = "")
all_data = all_data[,c(4)]

interval_value = ncol(extract_temp_all)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])

all_data = data.frame(colnames(extract_temp_all)[4:42], all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "mean_temp_county_summary.csv")

########################################################################

rm(list = ls())

load("min_temp_within_cells.Rdata") # add path to your directory

# to convert to numeric values
for (i in 1:length(extract_temp_all)){
  extract_temp_all[,i] = as.numeric(extract_temp_all[,i])
}

# Celsius to Fahrenheit
for (i in 4:length(extract_temp_all)){
  extract_temp_all[,i] = (9/5) * extract_temp_all[,i] + 32
}

# to calculate monthly summary (mean and SD) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = extract_temp_all[which(extract_temp_all$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean, temp_data_sd), 1)
  all_data = rbind(part_data, all_data)
}

# to calculate total state average 
total_summary_data = {}
for (total_summary in 12:1){
  
  temp_data = all_data[which(all_data[,1] == total_summary),]
  temp_data_mean = colMeans(temp_data)
  total_summary_data = rbind(temp_data_mean, total_summary_data)
  
}

total_summary_data = round(total_summary_data, 1)

# formatting
total_summary_data  = data.frame(total_summary_data)
total_summary_data$text_data = paste(total_summary_data$temp_data_mean, "(", total_summary_data$temp_data_sd, ")", sep = "")
total_summary_data = total_summary_data[,c(4)]

all_data  = data.frame(all_data)
all_data$text_data = paste(all_data$temp_data_mean, "(", all_data$temp_data_sd, ")", sep = "")
all_data = all_data[,c(4)]

interval_value = ncol(extract_temp_all)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])

all_data = data.frame(colnames(extract_temp_all)[4:42], all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "min_temp_county_summary.csv")

########################################################################


rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory

# to convert to numeric values
for (i in 1:length(max_HI_within_cells)){
  max_HI_within_cells[,i] = as.numeric(max_HI_within_cells[,i])
}

# to calculate monthly summary (mean and SD) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean, temp_data_sd), 1)
  all_data = rbind(part_data, all_data)
}

# to calculate total state average 
total_summary_data = {}
for (total_summary in 12:1){
  
  temp_data = all_data[which(all_data[,1] == total_summary),]
  temp_data_mean = colMeans(temp_data)
  total_summary_data = rbind(temp_data_mean, total_summary_data)
  
}

total_summary_data = round(total_summary_data, 1)

# formatting
total_summary_data  = data.frame(total_summary_data)
total_summary_data$text_data = paste(total_summary_data$temp_data_mean, "(", total_summary_data$temp_data_sd, ")", sep = "")
total_summary_data = total_summary_data[,c(4)]

all_data  = data.frame(all_data)
all_data$text_data = paste(all_data$temp_data_mean, "(", all_data$temp_data_sd, ")", sep = "")
all_data = all_data[,c(4)]

interval_value = ncol(max_HI_within_cells)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])

all_data = data.frame(colnames(max_HI_within_cells)[4:42], all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "max_HI_county_summary.csv")

########################################################################

rm(list = ls())

load("mean_HI_within_cells.Rdata") # add path to your directory

# to convert to numeric values
for (i in 1:length(mean_HI_within_cells)){
  mean_HI_within_cells[,i] = as.numeric(mean_HI_within_cells[,i])
}

# to calculate monthly summary (mean and SD) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = mean_HI_within_cells[which(mean_HI_within_cells$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean, temp_data_sd), 1)
  all_data = rbind(part_data, all_data)
}

# to calculate total state average 
total_summary_data = {}
for (total_summary in 12:1){
  
  temp_data = all_data[which(all_data[,1] == total_summary),]
  temp_data_mean = colMeans(temp_data)
  total_summary_data = rbind(temp_data_mean, total_summary_data)
  
}

total_summary_data = round(total_summary_data, 1)

# formatting
total_summary_data  = data.frame(total_summary_data)
total_summary_data$text_data = paste(total_summary_data$temp_data_mean, "(", total_summary_data$temp_data_sd, ")", sep = "")
total_summary_data = total_summary_data[,c(4)]

all_data  = data.frame(all_data)
all_data$text_data = paste(all_data$temp_data_mean, "(", all_data$temp_data_sd, ")", sep = "")
all_data = all_data[,c(4)]

interval_value = ncol(mean_HI_within_cells)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])

all_data = data.frame(colnames(mean_HI_within_cells)[4:42], all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "mean_HI_county_summary.csv")

########################################################################

rm(list = ls())

load("min_HI_within_cells.Rdata") # add path to your directory

# to convert to numeric values
for (i in 1:length(min_HI_within_cells)){
  min_HI_within_cells[,i] = as.numeric(min_HI_within_cells[,i])
}

# to calculate monthly summary (mean and SD) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = min_HI_within_cells[which(min_HI_within_cells$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean, temp_data_sd), 1)
  all_data = rbind(part_data, all_data)
}

# to calculate total state average 
total_summary_data = {}
for (total_summary in 12:1){
  
  temp_data = all_data[which(all_data[,1] == total_summary),]
  temp_data_mean = colMeans(temp_data)
  total_summary_data = rbind(temp_data_mean, total_summary_data)
  
}

total_summary_data = round(total_summary_data, 1)

# formatting
total_summary_data  = data.frame(total_summary_data)
total_summary_data$text_data = paste(total_summary_data$temp_data_mean, "(", total_summary_data$temp_data_sd, ")", sep = "")
total_summary_data = total_summary_data[,c(4)]

all_data  = data.frame(all_data)
all_data$text_data = paste(all_data$temp_data_mean, "(", all_data$temp_data_sd, ")", sep = "")
all_data = all_data[,c(4)]

interval_value = ncol(min_HI_within_cells)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])

all_data = data.frame(colnames(min_HI_within_cells)[4:42], all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "min_HI_county_summary.csv")


