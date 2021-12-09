
# This code is to calculate the average (sd) number of days per month over thresholds 
# please make sure here we only use data based on the second extraction method (i.e. average of cells within polygons)
# If you want to use data based on the first extraction method (i.e. centroid), you just need to load centroid data.
# If you have any questions on the code, please send an email to Jihoon Jung (climategeo@gmail.com) or John C Flunker (jflunker@uw.edu)

rm(list = ls())

library(matrixStats)
library(rgdal)

# over 80

rm(list = ls())

load("D:/Research/6_heat_policy/1_data/prism/extracted/within/max_temp_within_cells.Rdata")

# to convert to numeric values
for (i in 1:length(extract_temp_all)){
  extract_temp_all[,i] = as.numeric(extract_temp_all[,i])
}

# Celsius to Fahrenheit
for (i in 4:length(extract_temp_all)){
  extract_temp_all[,i] = (9/5) * extract_temp_all[,i] + 32
}

# to count the number of days >= 80
for (i in 4:length(extract_temp_all)){
  temp_data = extract_temp_all[,i]
  temp_data[which(temp_data < 80)] = 0 
  temp_data[which(temp_data >= 80)] = 1
  extract_temp_all[,i] = temp_data
}

# to calculate monthly mean number of days >= 80
all_data = {}
for (month_data in 12:1){
  temp_data1 = extract_temp_all[which(extract_temp_all$Month == month_data),]
  
  for (year_data in 2002:2020) {
    
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
    
  }
}

all_data = data.frame(all_data)
colnames(all_data) = c("year", "month", colnames(extract_temp_all)[4:42])

final_data = {}
for (month_data in 12:1){
  temp_data = all_data[which(all_data$month == month_data),]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  
  part_data = round(cbind(month_data, temp_data_mean[3:41], temp_data_sd[3:41]), 1)
  final_data = rbind(part_data, final_data)
}

final_data = data.frame(final_data)
colnames(final_data) = c("month_data", "temp_data_mean", "temp_data_sd")

# formatting
final_data$text_data = paste(final_data$temp_data_mean, " (", final_data$temp_data_sd, ")", sep = "")
final_data = final_data[,c(4)]

interval_value = ncol(extract_temp_all)- 3
final_data = cbind(final_data[1:interval_value], final_data[40:(interval_value*2)], final_data[(interval_value*2+1):(interval_value*3)], 
                   final_data[(interval_value*3+1):(interval_value*4)], final_data[(interval_value*4+1):(interval_value*5)], 
                   final_data[(interval_value*5+1):(interval_value*6)], final_data[(interval_value*6+1):(interval_value*7)], 
                   final_data[(interval_value*7+1):(interval_value*8)], final_data[(interval_value*8+1):(interval_value*9)], 
                   final_data[(interval_value*9+1):(interval_value*10)], final_data[(interval_value*10+1):(interval_value*11)], 
                   final_data[(interval_value*11+1):(interval_value*12)])

final_data = data.frame(colnames(extract_temp_all)[4:42], final_data)
colnames(final_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
final_data = final_data[order(final_data$county),]

write.csv(final_data, file = "max_temp_80_mean_sd.csv")

########################################################################

# over 85

rm(list = ls())

load("D:/Research/6_heat_policy/1_data/prism/extracted/within/max_temp_within_cells.Rdata")

# to convert to numeric values
for (i in 1:length(extract_temp_all)){
  extract_temp_all[,i] = as.numeric(extract_temp_all[,i])
}

# Celsius to Fahrenheit
for (i in 4:length(extract_temp_all)){
  extract_temp_all[,i] = (9/5) * extract_temp_all[,i] + 32
}

# to count the number of days >= 85
for (i in 4:length(extract_temp_all)){
  temp_data = extract_temp_all[,i]
  temp_data[which(temp_data < 85)] = 0 
  temp_data[which(temp_data >= 85)] = 1
  extract_temp_all[,i] = temp_data
}

# to calculate monthly mean number of days >= 85
all_data = {}
for (month_data in 12:1){
  temp_data1 = extract_temp_all[which(extract_temp_all$Month == month_data),]
  
  for (year_data in 2002:2020) {
    
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
    
  }
}

all_data = data.frame(all_data)
colnames(all_data) = c("year", "month", colnames(extract_temp_all)[4:42])

final_data = {}
for (month_data in 12:1){
  temp_data = all_data[which(all_data$month == month_data),]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  
  part_data = round(cbind(month_data, temp_data_mean[3:41], temp_data_sd[3:41]), 1)
  final_data = rbind(part_data, final_data)
}

final_data  = data.frame(final_data)
colnames(final_data) = c("month_data", "temp_data_mean", "temp_data_sd")

# formatting
final_data$text_data = paste(final_data$temp_data_mean, " (", final_data$temp_data_sd, ")", sep = "")
final_data = final_data[,c(4)]

interval_value = ncol(extract_temp_all)- 3
final_data = cbind(final_data[1:interval_value], final_data[40:(interval_value*2)], final_data[(interval_value*2+1):(interval_value*3)], 
                   final_data[(interval_value*3+1):(interval_value*4)], final_data[(interval_value*4+1):(interval_value*5)], 
                   final_data[(interval_value*5+1):(interval_value*6)], final_data[(interval_value*6+1):(interval_value*7)], 
                   final_data[(interval_value*7+1):(interval_value*8)], final_data[(interval_value*8+1):(interval_value*9)], 
                   final_data[(interval_value*9+1):(interval_value*10)], final_data[(interval_value*10+1):(interval_value*11)], 
                   final_data[(interval_value*11+1):(interval_value*12)])

final_data = data.frame(colnames(extract_temp_all)[4:42], final_data)
colnames(final_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
final_data = final_data[order(final_data$county),]

write.csv(final_data, file = "max_temp_85_mean_sd.csv")

#######################################################################

# over 90

rm(list = ls())

load("D:/Research/6_heat_policy/1_data/prism/extracted/within/max_temp_within_cells.Rdata")

# to convert to numeric values
for (i in 1:length(extract_temp_all)){
  extract_temp_all[,i] = as.numeric(extract_temp_all[,i])
}

# Celsius to Fahrenheit
for (i in 4:length(extract_temp_all)){
  extract_temp_all[,i] = (9/5) * extract_temp_all[,i] + 32
}

# to count the number of days >= 90
for (i in 4:length(extract_temp_all)){
  temp_data = extract_temp_all[,i]
  temp_data[which(temp_data < 90)] = 0 
  temp_data[which(temp_data >= 90)] = 1
  extract_temp_all[,i] = temp_data
}

# to calculate monthly mean number of days >= 90
all_data = {}
for (month_data in 12:1){
  temp_data1 = extract_temp_all[which(extract_temp_all$Month == month_data),]
  
  for (year_data in 2002:2020) {
    
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
    
  }
}

all_data = data.frame(all_data)
colnames(all_data) = c("year", "month", colnames(extract_temp_all)[4:42])

final_data = {}
for (month_data in 12:1){
  temp_data = all_data[which(all_data$month == month_data),]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  
  part_data = round(cbind(month_data, temp_data_mean[3:41], temp_data_sd[3:41]), 1)
  final_data = rbind(part_data, final_data)
}

final_data  = data.frame(final_data)
colnames(final_data) = c("month_data", "temp_data_mean", "temp_data_sd")

# formatting
final_data$text_data = paste(final_data$temp_data_mean, " (", final_data$temp_data_sd, ")", sep = "")
final_data = final_data[,c(4)]

interval_value = ncol(extract_temp_all)- 3
final_data = cbind(final_data[1:interval_value], final_data[40:(interval_value*2)], final_data[(interval_value*2+1):(interval_value*3)], 
                   final_data[(interval_value*3+1):(interval_value*4)], final_data[(interval_value*4+1):(interval_value*5)], 
                   final_data[(interval_value*5+1):(interval_value*6)], final_data[(interval_value*6+1):(interval_value*7)], 
                   final_data[(interval_value*7+1):(interval_value*8)], final_data[(interval_value*8+1):(interval_value*9)], 
                   final_data[(interval_value*9+1):(interval_value*10)], final_data[(interval_value*10+1):(interval_value*11)], 
                   final_data[(interval_value*11+1):(interval_value*12)])

final_data = data.frame(colnames(extract_temp_all)[4:42], final_data)
colnames(final_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
final_data = final_data[order(final_data$county),]

write.csv(final_data, file = "max_temp_90_mean_sd.csv")

########################################################################

# over 95

rm(list = ls())

load("D:/Research/6_heat_policy/1_data/prism/extracted/within/max_temp_within_cells.Rdata")

# to convert to numeric values
for (i in 1:length(extract_temp_all)){
  extract_temp_all[,i] = as.numeric(extract_temp_all[,i])
}

# Celsius to Fahrenheit
for (i in 4:length(extract_temp_all)){
  extract_temp_all[,i] = (9/5) * extract_temp_all[,i] + 32
}

# to count the number of days >= 95
for (i in 4:length(extract_temp_all)){
  temp_data = extract_temp_all[,i]
  temp_data[which(temp_data < 95)] = 0 
  temp_data[which(temp_data >= 95)] = 1
  extract_temp_all[,i] = temp_data
}

# to calculate monthly mean number of days >= 95
all_data = {}
for (month_data in 12:1){
  temp_data1 = extract_temp_all[which(extract_temp_all$Month == month_data),]
  
  for (year_data in 2002:2020) {
    
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
    
  }
}

all_data = data.frame(all_data)
colnames(all_data) = c("year", "month", colnames(extract_temp_all)[4:42])

final_data = {}
for (month_data in 12:1){
  temp_data = all_data[which(all_data$month == month_data),]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  
  part_data = round(cbind(month_data, temp_data_mean[3:41], temp_data_sd[3:41]), 1)
  final_data = rbind(part_data, final_data)
}

final_data  = data.frame(final_data)
colnames(final_data) = c("month_data", "temp_data_mean", "temp_data_sd")

# formatting
final_data$text_data = paste(final_data$temp_data_mean, " (", final_data$temp_data_sd, ")", sep = "")
final_data = final_data[,c(4)]

interval_value = ncol(extract_temp_all)- 3
final_data = cbind(final_data[1:interval_value], final_data[40:(interval_value*2)], final_data[(interval_value*2+1):(interval_value*3)], 
                   final_data[(interval_value*3+1):(interval_value*4)], final_data[(interval_value*4+1):(interval_value*5)], 
                   final_data[(interval_value*5+1):(interval_value*6)], final_data[(interval_value*6+1):(interval_value*7)], 
                   final_data[(interval_value*7+1):(interval_value*8)], final_data[(interval_value*8+1):(interval_value*9)], 
                   final_data[(interval_value*9+1):(interval_value*10)], final_data[(interval_value*10+1):(interval_value*11)], 
                   final_data[(interval_value*11+1):(interval_value*12)])

final_data = data.frame(colnames(extract_temp_all)[4:42], final_data)
colnames(final_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
final_data = final_data[order(final_data$county),]

write.csv(final_data,file = "max_temp_95_mean_sd.csv")

########################################################################

# over 80

rm(list = ls())

load("D:/Research/6_heat_policy/1_data/prism/extracted/within/max_HI_within_cells.Rdata")

# to convert to numeric values
for (i in 1:length(max_HI_within_cells)){
  max_HI_within_cells[,i] = as.numeric(max_HI_within_cells[,i])
}

# to count the number of days >= 80
for (i in 4:length(max_HI_within_cells)){
  temp_data = max_HI_within_cells[,i]
  temp_data[which(temp_data < 80)] = 0 
  temp_data[which(temp_data >= 80)] = 1
  max_HI_within_cells[,i] = temp_data
}

# to calculate monthly mean number of days >= 80
all_data = {}
for (month_data in 12:1){
  temp_data1 = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  
  for (year_data in 2002:2020) {
    
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
    
  }
}

all_data = data.frame(all_data)

poly = readOGR("D:/Research/6_heat_policy/1_data/county_shapefile/Washington_state.shp")
colnames(all_data) = c("year", "month", poly$NAME)

final_data = {}
for (month_data in 12:1){
  temp_data = all_data[which(all_data$month == month_data),]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  
  part_data = round(cbind(month_data, temp_data_mean[3:41], temp_data_sd[3:41]), 1)
  final_data = rbind(part_data, final_data)
}

final_data  = data.frame(final_data)
colnames(final_data) = c("month_data", "temp_data_mean", "temp_data_sd")

# formatting
final_data$text_data = paste(final_data$temp_data_mean, " (", final_data$temp_data_sd, ")", sep = "")
final_data = final_data[,c(4)]

interval_value = ncol(max_HI_within_cells)- 3
final_data = cbind(final_data[1:interval_value], final_data[40:(interval_value*2)], final_data[(interval_value*2+1):(interval_value*3)], 
                   final_data[(interval_value*3+1):(interval_value*4)], final_data[(interval_value*4+1):(interval_value*5)], 
                   final_data[(interval_value*5+1):(interval_value*6)], final_data[(interval_value*6+1):(interval_value*7)], 
                   final_data[(interval_value*7+1):(interval_value*8)], final_data[(interval_value*8+1):(interval_value*9)], 
                   final_data[(interval_value*9+1):(interval_value*10)], final_data[(interval_value*10+1):(interval_value*11)], 
                   final_data[(interval_value*11+1):(interval_value*12)])

final_data = data.frame(poly$NAME, final_data)
colnames(final_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
final_data = final_data[order(final_data$county),]

write.csv(final_data,file = "max_HI_80_mean_sd.csv")

########################################################################

# over 85

rm(list = ls())

load("D:/Research/6_heat_policy/1_data/prism/extracted/within/max_HI_within_cells.Rdata")

# to convert to numeric values
for (i in 1:length(max_HI_within_cells)){
  max_HI_within_cells[,i] = as.numeric(max_HI_within_cells[,i])
}

# to count the number of days >= 85
for (i in 4:length(max_HI_within_cells)){
  temp_data = max_HI_within_cells[,i]
  temp_data[which(temp_data < 85)] = 0 
  temp_data[which(temp_data >= 85)] = 1
  max_HI_within_cells[,i] = temp_data
}

# to calculate monthly mean number of days >= 85
all_data = {}
for (month_data in 12:1){
  temp_data1 = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  
  for (year_data in 2002:2020) {
    
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
    
  }
}

all_data = data.frame(all_data)
poly = readOGR("D:/Research/6_heat_policy/1_data/county_shapefile/Washington_state.shp")
colnames(all_data) = c("year", "month", poly$NAME)

final_data = {}
for (month_data in 12:1){
  temp_data = all_data[which(all_data$month == month_data),]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  
  part_data = round(cbind(month_data, temp_data_mean[3:41], temp_data_sd[3:41]), 1)
  final_data = rbind(part_data, final_data)
}

final_data  = data.frame(final_data)
colnames(final_data) = c("month_data", "temp_data_mean", "temp_data_sd")

# formatting
final_data$text_data = paste(final_data$temp_data_mean, " (", final_data$temp_data_sd, ")", sep = "")
final_data = final_data[,c(4)]

interval_value = ncol(max_HI_within_cells)- 3
final_data = cbind(final_data[1:interval_value], final_data[40:(interval_value*2)], final_data[(interval_value*2+1):(interval_value*3)], 
                   final_data[(interval_value*3+1):(interval_value*4)], final_data[(interval_value*4+1):(interval_value*5)], 
                   final_data[(interval_value*5+1):(interval_value*6)], final_data[(interval_value*6+1):(interval_value*7)], 
                   final_data[(interval_value*7+1):(interval_value*8)], final_data[(interval_value*8+1):(interval_value*9)], 
                   final_data[(interval_value*9+1):(interval_value*10)], final_data[(interval_value*10+1):(interval_value*11)], 
                   final_data[(interval_value*11+1):(interval_value*12)])

final_data = data.frame(poly$NAME, final_data)
colnames(final_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
final_data = final_data[order(final_data$county),]

write.csv(final_data, file = "max_HI_85_mean_sd.csv")


########################################################################

# over 90

rm(list = ls())

load("D:/Research/6_heat_policy/1_data/prism/extracted/within/max_HI_within_cells.Rdata")

# to convert to numeric values
for (i in 1:length(max_HI_within_cells)){
  max_HI_within_cells[,i] = as.numeric(max_HI_within_cells[,i])
}

# to count the number of days >= 90
for (i in 4:length(max_HI_within_cells)){
  temp_data = max_HI_within_cells[,i]
  temp_data[which(temp_data < 90)] = 0 
  temp_data[which(temp_data >= 90)] = 1
  max_HI_within_cells[,i] = temp_data
}

# to calculate monthly mean number of days >= 90
all_data = {}
for (month_data in 12:1){
  temp_data1 = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  
  for (year_data in 2002:2020) {
    
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
    
  }
}

all_data = data.frame(all_data)
poly = readOGR("D:/Research/6_heat_policy/1_data/county_shapefile/Washington_state.shp")
colnames(all_data) = c("year", "month", poly$NAME)

final_data = {}
for (month_data in 12:1){
  temp_data = all_data[which(all_data$month == month_data),]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  
  part_data = round(cbind(month_data, temp_data_mean[3:41], temp_data_sd[3:41]), 1)
  final_data = rbind(part_data, final_data)
}

final_data  = data.frame(final_data)
colnames(final_data) = c("month_data", "temp_data_mean", "temp_data_sd")

# formatting
final_data$text_data = paste(final_data$temp_data_mean, " (", final_data$temp_data_sd, ")", sep = "")
final_data = final_data[,c(4)]

interval_value = ncol(max_HI_within_cells)- 3
final_data = cbind(final_data[1:interval_value], final_data[40:(interval_value*2)], final_data[(interval_value*2+1):(interval_value*3)], 
                   final_data[(interval_value*3+1):(interval_value*4)], final_data[(interval_value*4+1):(interval_value*5)], 
                   final_data[(interval_value*5+1):(interval_value*6)], final_data[(interval_value*6+1):(interval_value*7)], 
                   final_data[(interval_value*7+1):(interval_value*8)], final_data[(interval_value*8+1):(interval_value*9)], 
                   final_data[(interval_value*9+1):(interval_value*10)], final_data[(interval_value*10+1):(interval_value*11)], 
                   final_data[(interval_value*11+1):(interval_value*12)])

final_data = data.frame(poly$NAME, final_data)
colnames(final_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
final_data = final_data[order(final_data$county),]

write.csv(final_data, file = "max_HI_90_mean_sd.csv")


########################################################################


# over 95

rm(list = ls())

load("D:/Research/6_heat_policy/1_data/prism/extracted/within/max_HI_within_cells.Rdata")

# to convert to numeric values
for (i in 1:length(max_HI_within_cells)){
  max_HI_within_cells[,i] = as.numeric(max_HI_within_cells[,i])
}

# to count the number of days >= 95
for (i in 4:length(max_HI_within_cells)){
  temp_data = max_HI_within_cells[,i]
  temp_data[which(temp_data < 95)] = 0 
  temp_data[which(temp_data >= 95)] = 1
  max_HI_within_cells[,i] = temp_data
}

# to calculate monthly mean number of days >= 95
all_data = {}
for (month_data in 12:1){
  temp_data1 = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  
  for (year_data in 2002:2020) {
    
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
    
  }
}

all_data = data.frame(all_data)
poly = readOGR("D:/Research/6_heat_policy/1_data/county_shapefile/Washington_state.shp")
colnames(all_data) = c("year", "month", poly$NAME)

final_data = {}
for (month_data in 12:1){
  temp_data = all_data[which(all_data$month == month_data),]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  
  part_data = round(cbind(month_data, temp_data_mean[3:41], temp_data_sd[3:41]), 1)
  final_data = rbind(part_data, final_data)
}

final_data  = data.frame(final_data)
colnames(final_data) = c("month_data", "temp_data_mean", "temp_data_sd")

# formatting
final_data$text_data = paste(final_data$temp_data_mean, " (", final_data$temp_data_sd, ")", sep = "")
final_data = final_data[,c(4)]

interval_value = ncol(max_HI_within_cells)- 3
final_data = cbind(final_data[1:interval_value], final_data[40:(interval_value*2)], final_data[(interval_value*2+1):(interval_value*3)], 
                   final_data[(interval_value*3+1):(interval_value*4)], final_data[(interval_value*4+1):(interval_value*5)], 
                   final_data[(interval_value*5+1):(interval_value*6)], final_data[(interval_value*6+1):(interval_value*7)], 
                   final_data[(interval_value*7+1):(interval_value*8)], final_data[(interval_value*8+1):(interval_value*9)], 
                   final_data[(interval_value*9+1):(interval_value*10)], final_data[(interval_value*10+1):(interval_value*11)], 
                   final_data[(interval_value*11+1):(interval_value*12)])

final_data = data.frame(poly$NAME, final_data)
colnames(final_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
final_data = final_data[order(final_data$county),]

write.csv(final_data,file = "max_HI_95_mean_sd.csv")


########################################################################

# over 89

rm(list = ls())

load("D:/Research/6_heat_policy/1_data/prism/extracted/within/max_temp_within_cells.Rdata")

# to convert to numeric values
for (i in 1:length(extract_temp_all)){
  extract_temp_all[,i] = as.numeric(extract_temp_all[,i])
}

# Celsius to Fahrenheit
for (i in 4:length(extract_temp_all)){
  extract_temp_all[,i] = (9/5) * extract_temp_all[,i] + 32
}

# to count the number of days >= 89
for (i in 4:length(extract_temp_all)){
  temp_data = extract_temp_all[,i]
  temp_data[which(temp_data < 89)] = 0 
  temp_data[which(temp_data >= 89)] = 1
  extract_temp_all[,i] = temp_data
}

# to calculate monthly mean number of days >= 89
all_data = {}
for (month_data in 12:1){
  temp_data1 = extract_temp_all[which(extract_temp_all$Month == month_data),]
  
  for (year_data in 2002:2020) {
    
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
    
  }
}

all_data = data.frame(all_data)
colnames(all_data) = c("year", "month", colnames(extract_temp_all)[4:42])

final_data = {}
for (month_data in 12:1){
  temp_data = all_data[which(all_data$month == month_data),]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  
  part_data = round(cbind(month_data, temp_data_mean[3:41], temp_data_sd[3:41]), 1)
  final_data = rbind(part_data, final_data)
}

final_data  = data.frame(final_data)
colnames(final_data) = c("month_data", "temp_data_mean", "temp_data_sd")

# formatting
final_data$text_data = paste(final_data$temp_data_mean, " (", final_data$temp_data_sd, ")", sep = "")
final_data = final_data[,c(4)]

interval_value = ncol(extract_temp_all)- 3
final_data = cbind(final_data[1:interval_value], final_data[40:(interval_value*2)], final_data[(interval_value*2+1):(interval_value*3)], 
                   final_data[(interval_value*3+1):(interval_value*4)], final_data[(interval_value*4+1):(interval_value*5)], 
                   final_data[(interval_value*5+1):(interval_value*6)], final_data[(interval_value*6+1):(interval_value*7)], 
                   final_data[(interval_value*7+1):(interval_value*8)], final_data[(interval_value*8+1):(interval_value*9)], 
                   final_data[(interval_value*9+1):(interval_value*10)], final_data[(interval_value*10+1):(interval_value*11)], 
                   final_data[(interval_value*11+1):(interval_value*12)])

final_data = data.frame(colnames(extract_temp_all)[4:42], final_data)
colnames(final_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
final_data = final_data[order(final_data$county),]

write.csv(final_data, file = "max_temp_89_mean_sd.csv")

#######################################################################

# over 89

rm(list = ls())

load("D:/Research/6_heat_policy/1_data/prism/extracted/within/max_HI_within_cells.Rdata")

# to convert to numeric values
for (i in 1:length(max_HI_within_cells)){
  max_HI_within_cells[,i] = as.numeric(max_HI_within_cells[,i])
}

# to count the number of days >= 89
for (i in 4:length(max_HI_within_cells)){
  temp_data = max_HI_within_cells[,i]
  temp_data[which(temp_data < 89)] = 0 
  temp_data[which(temp_data >= 89)] = 1
  max_HI_within_cells[,i] = temp_data
}

# to calculate monthly mean number of days >= 89
all_data = {}
for (month_data in 12:1){
  temp_data1 = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  
  for (year_data in 2002:2020) {
    
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
    
  }
}

all_data = data.frame(all_data)
poly = readOGR("D:/Research/6_heat_policy/1_data/county_shapefile/Washington_state.shp")
colnames(all_data) = c("year", "month", poly$NAME)

final_data = {}
for (month_data in 12:1){
  temp_data = all_data[which(all_data$month == month_data),]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colMeans(temp_data_mat)
  temp_data_sd = colSds(temp_data_mat)
  
  part_data = round(cbind(month_data, temp_data_mean[3:41], temp_data_sd[3:41]), 1)
  final_data = rbind(part_data, final_data)
}

final_data  = data.frame(final_data)
colnames(final_data) = c("month_data", "temp_data_mean", "temp_data_sd")

# formatting
final_data$text_data = paste(final_data$temp_data_mean, " (", final_data$temp_data_sd, ")", sep = "")
final_data = final_data[,c(4)]

interval_value = ncol(max_HI_within_cells)- 3
final_data = cbind(final_data[1:interval_value], final_data[40:(interval_value*2)], final_data[(interval_value*2+1):(interval_value*3)], 
                   final_data[(interval_value*3+1):(interval_value*4)], final_data[(interval_value*4+1):(interval_value*5)], 
                   final_data[(interval_value*5+1):(interval_value*6)], final_data[(interval_value*6+1):(interval_value*7)], 
                   final_data[(interval_value*7+1):(interval_value*8)], final_data[(interval_value*8+1):(interval_value*9)], 
                   final_data[(interval_value*9+1):(interval_value*10)], final_data[(interval_value*10+1):(interval_value*11)], 
                   final_data[(interval_value*11+1):(interval_value*12)])

final_data = data.frame(poly$NAME, final_data)
colnames(final_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
final_data = final_data[order(final_data$county),]

write.csv(final_data, file = "max_HI_89_mean_sd.csv")



