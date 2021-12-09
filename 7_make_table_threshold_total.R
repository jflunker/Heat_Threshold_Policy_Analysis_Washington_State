
# This code is to show the total number of days over thresholdsd at the county level
# please make sure here we only use data based on the second extraction method (i.e. average of cells within polygons)
# If you want to use data based on the first extraction method (i.e. centroid), you just need to load centroid data.
# If you have any questions on the code, please send an email to Jihoon Jung (climategeo@gmail.com) or John C Flunker (jflunker@uw.edu)

library(matrixStats)

# over 80

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

# to count the number of days >= 80
for (i in 4:length(extract_temp_all)){
  temp_data = extract_temp_all[,i]
  temp_data[which(temp_data < 80)] = 0 
  temp_data[which(temp_data >= 80)] = 1
  extract_temp_all[,i] = temp_data
}

# to calculate monthly summary (mean) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = extract_temp_all[which(extract_temp_all$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colSums(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean), 1)
  all_data = rbind(part_data, all_data)
}

all_data  = data.frame(all_data)
all_data = all_data[,c(2)]

# formatting
interval_value = ncol(extract_temp_all)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])
total_data_mean = round(colMeans(all_data),1)
total_data_sum = colSums(all_data)
total_data_sd = round(colSds(all_data),1)
total_summary = data.frame(cbind(c("mean", "sum", "sd"), rbind(total_data_mean, total_data_sum, total_data_sd)))
colnames(total_summary) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

all_data = data.frame(c(colnames(extract_temp_all)[4:42]), all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]

all_data = data.frame(rbind(all_data, total_summary))
write.csv(all_data, file = "max_temp_80.csv")

########################################################################

# over 85

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

# to count the number of days >= 85
for (i in 4:length(extract_temp_all)){
  temp_data = extract_temp_all[,i]
  temp_data[which(temp_data < 85)] = 0 
  temp_data[which(temp_data >= 85)] = 1
  extract_temp_all[,i] = temp_data
}

# to calculate monthly summary (mean) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = extract_temp_all[which(extract_temp_all$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colSums(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean), 1)
  all_data = rbind(part_data, all_data)
}

all_data  = data.frame(all_data)
all_data = all_data[,c(2)]

# formatting
interval_value = ncol(extract_temp_all)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])
total_data_mean = round(colMeans(all_data),1)
total_data_sum = colSums(all_data)
total_data_sd = round(colSds(all_data),1)
total_summary = data.frame(cbind(c("mean", "sum", "sd"), rbind(total_data_mean, total_data_sum, total_data_sd)))
colnames(total_summary) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

all_data = data.frame(c(colnames(extract_temp_all)[4:42]), all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]

all_data = data.frame(rbind(all_data, total_summary))
write.csv(all_data, file = "max_temp_85.csv")

#######################################################################

# over 90

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

# to count the number of days >= 90
for (i in 4:length(extract_temp_all)){
  temp_data = extract_temp_all[,i]
  temp_data[which(temp_data < 90)] = 0 
  temp_data[which(temp_data >= 90)] = 1
  extract_temp_all[,i] = temp_data
}

# to calculate monthly summary (mean) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = extract_temp_all[which(extract_temp_all$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colSums(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean), 1)
  all_data = rbind(part_data, all_data)
}

all_data  = data.frame(all_data)
all_data = all_data[,c(2)]

# formatting
interval_value = ncol(extract_temp_all)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])
total_data_mean = round(colMeans(all_data),1)
total_data_sum = colSums(all_data)
total_data_sd = round(colSds(all_data),1)
total_summary = data.frame(cbind(c("mean", "sum", "sd"), rbind(total_data_mean, total_data_sum, total_data_sd)))
colnames(total_summary) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

all_data = data.frame(c(colnames(extract_temp_all)[4:42]), all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]

all_data = data.frame(rbind(all_data, total_summary))
write.csv(all_data, file = "max_temp_90.csv")

########################################################################

# over 95

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

# to count the number of days >= 95
for (i in 4:length(extract_temp_all)){
  temp_data = extract_temp_all[,i]
  temp_data[which(temp_data < 95)] = 0 
  temp_data[which(temp_data >= 95)] = 1
  extract_temp_all[,i] = temp_data
}

# to calculate monthly summary (mean) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = extract_temp_all[which(extract_temp_all$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colSums(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean), 1)
  all_data = rbind(part_data, all_data)
}

all_data  = data.frame(all_data)
all_data = all_data[,c(2)]

# formatting
interval_value = ncol(extract_temp_all)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])
total_data_mean = round(colMeans(all_data),1)
total_data_sum = colSums(all_data)
total_data_sd = round(colSds(all_data),1)
total_summary = data.frame(cbind(c("mean", "sum", "sd"), rbind(total_data_mean, total_data_sum, total_data_sd)))
colnames(total_summary) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

all_data = data.frame(c(colnames(extract_temp_all)[4:42]), all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]

all_data = data.frame(rbind(all_data, total_summary))
write.csv(all_data, file = "max_temp_95.csv")

########################################################################

# over 80

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory

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

# to calculate monthly summary (mean) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colSums(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean), 1)
  all_data = rbind(part_data, all_data)
}

all_data  = data.frame(all_data)
all_data = all_data[,c(2)]

# formatting
interval_value = ncol(max_HI_within_cells)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])
total_data_mean = round(colMeans(all_data),1)
total_data_sum = colSums(all_data)
total_data_sd = round(colSds(all_data),1)
total_summary = data.frame(cbind(c("mean", "sum", "sd"), rbind(total_data_mean, total_data_sum, total_data_sd)))
colnames(total_summary) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

all_data = data.frame(c(colnames(max_HI_within_cells)[4:42]), all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]

all_data = data.frame(rbind(all_data, total_summary))
write.csv(all_data, file = "max_HI_80.csv")

########################################################################

# over 85

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory

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

# to calculate monthly summary (mean) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colSums(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean), 1)
  all_data = rbind(part_data, all_data)
}

all_data  = data.frame(all_data)
all_data = all_data[,c(2)]

# formatting
interval_value = ncol(max_HI_within_cells)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])
total_data_mean = round(colMeans(all_data),1)
total_data_sum = colSums(all_data)
total_data_sd = round(colSds(all_data),1)
total_summary = data.frame(cbind(c("mean", "sum", "sd"), rbind(total_data_mean, total_data_sum, total_data_sd)))
colnames(total_summary) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

all_data = data.frame(c(colnames(max_HI_within_cells)[4:42]), all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]

all_data = data.frame(rbind(all_data, total_summary))
write.csv(all_data, file = "max_HI_85.csv")


########################################################################

# over 90

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory

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

# to calculate monthly summary (mean) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colSums(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean), 1)
  all_data = rbind(part_data, all_data)
}

all_data  = data.frame(all_data)
all_data = all_data[,c(2)]

# formatting
interval_value = ncol(max_HI_within_cells)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])
total_data_mean = round(colMeans(all_data),1)
total_data_sum = colSums(all_data)
total_data_sd = round(colSds(all_data),1)
total_summary = data.frame(cbind(c("mean", "sum", "sd"), rbind(total_data_mean, total_data_sum, total_data_sd)))
colnames(total_summary) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

all_data = data.frame(c(colnames(max_HI_within_cells)[4:42]), all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]

all_data = data.frame(rbind(all_data, total_summary))
write.csv(all_data, file = "max_HI_90.csv")


########################################################################


# over 95

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory

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

# to calculate monthly summary (mean) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colSums(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean), 1)
  all_data = rbind(part_data, all_data)
}

all_data  = data.frame(all_data)
all_data = all_data[,c(2)]

# formatting
interval_value = ncol(max_HI_within_cells)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])
total_data_mean = round(colMeans(all_data),1)
total_data_sum = colSums(all_data)
total_data_sd = round(colSds(all_data),1)
total_summary = data.frame(cbind(c("mean", "sum", "sd"), rbind(total_data_mean, total_data_sum, total_data_sd)))
colnames(total_summary) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

all_data = data.frame(c(colnames(max_HI_within_cells)[4:42]), all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]

all_data = data.frame(rbind(all_data, total_summary))
write.csv(all_data, file = "max_HI_95.csv")


########################################################################

# over 89

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

# to count the number of days >= 89
for (i in 4:length(extract_temp_all)){
  temp_data = extract_temp_all[,i]
  temp_data[which(temp_data < 89)] = 0 
  temp_data[which(temp_data >= 89)] = 1
  extract_temp_all[,i] = temp_data
}

# to calculate monthly summary (mean) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = extract_temp_all[which(extract_temp_all$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colSums(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean), 1)
  all_data = rbind(part_data, all_data)
}

all_data  = data.frame(all_data)
all_data = all_data[,c(2)]

# formatting
interval_value = ncol(extract_temp_all)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])
total_data_mean = round(colMeans(all_data),1)
total_data_sum = colSums(all_data)
total_data_sd = round(colSds(all_data),1)
total_summary = data.frame(cbind(c("mean", "sum", "sd"), rbind(total_data_mean, total_data_sum, total_data_sd)))
colnames(total_summary) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

all_data = data.frame(c(colnames(extract_temp_all)[4:42]), all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]

all_data = data.frame(rbind(all_data, total_summary))
write.csv(all_data, file = "max_temp_89.csv")

#######################################################################

# over 89

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory

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

# to calculate monthly summary (mean) at the county level
all_data = {}
for (month_data in 12:1){
  
  temp_data = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  temp_data = temp_data[,-c(1,2,3)]
  temp_data_mat = as.matrix(temp_data)
  
  temp_data_mean = colSums(temp_data_mat)
  part_data = round(cbind(month_data, temp_data_mean), 1)
  all_data = rbind(part_data, all_data)
}

all_data  = data.frame(all_data)
all_data = all_data[,c(2)]

# formatting
interval_value = ncol(max_HI_within_cells)- 3
all_data = cbind(all_data[1:interval_value], all_data[40:(interval_value*2)], all_data[(interval_value*2+1):(interval_value*3)], 
                 all_data[(interval_value*3+1):(interval_value*4)], all_data[(interval_value*4+1):(interval_value*5)], 
                 all_data[(interval_value*5+1):(interval_value*6)], all_data[(interval_value*6+1):(interval_value*7)], 
                 all_data[(interval_value*7+1):(interval_value*8)], all_data[(interval_value*8+1):(interval_value*9)], 
                 all_data[(interval_value*9+1):(interval_value*10)], all_data[(interval_value*10+1):(interval_value*11)], 
                 all_data[(interval_value*11+1):(interval_value*12)])
total_data_mean = round(colMeans(all_data),1)
total_data_sum = colSums(all_data)
total_data_sd = round(colSds(all_data),1)
total_summary = data.frame(cbind(c("mean", "sum", "sd"), rbind(total_data_mean, total_data_sum, total_data_sd)))
colnames(total_summary) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

all_data = data.frame(c(colnames(max_HI_within_cells)[4:42]), all_data)
colnames(all_data) = c("county", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]

all_data = data.frame(rbind(all_data, total_summary))
write.csv(all_data, file = "max_HI_89.csv")

