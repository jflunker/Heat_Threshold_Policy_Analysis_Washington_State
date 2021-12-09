
# This code is to calculate 
# maximum heat index using maximum temperature and extracted minimum relative humidity
# minimum heat index using minimum temperature and extracted maximum relative humidity
# mean heat index using mean temperature and average of maximum RH and minimum RH
# more details on HI calculation can be found https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
# If you have any questions on the code, please send an email to Jihoon Jung (climategeo@gmail.com) or John C Flunker (jflunker@uw.edu)

#################################################################

# To calculate maximum heat index using maximum temperature and extracted minimum relative humidity
# based on centroid method

#################################################################

rm(list = ls())

setwd("") # add path to your directory where you saved data sets 

load("max_temp_centroid.Rdata")
Tmax = extract_temp_all[,-c(1,2,3)]

for (i in 1:length(Tmax)){
  Tmax[,i] = round(as.numeric(Tmax[,i]),1)
}

load("RH_min_centroid.Rdata")

my_temperature = Tmax
my_temperature = (9/5) * my_temperature + 32  # Celsius to Fahrenheit
my_temperature_c = unlist(my_temperature)

my_relative_humidity = RH_min_centroid
my_relative_humidity_c = unlist(my_relative_humidity)

# HI calculation

# CASE 1: if a heat index value is below about 80 degrees F

my_HI = 0.5*(my_temperature_c + 61 + ((my_temperature_c-68)*1.2) + (my_relative_humidity_c*0.094))

# CASE 2: if a heat index value is above about 80 degrees F

first_option_data = my_HI[which(my_HI > 80)]
first_option_temperature = my_temperature_c[which(my_HI > 80)]
first_option_rh = my_relative_humidity_c[which(my_HI > 80)]

first_option__HI = (- 42.379 + 2.04901523 * first_option_temperature + 10.14333127 * first_option_rh - 
                      0.22475541 * first_option_temperature * first_option_rh - 0.00683783 * first_option_temperature * first_option_temperature -
                      0.05481717 * first_option_rh * first_option_rh + 0.00122874 * first_option_temperature * first_option_temperature * first_option_rh +
                      0.00085282 * first_option_rh * first_option_temperature * first_option_rh - 
                      0.00000199 * first_option_temperature * first_option_temperature * first_option_rh * first_option_rh)

my_HI[which(my_HI > 80)] = first_option__HI

# CASE 3: If the RH is less than 13% and the temperature is between 80 and 112 degrees F

second_option_temp_loc = which(my_temperature_c > 80 & my_temperature_c < 112)
second_option_rh_loc = which(my_relative_humidity_c < 13)
intersect_location = intersect(second_option_temp_loc, second_option_rh_loc)

if(length(intersect_location) != 0){
  
  second_option_temperature = my_temperature_c[intersect_location]
  second_option_rh = my_relative_humidity_c[intersect_location]
  second_option_hi = my_HI[intersect_location]
  second_option_HI = second_option_hi - ((13-second_option_rh)/4)*(((17 - abs(second_option_temperature-95))/17)**0.5)
  
  my_HI[intersect_location] = second_option_HI
  
} 

# CASE 4: if the RH is greater than 85% and the temperature is between 80 and 87 degrees F

third_option_temp_loc = which(my_temperature_c > 80 & my_temperature_c < 87)
third_option_rh_loc = which(my_relative_humidity_c > 85)
intersect_location = intersect(third_option_temp_loc, third_option_rh_loc)

if(length(intersect_location) != 0){
  
  third_option_temperature = my_temperature_c[intersect_location]
  third_option_rh = my_relative_humidity_c[intersect_location]
  third_option_hi = my_HI[intersect_location]
  third_option_HI = third_option_hi + ((third_option_rh-85)/10)*(87-third_option_temperature)/5
  
  my_HI[intersect_location] = third_option_HI
  
} 

max_HI_centroid = matrix(my_HI, nrow = nrow(Tmax), ncol = ncol(Tmax))
save(max_HI_centroid, file = "max_HI_centroid.Rdata")


#################################################################

# To calculate maximum heat index using maximum temperature and extracted minimum relative humidity
# based on the average of cells within polygons

#################################################################

rm(list = ls())

setwd("") # add path to your directory where you saved data sets 

load("max_temp_within_cells.Rdata")
Tmax = extract_temp_all[,-c(1,2,3)]

for (i in 1:length(Tmax)){
  Tmax[,i] = round(as.numeric(Tmax[,i]),1)
}

load("RH_min_within_cells.Rdata")

my_temperature = Tmax
my_temperature = (9/5) * my_temperature + 32  # Celsius to Fahrenheit
my_temperature_c = unlist(my_temperature)

my_relative_humidity = RH_min_within_cells
my_relative_humidity_c = unlist(my_relative_humidity)

# HI calculation

# CASE 1: if a heat index value is below about 80 degrees F

my_HI = 0.5*(my_temperature_c + 61 + ((my_temperature_c-68)*1.2) + (my_relative_humidity_c*0.094))

# CASE 2: if a heat index value is above about 80 degrees F

first_option_data = my_HI[which(my_HI > 80)]
first_option_temperature = my_temperature_c[which(my_HI > 80)]
first_option_rh = my_relative_humidity_c[which(my_HI > 80)]

first_option__HI = (- 42.379 + 2.04901523 * first_option_temperature + 10.14333127 * first_option_rh - 
                      0.22475541 * first_option_temperature * first_option_rh - 0.00683783 * first_option_temperature * first_option_temperature -
                      0.05481717 * first_option_rh * first_option_rh + 0.00122874 * first_option_temperature * first_option_temperature * first_option_rh +
                      0.00085282 * first_option_rh * first_option_temperature * first_option_rh - 
                      0.00000199 * first_option_temperature * first_option_temperature * first_option_rh * first_option_rh)

my_HI[which(my_HI > 80)] = first_option__HI

# CASE 3: If the RH is less than 13% and the temperature is between 80 and 112 degrees F

second_option_temp_loc = which(my_temperature_c > 80 & my_temperature_c < 112)
second_option_rh_loc = which(my_relative_humidity_c < 13)
intersect_location = intersect(second_option_temp_loc, second_option_rh_loc)

if(length(intersect_location) != 0){
  
  second_option_temperature = my_temperature_c[intersect_location]
  second_option_rh = my_relative_humidity_c[intersect_location]
  second_option_hi = my_HI[intersect_location]
  second_option_HI = second_option_hi - ((13-second_option_rh)/4)*(((17 - abs(second_option_temperature-95))/17)**0.5)
  
  my_HI[intersect_location] = second_option_HI
  
} 

# CASE 4: if the RH is greater than 85% and the temperature is between 80 and 87 degrees F

third_option_temp_loc = which(my_temperature_c > 80 & my_temperature_c < 87)
third_option_rh_loc = which(my_relative_humidity_c > 85)
intersect_location = intersect(third_option_temp_loc, third_option_rh_loc)

if(length(intersect_location) != 0){
  
  third_option_temperature = my_temperature_c[intersect_location]
  third_option_rh = my_relative_humidity_c[intersect_location]
  third_option_hi = my_HI[intersect_location]
  third_option_HI = third_option_hi + ((third_option_rh-85)/10)*(87-third_option_temperature)/5
  
  my_HI[intersect_location] = third_option_HI
  
} 

max_HI_within_cells = matrix(my_HI, nrow = nrow(Tmax), ncol = ncol(Tmax))
save(max_HI_within_cells, file = "max_HI_within_cells.Rdata")


#################################################################

# To calculate minimum heat index using minimum temperature and extracted maximum relative humidity
# based on centroid method

#################################################################

rm(list = ls())

setwd("") # add path to your directory where you saved data sets 

load("min_temp_centroid.Rdata")
Tmin = extract_temp_all[,-c(1,2,3)]

for (i in 1:length(Tmin)){
  Tmin[,i] = round(as.numeric(Tmin[,i]),1)
}

load("RH_max_centroid.Rdata")

my_temperature = Tmin
my_temperature = (9/5) * my_temperature + 32  # Celsius to Fahrenheit
my_temperature_c = unlist(my_temperature)

my_relative_humidity = RH_max_centroid
my_relative_humidity_c = unlist(my_relative_humidity)

# HI calculation

# CASE 1: if a heat index value is below about 80 degrees F

my_HI = 0.5*(my_temperature_c + 61 + ((my_temperature_c-68)*1.2) + (my_relative_humidity_c*0.094))

# CASE 2: if a heat index value is above about 80 degrees F

first_option_data = my_HI[which(my_HI > 80)]
first_option_temperature = my_temperature_c[which(my_HI > 80)]
first_option_rh = my_relative_humidity_c[which(my_HI > 80)]

first_option__HI = (- 42.379 + 2.04901523 * first_option_temperature + 10.14333127 * first_option_rh - 
                      0.22475541 * first_option_temperature * first_option_rh - 0.00683783 * first_option_temperature * first_option_temperature -
                      0.05481717 * first_option_rh * first_option_rh + 0.00122874 * first_option_temperature * first_option_temperature * first_option_rh +
                      0.00085282 * first_option_rh * first_option_temperature * first_option_rh - 
                      0.00000199 * first_option_temperature * first_option_temperature * first_option_rh * first_option_rh)

my_HI[which(my_HI > 80)] = first_option__HI

# CASE 3: If the RH is less than 13% and the temperature is between 80 and 112 degrees F

second_option_temp_loc = which(my_temperature_c > 80 & my_temperature_c < 112)
second_option_rh_loc = which(my_relative_humidity_c < 13)
intersect_location = intersect(second_option_temp_loc, second_option_rh_loc)

if(length(intersect_location) != 0){
  
  second_option_temperature = my_temperature_c[intersect_location]
  second_option_rh = my_relative_humidity_c[intersect_location]
  second_option_hi = my_HI[intersect_location]
  second_option_HI = second_option_hi - ((13-second_option_rh)/4)*(((17 - abs(second_option_temperature-95))/17)**0.5)
  
  my_HI[intersect_location] = second_option_HI
  
} 

# CASE 4: if the RH is greater than 85% and the temperature is between 80 and 87 degrees F

third_option_temp_loc = which(my_temperature_c > 80 & my_temperature_c < 87)
third_option_rh_loc = which(my_relative_humidity_c > 85)
intersect_location = intersect(third_option_temp_loc, third_option_rh_loc)

if(length(intersect_location) != 0){
  
  third_option_temperature = my_temperature_c[intersect_location]
  third_option_rh = my_relative_humidity_c[intersect_location]
  third_option_hi = my_HI[intersect_location]
  third_option_HI = third_option_hi + ((third_option_rh-85)/10)*(87-third_option_temperature)/5
  
  my_HI[intersect_location] = third_option_HI
  
} 

min_HI_centroid = matrix(my_HI, nrow = nrow(Tmix), ncol = ncol(Tmix))
save(min_HI_centroid, file = "min_HI_centroid.Rdata")


#################################################################

# To calculate minimum heat index using minimum temperature and extracted maximum relative humidity
# based on the average of cells within polygons

#################################################################

rm(list = ls())

setwd("") # add path to your directory where you saved data sets 

load("min_temp_within_cells.Rdata")
Tmin = extract_temp_all[,-c(1,2,3)]

for (i in 1:length(Tmin)){
  Tmin[,i] = round(as.numeric(Tmin[,i]),1)
}

load("RH_max_within_cells.Rdata")

my_temperature = Tmin
my_temperature = (9/5) * my_temperature + 32  # Celsius to Fahrenheit
my_temperature_c = unlist(my_temperature)

my_relative_humidity = RH_max_within_cells
my_relative_humidity_c = unlist(my_relative_humidity)

# HI calculation

# CASE 1: if a heat index value is below about 80 degrees F

my_HI = 0.5*(my_temperature_c + 61 + ((my_temperature_c-68)*1.2) + (my_relative_humidity_c*0.094))

# CASE 2: if a heat index value is above about 80 degrees F

first_option_data = my_HI[which(my_HI > 80)]
first_option_temperature = my_temperature_c[which(my_HI > 80)]
first_option_rh = my_relative_humidity_c[which(my_HI > 80)]

first_option__HI = (- 42.379 + 2.04901523 * first_option_temperature + 10.14333127 * first_option_rh - 
                      0.22475541 * first_option_temperature * first_option_rh - 0.00683783 * first_option_temperature * first_option_temperature -
                      0.05481717 * first_option_rh * first_option_rh + 0.00122874 * first_option_temperature * first_option_temperature * first_option_rh +
                      0.00085282 * first_option_rh * first_option_temperature * first_option_rh - 
                      0.00000199 * first_option_temperature * first_option_temperature * first_option_rh * first_option_rh)

my_HI[which(my_HI > 80)] = first_option__HI

# CASE 3: If the RH is less than 13% and the temperature is between 80 and 112 degrees F

second_option_temp_loc = which(my_temperature_c > 80 & my_temperature_c < 112)
second_option_rh_loc = which(my_relative_humidity_c < 13)
intersect_location = intersect(second_option_temp_loc, second_option_rh_loc)

if(length(intersect_location) != 0){
  
  second_option_temperature = my_temperature_c[intersect_location]
  second_option_rh = my_relative_humidity_c[intersect_location]
  second_option_hi = my_HI[intersect_location]
  second_option_HI = second_option_hi - ((13-second_option_rh)/4)*(((17 - abs(second_option_temperature-95))/17)**0.5)
  
  my_HI[intersect_location] = second_option_HI
  
} 

# CASE 4: if the RH is greater than 85% and the temperature is between 80 and 87 degrees F

third_option_temp_loc = which(my_temperature_c > 80 & my_temperature_c < 87)
third_option_rh_loc = which(my_relative_humidity_c > 85)
intersect_location = intersect(third_option_temp_loc, third_option_rh_loc)

if(length(intersect_location) != 0){
  
  third_option_temperature = my_temperature_c[intersect_location]
  third_option_rh = my_relative_humidity_c[intersect_location]
  third_option_hi = my_HI[intersect_location]
  third_option_HI = third_option_hi + ((third_option_rh-85)/10)*(87-third_option_temperature)/5
  
  my_HI[intersect_location] = third_option_HI
  
} 

min_HI_within_cells = matrix(my_HI, nrow = nrow(Tmix), ncol = ncol(Tmix))
save(min_HI_within_cells, file = "min_HI_within_cells.Rdata")


#################################################################

# To calculate mean heat index using mean temperature and average of maximum RH and minimum RH
# based on centroid method

#################################################################

rm(list = ls())

setwd("") # add path to your directory where you saved data sets 

load("mean_temp_centroid.Rdata")
Tmean = extract_temp_all[,-c(1,2,3)]

for (i in 1:length(Tmean)){
  Tmean[,i] = round(as.numeric(Tmean[,i]),1)
}

load("RH_min_centroid.Rdata")
load("RH_max_centroid.Rdata")

my_temperature = Tmean
my_temperature = (9/5) * my_temperature + 32  # Celsius to Fahrenheit
my_temperature_c = unlist(my_temperature)

my_relative_humidity = (RH_min_centroid + RH_max_centroid) / 2
my_relative_humidity_c = unlist(my_relative_humidity)

# HI calculation

# CASE 1: if a heat index value is below about 80 degrees F

my_HI = 0.5*(my_temperature_c + 61 + ((my_temperature_c-68)*1.2) + (my_relative_humidity_c*0.094))

# CASE 2: if a heat index value is above about 80 degrees F

first_option_data = my_HI[which(my_HI > 80)]
first_option_temperature = my_temperature_c[which(my_HI > 80)]
first_option_rh = my_relative_humidity_c[which(my_HI > 80)]

first_option__HI = (- 42.379 + 2.04901523 * first_option_temperature + 10.14333127 * first_option_rh - 
                      0.22475541 * first_option_temperature * first_option_rh - 0.00683783 * first_option_temperature * first_option_temperature -
                      0.05481717 * first_option_rh * first_option_rh + 0.00122874 * first_option_temperature * first_option_temperature * first_option_rh +
                      0.00085282 * first_option_rh * first_option_temperature * first_option_rh - 
                      0.00000199 * first_option_temperature * first_option_temperature * first_option_rh * first_option_rh)

my_HI[which(my_HI > 80)] = first_option__HI

# CASE 3: If the RH is less than 13% and the temperature is between 80 and 112 degrees F

second_option_temp_loc = which(my_temperature_c > 80 & my_temperature_c < 112)
second_option_rh_loc = which(my_relative_humidity_c < 13)
intersect_location = intersect(second_option_temp_loc, second_option_rh_loc)

if(length(intersect_location) != 0){
  
  second_option_temperature = my_temperature_c[intersect_location]
  second_option_rh = my_relative_humidity_c[intersect_location]
  second_option_hi = my_HI[intersect_location]
  second_option_HI = second_option_hi - ((13-second_option_rh)/4)*(((17 - abs(second_option_temperature-95))/17)**0.5)
  
  my_HI[intersect_location] = second_option_HI
  
} 

# CASE 4: if the RH is greater than 85% and the temperature is between 80 and 87 degrees F

third_option_temp_loc = which(my_temperature_c > 80 & my_temperature_c < 87)
third_option_rh_loc = which(my_relative_humidity_c > 85)
intersect_location = intersect(third_option_temp_loc, third_option_rh_loc)

if(length(intersect_location) != 0){
  
  third_option_temperature = my_temperature_c[intersect_location]
  third_option_rh = my_relative_humidity_c[intersect_location]
  third_option_hi = my_HI[intersect_location]
  third_option_HI = third_option_hi + ((third_option_rh-85)/10)*(87-third_option_temperature)/5
  
  my_HI[intersect_location] = third_option_HI
  
} 

mean_HI_centroid = matrix(my_HI, nrow = nrow(Tmean), ncol = ncol(Tmean))
save(mean_HI_centroid, file = "mean_HI_centroid.Rdata")


#################################################################

# To calculate mean heat index using mean temperature and average of maximum RH and minimum RH
# based on the average of cells within polygons

#################################################################

rm(list = ls())

setwd("") # add path to your directory where you saved data sets 

load("mean_temp_within_cells.Rdata")
Tmean = extract_temp_all[,-c(1,2,3)]

for (i in 1:length(Tmean)){
  Tmean[,i] = round(as.numeric(Tmean[,i]),1)
}

load("RH_max_within_cells.Rdata")
load("RH_min_within_cells.Rdata")

my_temperature = Tmean
my_temperature = (9/5) * my_temperature + 32  # Celsius to Fahrenheit
my_temperature_c = unlist(my_temperature)

my_relative_humidity = (RH_max_within_cells + RH_min_within_cells) / 2
my_relative_humidity_c = unlist(my_relative_humidity)

# HI calculation

# CASE 1: if a heat index value is below about 80 degrees F

my_HI = 0.5*(my_temperature_c + 61 + ((my_temperature_c-68)*1.2) + (my_relative_humidity_c*0.094))

# CASE 2: if a heat index value is above about 80 degrees F

first_option_data = my_HI[which(my_HI > 80)]
first_option_temperature = my_temperature_c[which(my_HI > 80)]
first_option_rh = my_relative_humidity_c[which(my_HI > 80)]

first_option__HI = (- 42.379 + 2.04901523 * first_option_temperature + 10.14333127 * first_option_rh - 
                      0.22475541 * first_option_temperature * first_option_rh - 0.00683783 * first_option_temperature * first_option_temperature -
                      0.05481717 * first_option_rh * first_option_rh + 0.00122874 * first_option_temperature * first_option_temperature * first_option_rh +
                      0.00085282 * first_option_rh * first_option_temperature * first_option_rh - 
                      0.00000199 * first_option_temperature * first_option_temperature * first_option_rh * first_option_rh)

my_HI[which(my_HI > 80)] = first_option__HI

# CASE 3: If the RH is less than 13% and the temperature is between 80 and 112 degrees F

second_option_temp_loc = which(my_temperature_c > 80 & my_temperature_c < 112)
second_option_rh_loc = which(my_relative_humidity_c < 13)
intersect_location = intersect(second_option_temp_loc, second_option_rh_loc)

if(length(intersect_location) != 0){
  
  second_option_temperature = my_temperature_c[intersect_location]
  second_option_rh = my_relative_humidity_c[intersect_location]
  second_option_hi = my_HI[intersect_location]
  second_option_HI = second_option_hi - ((13-second_option_rh)/4)*(((17 - abs(second_option_temperature-95))/17)**0.5)
  
  my_HI[intersect_location] = second_option_HI
  
} 

# CASE 4: if the RH is greater than 85% and the temperature is between 80 and 87 degrees F

third_option_temp_loc = which(my_temperature_c > 80 & my_temperature_c < 87)
third_option_rh_loc = which(my_relative_humidity_c > 85)
intersect_location = intersect(third_option_temp_loc, third_option_rh_loc)

if(length(intersect_location) != 0){
  
  third_option_temperature = my_temperature_c[intersect_location]
  third_option_rh = my_relative_humidity_c[intersect_location]
  third_option_hi = my_HI[intersect_location]
  third_option_HI = third_option_hi + ((third_option_rh-85)/10)*(87-third_option_temperature)/5
  
  my_HI[intersect_location] = third_option_HI
  
} 

mean_HI_within_cells = matrix(my_HI, nrow = nrow(Tmean), ncol = ncol(Tmean))
save(mean_HI_within_cells, file = "mean_HI_within_cells.Rdata")


#################################################

# to add year/month/day/county name

rm(list = ls())

load("max_HI_centroid.Rdata")  # add path to your directory
load("max_HI_within_cells.Rdata")  # add path to your directory
load("mean_HI_centroid.Rdata")  # add path to your directory
load("mean_HI_within_cells.Rdata")  # add path to your directory
load("min_HI_centroid.Rdata")  # add path to your directory
load("min_HI_within_cells.Rdata")  # add path to your directory

max_HI_centroid = data.frame(max_HI_centroid)
max_HI_within_cells = data.frame(max_HI_within_cells)
mean_HI_centroid = data.frame(mean_HI_centroid)
mean_HI_within_cells = data.frame(mean_HI_within_cells)
min_HI_centroid = data.frame(min_HI_centroid)
min_HI_within_cells = data.frame(min_HI_within_cells)

load("max_temp_centroid.Rdata")  # add path to your directory
date_data = extract_temp_all[,1:3]
date_data[,1] = as.numeric(date_data[,1])
date_data[,2] = as.numeric(date_data[,2])
date_data[,3] = as.numeric(date_data[,3])

max_HI_centroid = data.frame(cbind(date_data, max_HI_centroid))
max_HI_within_cells = data.frame(cbind(date_data, max_HI_within_cells))
mean_HI_centroid = data.frame(cbind(date_data, mean_HI_centroid))
mean_HI_within_cells = data.frame(cbind(date_data, mean_HI_within_cells))
min_HI_centroid = data.frame(cbind(date_data, min_HI_centroid))
min_HI_within_cells = data.frame(cbind(date_data, min_HI_within_cells))

csv_file_data = read.csv("my_centroid.csv") # add path to your directory
colnames(max_HI_centroid) = c("Year", "Month", "Day", csv_file_data$NAME)
colnames(mean_HI_centroid) = c("Year", "Month", "Day", csv_file_data$NAME)
colnames(min_HI_centroid) = c("Year", "Month", "Day", csv_file_data$NAME)

poly = readOGR("Washington_state.shp") # add path to your directory
colnames(max_HI_within_cells) = c("Year", "Month", "Day", poly$NAME)
colnames(mean_HI_within_cells) = c("Year", "Month", "Day", poly$NAME)
colnames(min_HI_within_cells) = c("Year", "Month", "Day", poly$NAME)

save(max_HI_centroid, file = "max_HI_centroid.Rdata")
save(max_HI_within_cells, file = "max_HI_within_cells.Rdata")
save(mean_HI_centroid, file = "mean_HI_centroid.Rdata")
save(mean_HI_within_cells, file = "mean_HI_within_cells.Rdata")
save(min_HI_centroid, file = "min_HI_centroid.Rdata")
save(min_HI_within_cells, file = "min_HI_within_cells.Rdata")

