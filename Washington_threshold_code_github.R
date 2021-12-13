


# This code provides visualizations and tables to examine variability in meteorological data and heat threshold exceedances throughout the state of Washington at the county level.
# The data acquisition and code execution process is approximately five steps, each of which are marked in the below navigation bar (1: data extraction; 2:generate monthly county-level summary tables; 3:generate threshold days per month summary tables; 4: generate average number of days above thresholds at the county-level summary tables; and 5:generate county-level exceedance maps.
# As an initial step prior to executing the below code, download the PRISM weather data of interest. For the below code the following daily data for the years of 2002-2020 is required: maximum temperature, maximum vapor pressure deficit, mean temperature, minimum temperature, and minimum vapor pressure deficit.
# As an additional preliminary step, one must also have the Washington shape files (Washington_state.shx, .sbx, .sbn, .prg, .dbf, and .CPG files) in working directory to generate state maps with county borders.
# Issues with mac error when attempting to load the 'raster' package (Error = xcrun: error: invalid active developer path (/Library/Developer/CommandLineTools), missing xcrun at: /Library/Developer/CommandLineTools/usr/bin/xcrun.) can be corrected by executing the following command in the R terminal: xcode-select --install


# Begin code below







# NOTE: one may skip section 1 of this code (extract data from PRISM .csv files) code if using the .Rdata files (3_within_R_data_files_PRISM_Summary folder on jflunker GitHub repo) with pre-extracted PRISM data based on the below extraction method
# if skipping data extraction from PRISM and using the provided .Rdata, start code at section two (~line 596), Generate monthly_county_level_summary_tables 2002-2020"

######### extract data from PRISM .csv files -----------------------------------------------------------------
#SECTION 1

# This section of code is to extract PRISM weather data from yearly/monthly/hourly modeled weather data using the average of cells within polygons. PRISM data must de downloaded prior to executing section 1.
# More detailed information about PRISM data can be found at https://prism.oregonstate.edu/ (opyright ©<year>, PRISM Climate Group, Oregon State University, https://prism.oregonstate.edu)

# FIRST, download daily hourly weather data from PRISM, covering the years of 2002 to 2020 (https://prism.oregonstate.edu/recent/).
# Data is extraction based on the average of cells within polygons
# Each county (polygon) has multiple pixels within the polygon, this method takes average of all the pixels (referred to as "within cells"). Spactial resolution is 4x4 KM. 
# If interested in PRISM weather data outside of 2002-2020, you may want to change data period by changing "start_year" and "end_year" in the code to match years of interest/PRISM data.

# To run the first section of code (extraction), one must also download the Washington state research area shapefiles (from the below noted jflunker github repo or https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html) and PRISM weather data (https://prism.oregonstate.edu/). 
# For ease of use the Washington state shape files can be downloaded from this GitHub repository: "https://github.com/jflunker/Heat_Threshold_Policy_Analysis_Washington_State"
# If interested in downloading from the source, most state shapefiles can be found at https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html




# data extraction based on the average of cells within polygons will perform 5 data extraction steps
#   1 data extraction : Maximum temperature
#   2 data extraction : maximum vapor pressure deficit
#   3 data extraction : mean temperature
#   4 data extraction : minimum temperature
#   5 data extraction : minimum vapor pressure deficit

# 1 data extraction : Maximum temperature

rm(list = ls())

library(rgdal)
library(raster)
library(rgeos)

getwd()
setwd("") # insert your file working directory where you saved prism data sets

start_year = 2002 # change years depending on your research period
end_year = 2020 # change years depending on your research period
extract_temp_all = {}
for (overall_year_data in start_year:end_year) {
  temp_address = paste("PRISM_tmax_stable_4kmD2_", overall_year_data, "0101_", overall_year_data, "1231_bil", sep = "") # add path to your directory
  setwd(temp_address)
  temp_files_to_read = list.files(path = ".", pattern = "_bil.bil")
  temp_files_to_read = temp_files_to_read[seq(1,length(temp_files_to_read),2)]
  poly = readOGR("Washington_state.shp") # add path to your directory
  for (i in 1:length(temp_files_to_read)){
    raster_data = raster(temp_files_to_read[i])
    extract_temp = exact_extract(raster_data, poly, 'mean')
    year_data = substr(temp_files_to_read[i], 25,28)
    month_data = substr(temp_files_to_read[i], 29,30)
    day_data = substr(temp_files_to_read[i], 31,32)
    temp_re = c(year_data, month_data, day_data, extract_temp)
    extract_temp_all = data.frame(rbind(extract_temp_all, temp_re))
  }
}

poly = readOGR("Washington_state.shp") # add path to your directory
colnames(extract_temp_all) = c("Year", "Month", "Day", poly$NAME)

setwd("") # add path to your directory where you want to save your file
save(extract_temp_all, file = "max_temp_within_cells.Rdata")


# 2 data extraction : maximum vapor pressure deficit

rm(list = ls())

setwd() # insert your file working directory where you saved prism data sets

start_year = 2002 # change years depending on your research period
end_year = 2020 # change years depending on your research period
extract_temp_all = {}
for (overall_year_data in start_year:end_year) {
  temp_address = paste("PRISM_vpdmax_stable_4kmD2_", overall_year_data, "0101_", overall_year_data, "1231_bil", sep = "") # add path to your directory
  setwd(temp_address)
  temp_files_to_read = list.files(path = ".", pattern = "_bil.bil")
  temp_files_to_read = temp_files_to_read[seq(1,length(temp_files_to_read),2)]
  poly = readOGR("Washington_state.shp") # add path to your directory
  for (i in 1:length(temp_files_to_read)){
    raster_data <- raster(temp_files_to_read[i])
    extract_temp = exact_extract(raster_data, poly, 'mean')
    year_data = substr(temp_files_to_read[i], 27,30)
    month_data = substr(temp_files_to_read[i], 31,32)
    day_data = substr(temp_files_to_read[i], 33,34)
    temp_re = c(year_data, month_data, day_data, extract_temp)
    extract_temp_all = data.frame(rbind(extract_temp_all, temp_re))
  }
}

poly = readOGR("Washington_state.shp") # insert your file working directory where you saved prism data sets
 # add path to your directory
colnames(extract_temp_all) = c("Year", "Month", "Day", poly$NAME)

setwd("") # add path to your directory where you want to save your file
save(extract_temp_all, file = "max_vpd_within_cells.Rdata")


#   3 data extraction : mean temperature

rm(list = ls())

setwd() # insert your file working directory where you saved prism data sets

start_year = 2002 # change years depending on your research period
end_year = 2020 # change years depending on your research period
extract_temp_all = {}
for (overall_year_data in start_year:end_year) {
  temp_address = paste("PRISM_tmean_stable_4kmD2_", overall_year_data, "0101_", overall_year_data, "1231_bil", sep = "") # add path to your directory
  setwd(temp_address)
  temp_files_to_read = list.files(path = ".", pattern = "_bil.bil")
  temp_files_to_read = temp_files_to_read[seq(1,length(temp_files_to_read),2)]
  poly = readOGR("Washington_state.shp") # add path to your directory
  for (i in 1:length(temp_files_to_read)){
    raster_data <- raster(temp_files_to_read[i])
    extract_temp = exact_extract(raster_data, poly, 'mean')
    year_data = substr(temp_files_to_read[i], 26,29)
    month_data = substr(temp_files_to_read[i], 30,31)
    day_data = substr(temp_files_to_read[i], 32,33)
    temp_re = c(year_data, month_data, day_data, extract_temp)
    extract_temp_all = data.frame(rbind(extract_temp_all, temp_re))
  }
}

poly = readOGR("Washington_state.shp") # add path to your directory
colnames(extract_temp_all) = c("Year", "Month", "Day", poly$NAME)

setwd("") # add path to your directory where you want to save your file
save(extract_temp_all, file = "mean_temp_within_cells.Rdata")


# 4 data extraction : minimum temperature

rm(list = ls())

setwd() # insert your file working directory where you saved prism data sets

start_year = 2002 # change years depending on your research period
end_year = 2020 # change years depending on your research period
extract_temp_all = {}
for (overall_year_data in start_year:end_year) {
  temp_address = paste("PRISM_tmin_stable_4kmD2_", overall_year_data, "0101_", overall_year_data, "1231_bil", sep = "") # add path to your directory
  setwd(temp_address)
  temp_files_to_read = list.files(path = ".", pattern = "_bil.bil")
  temp_files_to_read = temp_files_to_read[seq(1,length(temp_files_to_read),2)]
  poly = readOGR("Washington_state.shp") # add path to your directory
  for (i in 1:length(temp_files_to_read)){
    raster_data = raster(temp_files_to_read[i])
    extract_temp = exact_extract(raster_data, poly, 'mean')
    year_data = substr(temp_files_to_read[i], 25,28)
    month_data = substr(temp_files_to_read[i], 29,30)
    day_data = substr(temp_files_to_read[i], 31,32)
    temp_re = c(year_data, month_data, day_data, extract_temp)
    extract_temp_all = data.frame(rbind(extract_temp_all, temp_re))
  }
}

poly = readOGR("Washington_state.shp") # add path to your directory
colnames(extract_temp_all) = c("Year", "Month", "Day", poly$NAME)

setwd("") # add path to your directory where you want to save your file
save(extract_temp_all, file = "min_temp_within_cells.Rdata")


# 5 data extraction : minimum vapor pressure deficit

rm(list = ls())

setwd() # insert your file working directory where you saved prism data sets

start_year = 2002 # change years depending on your research period
end_year = 2020 # change years depending on your research period
extract_temp_all = {}
for (overall_year_data in start_year:end_year) {
  temp_address = paste("PRISM_vpdmin_stable_4kmD2_", overall_year_data, "0101_", overall_year_data, "1231_bil", sep = "") # add path to your directory
  setwd(temp_address)
  temp_files_to_read = list.files(path = ".", pattern = "_bil.bil")
  temp_files_to_read = temp_files_to_read[seq(1,length(temp_files_to_read),2)]
  poly = readOGR("Washington_state.shp") # add path to your directory
  for (i in 1:length(temp_files_to_read)){
    raster_data <- raster(temp_files_to_read[i])
    extract_temp = exact_extract(raster_data, poly, 'mean')
    year_data = substr(temp_files_to_read[i], 27,30)
    month_data = substr(temp_files_to_read[i], 31,32)
    day_data = substr(temp_files_to_read[i], 33,34)
    temp_re = c(year_data, month_data, day_data, extract_temp)
    extract_temp_all = data.frame(rbind(extract_temp_all, temp_re))
  }
}

poly = readOGR("Washington_state.shp") # add path to your directory
colnames(extract_temp_all) = c("Year", "Month", "Day", poly$NAME)

setwd("") # add path to your directory where you want to save your file
save(extract_temp_all, file = "min_vpd_within_cells.Rdata")




# relative_humidity_calculation ----------------------------------------------------------

# This part is to calculate 
# minimum relative humidity using max temperature and max vapor pressure deficit
# maximum relative humidity using min temperature and min vapor pressure deficit
# More details on the RH calculation in lines 36-38 can be found https://prism.oregonstate.edu/documents/pubs/2015plosone_humidityMapping_daly.pdf



# 1. To calculate minimum relative humidity using maximum temperature and maximum vapor pressure deficit based on the average of cells within polygons

rm(list = ls())

setwd("") # add path to your directory where you saved "max_temp_within_cells.Rdata" and "max_vpd_within_cells.Rdata" 

load("max_temp_within_cells.Rdata") # add path to your directory
Tmax = extract_temp_all[,-c(1,2,3)]
load("max_vpd_within_cells.Rdata") # add path to your directory
VPDmax = extract_temp_all[,-c(1,2,3)]

for (i in 1:length(Tmax)){
  Tmax[,i] = round(as.numeric(Tmax[,i]),1)
}
for (i in 1:length(VPDmax)){
  VPDmax[,i] = round(as.numeric(VPDmax[,i]),1)
}

T_max = Tmax
vpd_max = VPDmax
part_eq = (17.625 * T_max) / (243.04 + T_max)
SATVP_a = 6.1094 * exp(part_eq)
RH_min_within_cells = (100 * (SATVP_a - vpd_max)) / SATVP_a

RH_min_within_cells[RH_min_within_cells < 0] = 0 # to remove values under 0
save(RH_min_within_cells, file = "RH_min_within_cells.Rdata")


# To calculate maximum relative humidity using minimum temperature and minimum vapor pressure deficit based on the average of cells within polygons

rm(list = ls())

setwd("") # add path to your directory where you saved "min_temp_within_cells.Rdata" and "min_vpd_within_cells.Rdata"

load("min_temp_within_cells.Rdata") # add path to your directory
Tmin = extract_temp_all[,-c(1,2,3)]
load("min_vpd_within_cells.Rdata") # add path to your directory
VPDmin = extract_temp_all[,-c(1,2,3)]

for (i in 1:length(Tmin)){
  Tmin[,i] = round(as.numeric(Tmin[,i]),1)
}
for (i in 1:length(VPDmin)){
  VPDmin[,i] = round(as.numeric(VPDmin[,i]),1)
}

T_min = Tmin
vpd_min = VPDmin
part_eq = (17.625 * T_min) / (243.04 + T_min)
SATVP_a = 6.1094 * exp(part_eq)
RH_max_within_cells = (100 * (SATVP_a - vpd_min)) / SATVP_a

RH_max_within_cells[RH_max_within_cells < 0] = 0 # to remove values under 0
save(RH_max_within_cells, file = "RH_max_within_cells.Rdata")









########## HI_calculation ----------------------------------------------------------

# This part is to calculate 
# maximum heat index using maximum temperature and extracted minimum relative humidity
# minimum heat index using minimum temperature and extracted maximum relative humidity
# mean heat index using mean temperature and average of maximum RH and minimum RH
# more details on HI calculation can be found https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml



# 1. To calculate maximum heat index using maximum temperature and extracted minimum relative humidity based on the average of cells within polygons

rm(list = ls())

setwd("") # add path to your directory where you saved "max_temp_within_cells.Rdata" and "RH_min_within_cells.Rdata"

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


# 2. To calculate minimum heat index using minimum temperature and extracted maximum relative humidity based on the average of cells within polygons

rm(list = ls())

setwd("") # add path to your directory where you saved "min_temp_within_cells.Rdata" and "RH_max_within_cells.Rdata"

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


# 3. To calculate mean heat index using mean temperature and average of maximum RH and minimum RH based on the average of cells within polygons

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


# to add year/month/day/county name

rm(list = ls())

#load("max_HI_centroid.Rdata")  # add path to your directory
load("max_HI_within_cells.Rdata")  # add path to your directory
#load("mean_HI_centroid.Rdata")  # add path to your directory
load("mean_HI_within_cells.Rdata")  # add path to your directory
#load("min_HI_centroid.Rdata")  # add path to your directory
load("min_HI_within_cells.Rdata")  # add path to your directory

#max_HI_centroid = data.frame(max_HI_centroid)
max_HI_within_cells = data.frame(max_HI_within_cells)
#mean_HI_centroid = data.frame(mean_HI_centroid)
mean_HI_within_cells = data.frame(mean_HI_within_cells)
#min_HI_centroid = data.frame(min_HI_centroid)
min_HI_within_cells = data.frame(min_HI_within_cells)

load("max_temp_within_cells.Rdata")  # add path to your directory
date_data = extract_temp_all[,1:3]
date_data[,1] = as.numeric(date_data[,1])
date_data[,2] = as.numeric(date_data[,2])
date_data[,3] = as.numeric(date_data[,3])

#max_HI_centroid = data.frame(cbind(date_data, max_HI_centroid))
max_HI_within_cells = data.frame(cbind(date_data, max_HI_within_cells))
#mean_HI_centroid = data.frame(cbind(date_data, mean_HI_centroid))
mean_HI_within_cells = data.frame(cbind(date_data, mean_HI_within_cells))
#min_HI_centroid = data.frame(cbind(date_data, min_HI_centroid))
min_HI_within_cells = data.frame(cbind(date_data, min_HI_within_cells))

#csv_file_data = read.csv("my_centroid.csv") # add path to your directory
#colnames(max_HI_centroid) = c("Year", "Month", "Day", csv_file_data$NAME)
#colnames(mean_HI_centroid) = c("Year", "Month", "Day", csv_file_data$NAME)
#colnames(min_HI_centroid) = c("Year", "Month", "Day", csv_file_data$NAME)

poly = readOGR("Washington_state.shp") # add path to your directory
colnames(max_HI_within_cells) = c("Year", "Month", "Day", poly$NAME)
colnames(mean_HI_within_cells) = c("Year", "Month", "Day", poly$NAME)
colnames(min_HI_within_cells) = c("Year", "Month", "Day", poly$NAME)

#save(max_HI_centroid, file = "max_HI_centroid.Rdata")
save(max_HI_within_cells, file = "max_HI_within_cells.Rdata")
#save(mean_HI_centroid, file = "mean_HI_centroid.Rdata")
save(mean_HI_within_cells, file = "mean_HI_within_cells.Rdata")
#save(min_HI_centroid, file = "min_HI_centroid.Rdata")
save(min_HI_within_cells, file = "min_HI_within_cells.Rdata")








########## Generate monthly_county_level_summary_tables 2002-2020 -----------------------------------------------------------
#SECTION 2

# This section of code generates monthly counts of max, mean, and min temperatures and heat indexs at the county level



rm(list = ls())

library(matrixStats) 

# 1. County level dew point mean (SD) summary tables

load("mean_dpt_within_cells.Rdata") # add path to your directory where you saved "mean_dpt_within_cells.Rdata"

# to convert to numeric values
for (i in 1:length(extract_temp_all)){
  extract_temp_all[,i] = as.numeric(extract_temp_all[,i])
}

# Celsius to Fahrenheit
for (i in 4:length(extract_temp_all)){
  extract_temp_all[,i] = (9/5) * extract_temp_all[,i] + 32
}

# to calculate monthly counts at the county level
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

# to calculate table summary statistics (bottom rows of table)
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
colnames(all_data) = c("County", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "Average_SD_dew_point_per_county_per_month_summary_2002_2020.csv")


# 2. County level max temp mean (SD) summary tables

rm(list = ls())

load("max_temp_within_cells.Rdata") # add path to your directory where you saved "max_temp_within_cells.Rdata"

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

# to calculate total state average summary statistics (bottom rows of table)
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
colnames(all_data) = c("County", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "Average_SD_max_temperature_per_county_per_month_summary_2002_2020.csv")


# 3. County level mean (SD) daily temp summary tables

rm(list = ls())

load("mean_temp_within_cells.Rdata") # add path to your directory where you saved "mean_temp_within_cells.Rdata"

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
colnames(all_data) = c("County", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "Average_mean_daily_temperature_per_county_per_month_summary_2002_2020.csv")


# 4. County level Min temp mean (SD) summary tables

rm(list = ls())

load("min_temp_within_cells.Rdata") # add path to your directory where you saved "min_temp_within_cells.Rdata"

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
colnames(all_data) = c("County", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "Average_minimum_temperature_per_county_per_month_summary_2002_2020.csv")


# 5. County level max heat index mean (SD) summary tables

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory where you saved "max_HI_within_cells.Rdata"

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
colnames(all_data) = c("County", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "Average_SD_Maximum_heat_index_per_county_per_month_summary_2002_2020.csv")


# 6. County level mean (SD) heat index summary tables

rm(list = ls())

load("mean_HI_within_cells.Rdata") # add path to your directory where you saved "mean_HI_within_cells.Rdata"

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
colnames(all_data) = c("County", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "Average_SD_heat_index_per_county_per_month_summary_2002_2020.csv")


# 7. County level min heat index, mean (SD), summary tables

rm(list = ls())

load("min_HI_within_cells.Rdata") # add path to your directory where you saved "min_HI_within_cells.Rdata"

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
colnames(all_data) = c("County", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
all_data = all_data[order(all_data$county),]
all_data = rbind(all_data, c("Total", total_summary_data))

write.csv(all_data, file = "Average_SD_minimum_heat_index_per_county_per_month_summary_2002_2020.csv")









########## threshold_total_days_monthly_county_level_summary_tables ----------------------------------------------------------
#SECTION 3

# Total number of days over multiple heat thresholds at the monthly and county level from 2002-2020


library(matrixStats)

# ≥ 80 maximum temperature, total number of days per month, county-level, 2002-2020

rm(list = ls())

load("max_temp_within_cells.Rdata") # add path to your directory where you saved "max_temp_within_cells.Rdata"

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
#####table(extract_temp_all$Stevens, extract_temp_all$Month)

# to calculate monthly totals at the county level from 2002-2020
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
write.csv(all_data, file = "max_temp_80_or_grtr_total_exceedances_per_month_summed_over_2002_2020.csv")


# ≥85 maximum temperature, total number of days per month, county-level, 2002-2020

rm(list = ls())

load("max_temp_within_cells.Rdata") # add path to your directory where you saved "max_temp_within_cells.Rdata"

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

# to calculate monthly totals at the county level
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
write.csv(all_data, file = "max_temp_85_or_grtr_total_exceedances_per_month_summed_over_2002_2020.csv")


# ≥ 89 maximum temperature, total number of days per month, county-level, 2002-2020

rm(list = ls())

load("max_temp_within_cells.Rdata") # add path to your directory where you saved "max_temp_within_cells.Rdata"

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

# to calculate monthly summary of total over 2002-2020) at the county level
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
write.csv(all_data, file = "max_temp_89_or_grtr_total_exceedances_per_month_summed_over_2002_2020.csv")


# ≥ 95 maximum temperature, total number of days per month, county-level, 2002-2020

rm(list = ls())

load("max_temp_within_cells.Rdata") # add path to your directory where you saved "max_temp_within_cells.Rdata"

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
write.csv(all_data, file = "max_temp_89_or_grtr_total_exceedances_per_month_summed_over_2002_2020.csv")


# ≥ 80 heat index max per day, total number of days per month, county-level, 2002-2020

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory where you saved "max_HI_within_cells.Rdata"

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
write.csv(all_data, file = "max_HI_80_or_grtr_total_exceedances_per_month_summed_over_2002_2020.csv")


# ≥ 85 maximum heat index per day, total number of days per month, county-level, 2002-2020

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory where you saved "max_HI_within_cells.Rdata"

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
write.csv(all_data, file = "max_HI_85_or_grtr_total_exceedances_per_month_summed_over_2002_2020.csv")


# ≥ 89 maximum heat index per day

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory where you saved "max_HI_within_cells.Rdata"

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
write.csv(all_data, file = "max_HI_89_or_grtr_total_exceedances_per_month_summed_over_2002_2020.csv")


# ≥ 95 maximum heat index per day, total number of days per month, county-level, 2002-2020

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory where you saved "max_HI_within_cells.Rdata"

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

# to calculate monthly summary at the county level
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
write.csv(all_data, file = "max_HI_95_or_grtr_total_exceedances_per_month_summed_over_2002_2020.csv")









######### threshold_mean_sd_days_monthly_county_level_summary_tables ---------------
# SECTION 4

# This section of code is designed to calculate the average (sd) for the number of days per month the temp is ≥ thresholds 



library(matrixStats)
library(rgdal)

# ≥ 80 maximum temperature, average (SD) number of days per month, county-level, 2002-2020

rm(list = ls())

load("max_temp_within_cells.Rdata") # add path to your directory where you saved "max_temp_within_cells.Rdata"

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
start_year = 2002
end_year = 2020
all_data = {}
for (month_data in 12:1){
  temp_data1 = extract_temp_all[which(extract_temp_all$Month == month_data),]
  for (year_data in start_year:end_year) {
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

write.csv(final_data, file = "avg_num_days_per_month_max_temp_grtr_eq_80_mean_sd_per_county_2002_2020.csv")


# ≥ 85 maximum temperature, average (SD) number of days per month, per county, 2002-2020

rm(list = ls())

load("max_temp_within_cells.Rdata") # add path to your directory where you saved "max_temp_within_cells.Rdata"

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
start_year = 2002
end_year = 2020
all_data = {}
for (month_data in 12:1){
  temp_data1 = extract_temp_all[which(extract_temp_all$Month == month_data),]
  for (year_data in start_year:end_year) {
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

write.csv(final_data, file = "avg_num_days_per_month_max_temp_85_mean_sd_2002_2020.csv")


# ≥ 85 maximum temperature, average (SD) number of days per month, 2002-2020

rm(list = ls())

load("max_temp_within_cells.Rdata") # add path to your directory where you saved "max_temp_within_cells.Rdata"

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
start_year = 2002
end_year = 2020
all_data = {}
for (month_data in 12:1){
  temp_data1 = extract_temp_all[which(extract_temp_all$Month == month_data),]
  for (year_data in start_year:end_year) {
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

write.csv(final_data, file = "avg_num_days_per_month_max_temp_89_mean_sd_2002_2020.csv")


# ≥ 95 maximum temperature, average (SD) number of days per month, 2002-2020

rm(list = ls())

load("max_temp_within_cells.Rdata") # add path to your directory where you saved "max_temp_within_cells.Rdata"

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

# >= 95 temp, average number of days per month per county, 2002-2020
start_year = 2002
end_year = 2020
all_data = {}
for (month_data in 12:1){
  temp_data1 = extract_temp_all[which(extract_temp_all$Month == month_data),]
  for (year_data in start_year:end_year) {
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

write.csv(final_data,file = "avg_num_days_per_month_max_temp_95_mean_sd_2002_2020.csv")


#  ≥ 80 max heat index, average (SD) number of days per year within month, 2002-2020

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory where you saved "max_HI_within_cells.Rdata"

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
start_year = 2002
end_year = 2020
all_data = {}
for (month_data in 12:1){
  temp_data1 = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  for (year_data in start_year:end_year) {
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
  }
}

all_data = data.frame(all_data)
poly = readOGR("Washington_state.shp") # add path to your directory 
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

write.csv(final_data,file = "avg_num_days_per_month_max_HI_80_mean_sd_2002_2020.csv")


# ≥ 85 max heat index, average (SD) number of days per year within month, 2002-2020

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory where you saved "max_HI_within_cells.Rdata"

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
start_year = 2002
end_year = 2020
all_data = {}
for (month_data in 12:1){
  temp_data1 = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  for (year_data in start_year:end_year) {
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
  }
}

all_data = data.frame(all_data)
poly = readOGR("Washington_state.shp") # add path to your directory
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

write.csv(final_data, file = "avg_num_days_per_month_max_HI_85_mean_sd_2002_2020.csv")


# ≥ 89 max heat index, average (SD) number of days per year within month, 2002-2020

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory where you saved "max_HI_within_cells.Rdata"

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
start_year = 2002
end_year = 2020
all_data = {}
for (month_data in 12:1){
  temp_data1 = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  for (year_data in start_year:end_year) {
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
  }
}

all_data = data.frame(all_data)
poly = readOGR("Washington_state.shp") # add path to your directory
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

write.csv(final_data, file = "avg_num_days_per_month_max_HI_89_mean_sd_2002_2020.csv")


# ≥ 95 max heat index, average (SD) number of days per year within month, 2002-2020

rm(list = ls())

load("max_HI_within_cells.Rdata") # add path to your directory where you saved "max_HI_within_cells.Rdata"

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

#  >= 95 HI, average (SD) number of days per year within month, 2002-2020
start_year = 2002
end_year = 2020
all_data = {}
for (month_data in 12:1){
  temp_data1 = max_HI_within_cells[which(max_HI_within_cells$Month == month_data),]
  for (year_data in start_year:end_year) {
    temp_data2 = temp_data1[which(temp_data1$Year == year_data),]
    temp_data_sum = colSums(temp_data2)
    part_data = c(year_data, month_data, unlist(data.frame(temp_data_sum)[4:42,]))
    all_data = rbind(all_data, part_data)
  }
}

all_data = data.frame(all_data)
poly = readOGR("Washington_state.shp") # add path to your directory
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

write.csv(final_data,file = "avg_num_days_per_month_max_HI_95_mean_sd_2002_2020.csv")









######### Create county_level_maps with exceedance values --------------------------------------------------------
# SECTION 5

# This part is to make maps showing the total number of days exceeding HI thresholds (80,85,89,95) and maximum temperatures (80,85,89,95) during warm period (May-Sep) and outside of the period (Jan-Apr, Oct-Dec)
# For ease of use the Washington state shape files can be downloaded from this GitHub repository: "https://github.com/jflunker/Heat_Threshold_Policy_Analysis_Washington_State"
# If interested in downloading from the source, most state shapefiles can be found at https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

# 1. to make maps showing the total number of days over HI thresholds (80,85,89,95) during warm period (May-Sep) and outside of the period (Jan-Apr, Oct-Dec) based on the average of cells within polygons
# run this code in terminal if error found on mac with raster: xcode-select --install
getwd()

rm(list = ls())

library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(raster)
library(sp)
library(gridExtra)

load("max_HI_within_cells.Rdata") # add path to your directory where you saved "max_HI_within_cells.Rdata"

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

# to merge the extracted data(i.e. all_max_HI) with shapefiles for mapping

load("max_temp_within_cells.Rdata") # add path to your directory
name_data = names(extract_temp_all)[-c(1,2,3)]
all_max_HI = data.frame(NAME = names(extract_temp_all)[-c(1,2,3)], all_max_HI)

poly = shapefile("") # add path to your directory
poly_re = fortify(poly, region = 'NAME')

all_max_HI_poly <- merge(poly_re, all_max_HI, by.x='id', by.y='NAME')

csv_file_data = read.csv("my_within_cells.csv") # add path to your directory
csv_file_data_m = merge(csv_file_data, all_max_HI, by = "NAME")

# warm period

plot1 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_HI_80_re_may_sep), 
               data=all_max_HI_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max HI ≥80 from May through September") +
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
  ggtitle("Total number of days exceeding max HI ≥85 from May through September") +
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
  ggtitle("Total number of days exceeding max HI ≥89 from May through September") +
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
  ggtitle("Total number of days exceeding max HI ≥95 from May through September") +
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

# non-warm period

plot1 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_HI_80_re_others), 
               data=all_max_HI_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max HI ≥80 outside May-Sept") +
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
  ggtitle("Total number of days exceeding max HI ≥85 outside May-Sept") +
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
  ggtitle("Total number of days exceeding max HI ≥89 outside May-Sept") +
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
  ggtitle("Total number of days exceeding max HI ≥95 outside May-Sept") +
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


# 2. to make maps showing the total number of days over maximum temperature thresholds (80,85,89,95) during warm period (May-Sep) and outside of the period (Jan-Apr, Oct-Dec) based on the average of cells within polygons

rm(list = ls())

load("max_temp_within_cells.Rdata") # add path to your directory where you saved "max_temp_within_cells.Rdata"

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

# to merge the extracted data(i.e. all_max_HI) with shapefiles for mapping

load("max_temp_within_cells.Rdata") # add path to your directory

name_data = names(extract_temp_all)[-c(1,2,3)]
all_max_HI = data.frame(NAME = names(extract_temp_all)[-c(1,2,3)], all_max_HI)

poly = shapefile("Washington_state.shp") # add path to your directory
poly_re = fortify(poly, region = 'NAME')

all_max_temp_poly <- merge(poly_re, all_max_HI, by.x='id', by.y='NAME')

csv_file_data = read.csv("my_centroid.csv") # add path to your directory
csv_file_data_m = merge(csv_file_data, all_max_HI, by = "NAME")

# warm period

plot1 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_temp_80_re_may_sep), 
               data=all_max_temp_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max temperature ≥80 from May through September") +
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
  ggtitle("Total number of days exceeding max temperature ≥85 from May through September") +
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
  ggtitle("Total number of days exceeding max temperature ≥89 from May through September") +
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
  ggtitle("Total number of days exceeding max temperature ≥95 from May through September") +
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

# non-warm period

plot1 = ggplot() + 
  geom_polygon(aes(x=long,y=lat, group=group, fill=max_temp_80_re_others), 
               data=all_max_temp_poly, color='black') + scale_fill_gradient(low='white', high='red') +
  ggtitle("Total number of days exceeding max temperature ≥80 outside May-Sept") +
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
  ggtitle("Total number of days exceeding max temperature ≥85 outside May-Sept") +
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
  ggtitle("Total number of days exceeding max temperature ≥89 outside May-Sept") +
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
  ggtitle("Total number of days exceeding max temperature ≥95 outside May-Sept") +
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


#END CODE
#Updated 12_11_21 by jcf
#Bar graph section eliminated


