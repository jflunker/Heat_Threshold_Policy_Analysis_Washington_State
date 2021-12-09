
# This code is to extract PRISM weather data using either centroids or average of cells within polygons
# more detailed information about PRISM data can be found https://prism.oregonstate.edu/
# This code has largely two parts 
# 1) data extraction based on centroids 
#    -> Each county has its own centroid and the raster value with the centrorid is extracted
# 2) data extraction based on the average of cells within polygons
#    -> Each county (polygon) has multiple pixels within the polygon, this method takes average of all the pixels

# To run this code, you first need to have your research area shapefile and PRISM data downloaded from the website above. 
# you may want to change data period by changing "start_year" and "end_year" in the code
# Most shapefiles can be found https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
# If you have any questions on the code, please send an email to Jihoon Jung (climategeo@gmail.com) or John C Flunker (jflunker@uw.edu)

rm(list = ls())

library(rgdal)
library(raster)
library(rgeos)

#################################################################

# 1. data extraction based on centroids
#   1.1 centroid extraction
#   1.2 data extraction : Maximum temperature
#   1.2 data extraction : maximum vapor pressure deficit
#   1.2 data extraction : mean temperature
#   1.2 data extraction : minimum temperature
#   1.2 data extraction : minimum vapor pressure deficit

#################################################################

# 1.1 calculate each polygon's centroid and save that csv file in your folder

poly = readOGR("Washington_state.shp") # add path to your directory
trueCentroids = gCentroid(poly,byid=TRUE) 
my_centroid = data.frame(poly$NAME, trueCentroids@coords)
colnames(my_centroid) = c("county_name", "lon", "lat")
write.csv(my_centroid, "my_centroid.csv") # add path to your directory where you want to save your file

# 1.2 data extraction : Maximum temperature

rm(list = ls())

start_year = 2002
end_year = 2020
extract_temp_all = {}
for (overall_year_data in start_year:end_year) {
  temp_address = paste("/PRISM_tmax_stable_4kmD2_", overall_year_data, "0101_", overall_year_data, "1231_bil", sep = "") # add path to your directory
  setwd(temp_address)
  temp_files_to_read = list.files(path = ".", pattern = "_bil.bil")
  temp_files_to_read = temp_files_to_read[seq(1,length(temp_files_to_read),2)]
  csv_file_data = read.csv("my_centroid.csv") # add path to your directory
  coordinates(csv_file_data)= ~ centroidx + centroidy
  for (i in 1:length(temp_files_to_read)){
    raster_data <- raster(temp_files_to_read[i])
    extract_temp = round(extract(raster_data, csv_file_data),1)
    year_data = substr(temp_files_to_read[i], 25,28)
    month_data = substr(temp_files_to_read[i], 29,30)
    day_data = substr(temp_files_to_read[i], 31,32)
    temp_re = c(year_data, month_data, day_data, extract_temp)
    extract_temp_all = data.frame(rbind(extract_temp_all, temp_re))
  }
}

csv_file_data = read.csv("my_centroid.csv") # add path to your directory
colnames(extract_temp_all) = c("Year", "Month", "Day", csv_file_data$NAME)

setwd("") # add path to your directory where you want to save your file
save(extract_temp_all, file = "max_temp_centroid.Rdata")

##################################################

# 1.2 data extraction : maximum vapor pressure deficit

rm(list = ls())

start_year = 2002
end_year = 2020
extract_temp_all = {}
for (overall_year_data in start_year:end_year) {
  temp_address = paste("/PRISM_vpdmax_stable_4kmD2_", overall_year_data, "0101_", overall_year_data, "1231_bil", sep = "") # add path to your directory
  setwd(temp_address)
  temp_files_to_read = list.files(path = ".", pattern = "_bil.bil")
  temp_files_to_read = temp_files_to_read[seq(1,length(temp_files_to_read),2)]
  csv_file_data = read.csv("my_centroid.csv") # add path to your directory
  coordinates(csv_file_data)= ~ centroidx + centroidy
  for (i in 1:length(temp_files_to_read)){
    raster_data <- raster(temp_files_to_read[i])
    extract_temp = round(extract(raster_data, csv_file_data),1)
    year_data = substr(temp_files_to_read[i], 27,30)
    month_data = substr(temp_files_to_read[i], 31,32)
    day_data = substr(temp_files_to_read[i], 33,34)
    temp_re = c(year_data, month_data, day_data, extract_temp)
    extract_temp_all = data.frame(rbind(extract_temp_all, temp_re))
  }
}

csv_file_data = read.csv("my_centroid.csv") # add path to your directory
colnames(extract_temp_all) = c("Year", "Month", "Day", csv_file_data$NAME)

setwd("") # add path to your directory where you want to save your file
save(extract_temp_all, file = "max_vpd_centroid.Rdata")

##################################################

# 1.2 data extraction : mean temperature

rm(list = ls())

start_year = 2002
end_year = 2020
extract_temp_all = {}
for (overall_year_data in start_year:end_year) {
  temp_address = paste("PRISM_tmean_stable_4kmD2_", overall_year_data, "0101_", overall_year_data, "1231_bil", sep = "") # add path to your directory
  setwd(temp_address)
  temp_files_to_read = list.files(path = ".", pattern = "_bil.bil")
  temp_files_to_read = temp_files_to_read[seq(1,length(temp_files_to_read),2)]
  csv_file_data = read.csv("my_centroid.csv") # add path to your directory
  coordinates(csv_file_data)= ~ centroidx + centroidy
  for (i in 1:length(temp_files_to_read)){
    raster_data <- raster(temp_files_to_read[i])
    extract_temp = round(extract(raster_data, csv_file_data),1)
    year_data = substr(temp_files_to_read[i], 26,29)
    month_data = substr(temp_files_to_read[i], 30,31)
    day_data = substr(temp_files_to_read[i], 32,33)
    temp_re = c(year_data, month_data, day_data, extract_temp)
    extract_temp_all = data.frame(rbind(extract_temp_all, temp_re))
  }
}

csv_file_data = read.csv("my_centroid.csv") # add path to your directory
colnames(extract_temp_all) = c("Year", "Month", "Day", csv_file_data$NAME)

setwd("") # add path to your directory where you want to save your file
save(extract_temp_all, file = "mean_temp_centroid.Rdata")

##################################################

# 1.2 data extraction : minimum temperature

rm(list = ls())

start_year = 2002
end_year = 2020
extract_temp_all = {}
for (overall_year_data in start_year:end_year) {
  temp_address = paste("PRISM_tmin_stable_4kmD2_", overall_year_data, "0101_", overall_year_data, "1231_bil", sep = "") # add path to your directory
  setwd(temp_address)
  temp_files_to_read = list.files(path = ".", pattern = "_bil.bil")
  temp_files_to_read = temp_files_to_read[seq(1,length(temp_files_to_read),2)]
  csv_file_data = read.csv("my_centroid.csv") # add path to your directory
  coordinates(csv_file_data)= ~ centroidx + centroidy
  for (i in 1:length(temp_files_to_read)){
    raster_data <- raster(temp_files_to_read[i])
    extract_temp = round(extract(raster_data, csv_file_data),1)
    year_data = substr(temp_files_to_read[i], 25,28)
    month_data = substr(temp_files_to_read[i], 29,30)
    day_data = substr(temp_files_to_read[i], 31,32)
    temp_re = c(year_data, month_data, day_data, extract_temp)
    extract_temp_all = data.frame(rbind(extract_temp_all, temp_re))
  }
}

csv_file_data = read.csv("my_centroid.csv") # add path to your directory
colnames(extract_temp_all) = c("Year", "Month", "Day", csv_file_data$NAME)

setwd("") # add path to your directory where you want to save your file
save(extract_temp_all, file = "min_temp_centroid.Rdata")

##################################################

# 1.2 data extraction : minimum vapor pressure deficit

rm(list = ls())

start_year = 2002
end_year = 2020
extract_temp_all = {}
for (overall_year_data in start_year:end_year) {
  temp_address = paste("PRISM_vpdmin_stable_4kmD2_", overall_year_data, "0101_", overall_year_data, "1231_bil", sep = "") # add path to your directory
  setwd(temp_address)
  temp_files_to_read = list.files(path = ".", pattern = "_bil.bil")
  temp_files_to_read = temp_files_to_read[seq(1,length(temp_files_to_read),2)]
  csv_file_data = read.csv("my_centroid.csv") # add path to your directory
  coordinates(csv_file_data)= ~ centroidx + centroidy
  for (i in 1:length(temp_files_to_read)){
    raster_data <- raster(temp_files_to_read[i])
    extract_temp = round(extract(raster_data, csv_file_data),1)
    year_data = substr(temp_files_to_read[i], 27,30)
    month_data = substr(temp_files_to_read[i], 31,32)
    day_data = substr(temp_files_to_read[i], 33,34)
    temp_re = c(year_data, month_data, day_data, extract_temp)
    extract_temp_all = data.frame(rbind(extract_temp_all, temp_re))
  }
}

csv_file_data = read.csv("my_centroid.csv") # add path to your directory
colnames(extract_temp_all) = c("Year", "Month", "Day", csv_file_data$NAME)

setwd("") # add path to your directory where you want to save your file
save(extract_temp_all, file = "min_vpd_centroid.Rdata")


#################################################################

# 2) data extraction based on the average of cells within polygons
#   2.1 data extraction : Maximum temperature
#   2.1 data extraction : maximum vapor pressure deficit
#   2.1 data extraction : mean temperature
#   2.1 data extraction : minimum temperature
#   2.1 data extraction : minimum vapor pressure deficit

#################################################################

# 2.1 data extraction : Maximum temperature

rm(list = ls())

start_year = 2002
end_year = 2020
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

#################################################

# 2.1 data extraction : maximum vapor pressure deficit

rm(list = ls())

start_year = 2002
end_year = 2020
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

poly = readOGR("Washington_state.shp") # add path to your directory
colnames(extract_temp_all) = c("Year", "Month", "Day", poly$NAME)

setwd("") # add path to your directory where you want to save your file
save(extract_temp_all, file = "max_vpd_within_cells.Rdata")

#################################################

#   2.1 data extraction : mean temperature

rm(list = ls())

start_year = 2002
end_year = 2020
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

#################################################

#   2.1 data extraction : minimum temperature

rm(list = ls())

start_year = 2002
end_year = 2020
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

#################################################

#   2.1 data extraction : minimum vapor pressure deficit

rm(list = ls())

start_year = 2002
end_year = 2020
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



