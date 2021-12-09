
# This code is to calculate 
# minimum relative humidity using max temperature and max vapor pressure deficit
# maximum relative humidity using min temperature and min vapor pressure deficit
# More details on the calculation can be found https://prism.oregonstate.edu/documents/pubs/2015plosone_humidityMapping_daly.pdf
# If you have any questions on the code, please send an email to Jihoon Jung (climategeo@gmail.com) or John C Flunker (jflunker@uw.edu)

#################################################################

# To calculate minimum relative humidity using maximum temperature and maximum vapor pressure deficit
# based on centroid method

#################################################################

rm(list = ls())

setwd("") # add path to your directory where you saved data sets in the code "1_extract.R"

load("max_temp_centroid.Rdata") # add path to your directory
Tmax = extract_temp_all[,-c(1,2,3)]

load("max_vpd_centroid.Rdata") # add path to your directory
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
RH_min_centroid = (100 * (SATVP_a - vpd_max)) / SATVP_a

RH_min_centroid[RH_min_centroid < 0] = 0 # to remove values under 0

save(RH_min_centroid, file = "RH_min_centroid.Rdata")


#################################################################

# To calculate minimum relative humidity using maximum temperature and maximum vapor pressure deficit
# based on the average of cells within polygons

#################################################################

rm(list = ls())

setwd("") # add path to your directory where you saved data sets in the code "1_extract.R"

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

#################################################################

# To calculate maximum relative humidity using minimum temperature and minimum vapor pressure deficit
# based on centroid method

#################################################################

rm(list = ls())

setwd("") # add path to your directory where you saved data sets in the code "1_extract.R"

load("min_temp_centroid.Rdata") # add path to your directory
Tmin = extract_temp_all[,-c(1,2,3)]

load("min_vpd_centroid.Rdata") # add path to your directory
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
RH_max_centroid = (100 * (SATVP_a - vpd_min)) / SATVP_a

RH_max_centroid[RH_max_centroid < 0] = 0 # to remove values under 0

save(RH_max_centroid, file = "RH_max_centroid.Rdata")

#################################################################

# To calculate maximum relative humidity using minimum temperature and minimum vapor pressure deficit
# based on the average of cells within polygons

#################################################################

rm(list = ls())

setwd("") # add path to your directory where you saved data sets in the code "1_extract.R"

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

#################################################
