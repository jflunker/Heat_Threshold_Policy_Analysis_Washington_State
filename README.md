# Heat_Threshold_Policy_Analysis_Washington_State
# This repository is designed to provide reproducible code used to explore variability in PRISM modeled meteorological data and heat threshold exceedances at the county level throughout the state of Washington (PRISM Climate Group, Oregon State University, https://prism.oregonstate.edu).
# The R code file "Washington_threshold_code_github" will direct one through the data extraction and table/figure generating process. 
# All required PRISM weather data must be downloaded directy from the PRISM website https://prism.oregonstate.edu) following the directions in the document, PRISM_data.docx, which is found in this repository. Complete data data sets per year (2002-2020) will need to be download for each weather variable separately (19 years by 5 weather variables = 95 data sets). 
# The weather variables required for this code include: 
#   maximum temperature
#   maximum vapor pressure deficit
#   mean temperature
#   minimum temperature
#   minimum vapor pressure deficit
# Alternatively, the R data files used to generate tables and figures based on the PRISM data can be found in this respository in the folder "3_within_R_data_files_PRISM_Summary".
# All required shape files for generating WA state maps are contained in this repository ("county_shapefiles" folder), yet can also be downloaded directly from the source. See comments in each .R code file for how to download the data directly.
# Stade-wide visualizations of exceedance statistics at the county-level (generated from the R code in this respository) can be found in the folder "State_level_figures_exceedances".
# Please refer any questions related to the R code or potential uses of this code and data to jflunker@uw.edu or jjung5@uw.edu.
# Code was initially created by JJung and modefied by JFlunker.
