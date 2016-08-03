library(shiny) # http://shiny.rstudio.com

# for debugging:
testing <- FALSE 
# testing <- TRUE # comment out this line to stop debugging

if (testing) {  cat('Starting global\n') }
if (testing) {
  options(shiny.reactlog = TRUE) # If TRUE, enable logging of reactive events, which can be 
  # viewed later with the showReactLog function. This incurs a substantial performance penalty and should not be used in production.
  options(shiny.trace = TRUE)
  options(shiny.stacktraceoffset = TRUE) # name of function appears next to srcref where defined not where it is being called from - more intuitive view?
  options(shiny.fullstacktrace = TRUE) # instead of shorter prettier view
  options(shiny.error = browser) # or options(shiny.error = recover) gives debugger prompt on err
  options(error = recover) # The functions dump.frames and recover provide alternatives that allow post-mortem debugging. 
  options(verbose = TRUE) 
  options(shiny.deprecation.messages = TRUE)
} else {
  options(shiny.reactlog = FALSE) 
  options(shiny.trace = FALSE)
  options(shiny.stacktraceoffset = FALSE) # name of function appears next to srcref where defined not where it is being called from - more intuitive view?
  options(shiny.fullstacktrace = FALSE) # instead of shorter prettier view
  options(shiny.error = NULL) # or options(shiny.error = recover) gives debugger prompt on err
  options(error = NULL) # resets behavior to normal when error hit
  options(verbose = FALSE) 
  options(shiny.deprecation.messages = FALSE)
}
on.exit({
  options(shiny.reactlog = FALSE) 
  options(shiny.trace = FALSE)
  options(shiny.stacktraceoffset = FALSE) # name of function appears next to srcref where defined not where it is being called from - more intuitive view?
  options(shiny.fullstacktrace = FALSE) # instead of shorter prettier view
  options(shiny.error = NULL) # or options(shiny.error = recover) gives debugger prompt on err
  options(error = NULL) # resets behavior to normal when error hit
  options(verbose = FALSE) 
  options(shiny.deprecation.messages = FALSE)
  
})

####################################################################################
# DEFINE GLOBAL VARIABLES FOR USE IN server or ui - default values, etc.
####################################################################################

# **** MUST UPDATE ACS DATA IN CODE WHEN SWITCHING TO NEW ACS DATASET!

# ***  COULD SPECIFY LARGE FILE THAT IS OUTPUT OF BATCH RUN AND INPUT TO THIS SUMMARIZER: ***
#
# Could do so here if huge file too large for Shiny's file upload limit - 
# For large file, edit code and run a local copy of this summarizer. 
# 
suppressWarnings(rm(mydemofile)) # just in case testing and had already specified this
#
# To specify a local large custom file, can uncomment one of the following lines
# 
# mydemofile <- 'path/hugebatchoutput.csv' # for example
# mydemofile <- 'ejtest2.csv'
# 
# Default example of export of batch results, for use in testing/demonstrating summarizer:
#
if (!exists('mydemofile'))     {mydemofile     <- 'Export_Output_Example2.csv'} # should update to 2016 format
if (!exists('mydemofile.pop')) {mydemofile.pop <- 'Export_Output_Example2.pop.csv'} # not realistic - just smaller pop numbers


# Required format for input file (***for 2015 version):
#  ***  These seem to be all out of order ***:
batchfields2015 <- c(
  'OBJECTID', 'FACID', 'NAME', 'LAT', 'LON', 'totpop', 'buff', 'stabbr', 'statename', 'region', 
  'S_E_TSDF_PER', 'R_P_TRAFFIC', 'S_E_PM25_PER', 'R_P_CANCER', 'S_P_DIESEL', 'N_D_INDEX', 
  'RAW_E_RMP', 'R_E_PM25', 'R_D_LESSHS', 'R_E_DIESEL', 
  'RAW_D_OVER64', 'N_E_TSDF', 'R_E_LEAD_PER', 'R_E_RMP_PER', 'S_E_DIESEL_PER', 
  'RAW_E_RESP', 'R_D_INDEX_PER', 
  'RAW_D_LESSHS', 'N_E_TRAFFIC', 'S_E_NEURO_PER', 'N_P_NPL', 'S_D_INDEX', 'S_D_MINOR', 'S_D_LESSHS', 'S_P_RESP', 'N_E_PM25_PER', 
  'RAW_D_INDEX', 'N_E_NEURO_PER', 
  'RAW_D_UNDER5', 
  'RAW_E_LEAD', 'R_E_NPL_PER', 'S_E_RESP_PER', 'S_E_O3_PER', 'N_P_PM25', 'S_D_LESSHS_PER', 'N_E_DIESEL_PER', 'S_D_INCOME_PER', 
  'RAW_E_NPL', 'R_D_MINOR_PER', 'S_E_TRAFFIC', 'R_P_TSDF', 
  'RAW_E_TSDF', 'N_P_CANCER', 
  'RAW_E_NEURO', 'S_E_DIESEL', 
  'RAW_D_INCOME', 'N_P_RMP', 'N_E_O3_PER', 'S_E_O3', 'R_E_RESP', 'S_E_RESP', 'N_E_DIESEL', 'N_D_INDEX_PER', 'N_E_RMP_PER', 
  'RAW_D_MINOR', 'N_E_CANCER_PER', 'R_E_O3_PER', 'S_D_INDEX_PER', 'N_E_RMP', 'R_P_LEAD', 'R_E_NEURO', 'N_E_LEAD', 'S_E_RMP_PER', 'R_E_RMP', 
  'RAW_E_DIESEL', 'R_D_LING_PER', 'R_E_TRAFFIC', 'R_E_LEAD', 'R_D_OVER64_PER', 'N_P_NEURO', 'R_E_CANCER_PER', 'R_E_NPDES_PER', 'N_E_CANCER', 'N_D_MINOR_PER', 'S_E_TSDF', 'S_E_NPL', 'R_D_OVER64', 'S_D_MINOR_PER', 'S_P_TSDF', 'S_P_RMP', 'N_E_PM25', 'R_E_TSDF', 'S_E_RMP', 
  'RAW_D_LING', 'S_E_TRAFFIC_PER', 'S_P_PM25', 'S_E_LEAD', 'R_P_NEURO', 'S_D_LING', 'N_E_NPL', 'R_E_DIESEL_PER', 'R_D_LESSHS_PER', 'R_P_O3', 'N_E_TRAFFIC_PER', 
  'RAW_E_NPDES', 'N_E_NPDES', 'N_E_NEURO', 'R_P_DIESEL', 'N_E_RESP_PER', 'R_E_TSDF_PER', 
  'RAW_E_TRAFFIC', 'R_D_INDEX', 'R_P_PM25', 'N_D_UNDER5_PER', 'N_D_LESSHS_PER', 'R_E_NPDES', 'N_D_LING', 'S_E_PM25', 'N_E_NPL_PER', 'R_E_NEURO_PER', 'R_D_MINOR', 'N_P_TSDF', 'S_D_LING_PER', 'R_P_NPL', 'S_P_NPDES', 'S_E_NPDES_PER', 'N_D_UNDER5', 'S_E_NPL_PER', 'S_E_CANCER_PER', 'N_E_RESP', 'N_D_LESSHS', 'S_D_UNDER5', 'N_P_LEAD', 
  'RAW_E_CANCER', 'S_P_TRAFFIC', 'N_E_NPDES_PER', 'R_E_TRAFFIC_PER', 'N_P_NPDES', 
  'RAW_E_O3', 'N_P_O3', 'R_E_O3', 'N_E_O3', 'N_E_TSDF_PER', 'R_E_RESP_PER', 'S_D_OVER64', 'N_D_INCOME', 'R_E_NPL', 'R_D_UNDER5_PER', 'R_P_RESP', 'R_P_NPDES', 'S_P_O3', 'N_P_DIESEL', 'N_D_OVER64_PER', 'R_P_RMP', 'N_P_TRAFFIC', 'N_E_LEAD_PER', 'S_E_NPDES', 'S_D_OVER64_PER', 'S_P_NPL', 'N_D_MINOR', 
  'RAW_E_PM25', 'N_D_LING_PER', 'S_D_INCOME', 'S_P_NEURO', 'N_P_RESP', 'N_D_OVER64', 'S_D_UNDER5_PER', 'R_D_LING', 'R_E_CANCER', 'S_E_CANCER', 'S_P_CANCER', 'N_D_INCOME_PER', 'R_D_INCOME_PER', 'S_E_NEURO', 'R_D_INCOME', 'R_E_PM25_PER', 'R_D_UNDER5', 'S_E_LEAD_PER', 'S_P_LEAD'
)
# # OBJECTID,FACID,NAME,LAT,LON,totpop,buff,stabbr,statename,region,S_E_TSDF_PER,R_P_TRAFFIC,S_E_PM25_PER,R_P_CANCER,S_P_DIESEL,N_D_INDEX,RAW_E_RMP,R_E_PM25,R_D_LESSHS,R_E_DIESEL,RAW_D_OVER64,N_E_TSDF,R_E_LEAD_PER,R_E_RMP_PER,S_E_DIESEL_PER,RAW_E_RESP,R_D_INDEX_PER,RAW_D_LESSHS,N_E_TRAFFIC,S_E_NEURO_PER,N_P_NPL,S_D_INDEX,S_D_MINOR,S_D_LESSHS,S_P_RESP,N_E_PM25_PER,RAW_D_INDEX,N_E_NEURO_PER,RAW_D_UNDER5,RAW_E_LEAD,R_E_NPL_PER,S_E_RESP_PER,S_E_O3_PER,N_P_PM25,S_D_LESSHS_PER,N_E_DIESEL_PER,S_D_INCOME_PER,RAW_E_NPL,R_D_MINOR_PER,S_E_TRAFFIC,R_P_TSDF,RAW_E_TSDF,N_P_CANCER,RAW_E_NEURO,S_E_DIESEL,RAW_D_INCOME,N_P_RMP,N_E_O3_PER,S_E_O3,R_E_RESP,S_E_RESP,N_E_DIESEL,N_D_INDEX_PER,N_E_RMP_PER,RAW_D_MINOR,N_E_CANCER_PER,R_E_O3_PER,S_D_INDEX_PER,N_E_RMP,R_P_LEAD,R_E_NEURO,N_E_LEAD,S_E_RMP_PER,R_E_RMP,RAW_E_DIESEL,R_D_LING_PER,R_E_TRAFFIC,R_E_LEAD,R_D_OVER64_PER,N_P_NEURO,R_E_CANCER_PER,R_E_NPDES_PER,N_E_CANCER,N_D_MINOR_PER,S_E_TSDF,S_E_NPL,R_D_OVER64,S_D_MINOR_PER,S_P_TSDF,S_P_RMP,N_E_PM25,R_E_TSDF,S_E_RMP,RAW_D_LING,S_E_TRAFFIC_PER,S_P_PM25,S_E_LEAD,R_P_NEURO,S_D_LING,N_E_NPL,R_E_DIESEL_PER,R_D_LESSHS_PER,R_P_O3,N_E_TRAFFIC_PER,RAW_E_NPDES,N_E_NPDES,N_E_NEURO,R_P_DIESEL,N_E_RESP_PER,R_E_TSDF_PER,RAW_E_TRAFFIC,R_D_INDEX,R_P_PM25,N_D_UNDER5_PER,N_D_LESSHS_PER,R_E_NPDES,N_D_LING,S_E_PM25,N_E_NPL_PER,R_E_NEURO_PER,R_D_MINOR,N_P_TSDF,S_D_LING_PER,R_P_NPL,S_P_NPDES,S_E_NPDES_PER,N_D_UNDER5,S_E_NPL_PER,S_E_CANCER_PER,N_E_RESP,N_D_LESSHS,S_D_UNDER5,N_P_LEAD,RAW_E_CANCER,S_P_TRAFFIC,N_E_NPDES_PER,R_E_TRAFFIC_PER,N_P_NPDES,RAW_E_O3,N_P_O3,R_E_O3,N_E_O3,N_E_TSDF_PER,R_E_RESP_PER,S_D_OVER64,N_D_INCOME,R_E_NPL,R_D_UNDER5_PER,R_P_RESP,R_P_NPDES,S_P_O3,N_P_DIESEL,N_D_OVER64_PER,R_P_RMP,N_P_TRAFFIC,N_E_LEAD_PER,S_E_NPDES,S_D_OVER64_PER,S_P_NPL,N_D_MINOR,RAW_E_PM25,N_D_LING_PER,S_D_INCOME,S_P_NEURO,N_P_RESP,N_D_OVER64,S_D_UNDER5_PER,R_D_LING,R_E_CANCER,S_E_CANCER,S_P_CANCER,N_D_INCOME_PER,R_D_INCOME_PER,S_E_NEURO,R_D_INCOME,R_E_PM25_PER,R_D_UNDER5,S_E_LEAD_PER,S_P_LEAD
# # 1,1000,BRASW Facility,32.4,-94.7,"7,946",3 miles,TX, Texas,6,97,74,44,75,61,35%,1.2,9.44,19%,0.733,13%,0.054,74,92,37,1.5,72,23%,110,99,95,47%,55%,20%,66,27,61%,92,7%,0.27,97,54,51,77,64,49,70,0.29,69,91,96,0.33,80,0.12,0.913,52%,95,34,42.9,1.4,1.5,0.824,83,95,69%,72,47,68,0.31,84,0.043,0.3,91,0.42,0.478,66,81,0.18,63,90,87,73,49,79,0.073,0.067,11%,62,94,86,10.7,0.062,0.47,6%,60,61,0.17,92,9%,0.096,50,67,68,60,0.32,0.25,0.063,70,43,97,55,44%,68,62,78,0.35,5%,9.63,94,99,49%,97,56,95,74,71,7%,97,84,2.3,15%,8%,84,56,69,81,64,89,43.8,79,43.6,46.3,98,63,10%,34%,0.063,52,73,79,62,78,55,89,80,57,0.38,69,93,36%,9.44,74,39%,89,76,13%,49,7%,42,44,68,79,71,0.044,39%,47,7%,76,82

# Required format for input file (***for 2016 version):
# This is the output of the new faster batch tool with 2016 dataset:

batchfields2016 <- c(
  "ID","LAT","LONG",
  "POP100","mins","pctmin","lowinc","pctlowinc","lths","pctlths","lingiso","pctlingiso","under5","pctunder5","over64","pctover64",
  "traffic.score","pctpre1960","pm","o3","cancer","dpm","resp","neuro","proximity.tsdf","proximity.rmp","proximity.npl","proximity.npdes",
  "VSI.eo","VSI.svi6",
  
  "inedx_EJ_Traffic","inedx_EJ_Lead","inedx_EJ_PM","inedx_EJ_Ozone","inedx_EJ_Cancer","inedx_EJ_DPM","inedx_EJ_Resp","inedx_EJ_Neuro","inedx_EJ_proximity.tsdf","inedx_EJ_proximity.rmp","inedx_EJ_proximity.npl","inedx_EJ_proximity.npdes",
  
  "BLOCKID","Distance.x",    # *****???????
  
  "BLOCKGROUPFIPS","STUSAB","STATE","COUNTY","TRACT","BLKGRP","BLOCK","REGION",
  
  "N_D_INDEX_PER",
  "N_E_NPDES_PER","N_E_TSDF_PER","N_E_RMP_PER","N_E_NPL_PER","N_E_LEAD_PER","N_E_TRAFFIC_PER","N_E_RESP_PER","N_E_NEURO_PER","N_E_CANCER_PER","N_E_DIESEL_PER","N_E_O3_PER","N_E_PM25_PER",
  "N_D_MINOR_PER","N_D_INCOME_PER","N_D_LESSHS_PER","N_D_LING_PER","N_D_UNDER5_PER","N_D_OVER64_PER",
  "N_D_INDEX",
  "N_E_NPDES","N_E_TSDF","N_E_RMP","N_E_NPL","N_E_LEAD","N_E_TRAFFIC","N_E_RESP","N_E_NEURO","N_E_CANCER","N_E_DIESEL","N_E_O3","N_E_PM25",
  "N_D_MINOR","N_D_INCOME","N_D_LESSHS","N_D_LING","N_D_UNDER5","N_D_OVER64",
  
  "R_D_INDEX_PER",
  "R_E_NPDES_PER","R_E_TSDF_PER","R_E_RMP_PER","R_E_NPL_PER","R_E_LEAD_PER","R_E_TRAFFIC_PER","R_E_RESP_PER","R_E_NEURO_PER","R_E_CANCER_PER","R_E_DIESEL_PER","R_E_O3_PER","R_E_PM25_PER",
  "R_D_MINOR_PER","R_D_INCOME_PER","R_D_LESSHS_PER","R_D_LING_PER","R_D_UNDER5_PER","R_D_OVER64_PER",
  "R_D_INDEX",
  "R_E_NPDES","R_E_TSDF","R_E_RMP","R_E_NPL","R_E_LEAD","R_E_TRAFFIC","R_E_RESP","R_E_NEURO","R_E_CANCER","R_E_DIESEL","R_E_O3","R_E_PM25",
  "R_D_MINOR","R_D_INCOME","R_D_LESSHS","R_D_LING","R_D_UNDER5","R_D_OVER64",
  
  "S_D_INDEX_PER",
  "S_E_NPDES_PER","S_E_TSDF_PER","S_E_RMP_PER","S_E_NPL_PER","S_E_LEAD_PER","S_E_TRAFFIC_PER","S_E_RESP_PER","S_E_NEURO_PER","S_E_CANCER_PER","S_E_DIESEL_PER","S_E_O3_PER","S_E_PM25_PER",
  "S_D_MINOR_PER","S_D_INCOME_PER","S_D_LESSHS_PER","S_D_LING_PER","S_D_UNDER5_PER","S_D_OVER64_PER",
  "S_D_INDEX",
  "S_E_NPDES","S_E_TSDF","S_E_RMP","S_E_NPL","S_E_LEAD","S_E_TRAFFIC","S_E_RESP","S_E_NEURO","S_E_CANCER","S_E_DIESEL","S_E_O3","S_E_PM25",
  "S_D_MINOR","S_D_INCOME","S_D_LESSHS","S_D_LING","S_D_UNDER5","S_D_OVER64"
)

# ** Some or all of the map between versions of field names could be handled using the 
# ejscreen package now, via data('ejscreenformulas'), with e.g., 
# gdbfieldname      "CANCER"                
# Rfieldname        "cancer"                
# acsfieldname      NA                      
# type              "Environmental"         
# glossaryfieldname "Air toxics cancer risk"
#

# Default File that has default input and output and friendly fieldnames & var type & var category:
# mynamesfile.default <- 'map batch to friendly fieldnames 2015.csv' 
mynamesfile.default <- 'map batch to friendly fieldnames 2016.csv' 

# Specify units that go with each environmental indicator. This is also available via 
# data(popupunits, package='ejscreen') # except this package has to be obtained from github for that.
popupunits <- structure(
  list(
    evar = c("pm", "o3", "cancer", "neuro", "resp", "dpm", "pctpre1960", 
             "traffic.score", "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes"), 
    units = c("ug/m3", "ppb", "lifetime risk per million", "", "", "ug/m3", "= fraction pre-1960", 
              "daily vehicles/meters distance", "sites/km distance", "facilities/km distance", "facilities/km distance", "facilities/km distance")
  ), 
  .Names = c("evar", "units"), 
  row.names = c(NA, -12L), 
  class = "data.frame"
)

#############################################

#############################
# for interactive plots/charts/graphs
#############################

source("plotlyGraphWidget.R") 
# see https://plot.ly/r/getting-started/

#############################
# For leaflet maps, might need to define these here 
# until latest version of leaflet package is on cran:
#############################

leafletOutput = function(outputId, width = "100%", height = 400) {
  htmlwidgets::shinyWidgetOutput(outputId, "leaflet", width, height, "leaflet")
}

renderLeaflet = function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr = substitute(expr)  # force quoted
  htmlwidgets::shinyRenderWidget(expr, leafletOutput, env, quoted = TRUE)
}

#############################################

# Useful open map layers

# layer.admin <- 'OpenMapSurfer.AdminBounds'   # neighborhoods, counties, etc.
# layer.houses <- 'HERE.hybridDay'
# layer.street1 <- 'Esri.WorldStreetMap'
# layer.street2 <- 'OpenStreetMap.HOT'
# layer.street3 <- 'Stamen.TonerHybrid'
# layer.topo <- 'Esri.WorldTopoMap'
# layer.delorme <- 'Esri.DeLorme'
# layer.natgeo <- 'Esri.NatGeoWorldMap'
# layer.sat1 <- 'Esri.WorldImagery'
# layer.sat2 <- 'MapQuestOpen.Aerial'
# layer.esrigray <- 'Esri.WorldGrayCanvas'

mapserver1= 'http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer'
mapserver2= 'http://services.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer'

# var MapQuestOpen_Aerial = L.tileLayer('http://oatile{s}.mqcdn.com/tiles/1.0.0/sat/{z}/{x}/{y}.jpg', {
#   attribution: 'Tiles Courtesy of <a href="http://www.mapquest.com/">MapQuest</a> &mdash; Portions Courtesy NASA/JPL-Caltech and U.S. Depart. of Agriculture, Farm Service Agency',
#   subdomains: '1234'
# });

#############################
# DEFAULT VALUES - possibly could recode to allow user to change them
#############################

pixels.per.char <- 10 #????
max.allowed=30 # max length of site name we want displayed before wrapping in sites table
default.tab <- 'Upload'

#############################
# WHICH FIELDS TO COMPARE TO THRESHOLDS
#############################

# allow user to specify user-specified # of groups of user-specified fields to compare to user-specified thresholds.
# Initially, just the 3 thresholds can be altered, not which fields are compared or how many groups or what the groups are called.
threshold.default   <- list(50, 50, 50)  # a default for cutoff in at/above threshold stat summarizing EJ US percentiles
threshgroup.default <- list('EJ US pctiles', 'EJ Region pctiles', 'EJ State pctiles')
# NOTE:  server.R creates threshold names at the moment
# threshnames.default <- list( 
#   grep('^pctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) , 
#   grep('regionpctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) , 
#   grep('statepctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) 
# )

#############################
# FIELDNAMES USED BY CODE TO REFER TO SPECIFIC TYPES OF FIELDS
# THIS COULD BE REPLACED BY JUST REQUIRING THE RELEVANT GROUPS OF NAMES BE PROVIDED IN A PARTICULAR SORT ORDER IN THE newnames COLUMN OF THE LOOKUP TABLE (CSV READ IN AS mynamesfile.default or user-uploaded version of that)
# AS WRITTEN CURRENTLY, THE non-friendly versions of the NAMES BELOW *MUST* MATCH THOSE IN THE newnames COLUMN OF THE LOOKUP TABLE
# and the friendly versions are read from the csv file for table outputs but from the defaults below for use in the graphics (hist/bar) labeling I believe.
#############################

# Only some of these maps between versions of field names could be handled using the 
# ejscreen package now, via data('ejscreenformulas'), with e.g., 
# gdbfieldname      "CANCER"                
# Rfieldname        "cancer"                  *****
# acsfieldname      NA                      
# type              "Environmental"         
# glossaryfieldname "Air toxics cancer risk"  *****
# e.g., change.fieldnames(ejscreenformulas$)
# BUT, 
# Many fields used just for batch processing outputs are not in ejscreenformulas at all, such as 
#          oldnames                newnames gdbfieldname
# 1        OBJECTID                OBJECTID     OBJECTID
# ...
# 12    RAW_D_INDEX                  VSI.eo     VULEOPCT
# 13      N_D_INDEX           us.avg.VSI.eo         <NA>
# 14      R_D_INDEX       region.avg.VSI.eo         <NA>
# 15      S_D_INDEX        state.avg.VSI.eo         <NA>
# 16  N_D_INDEX_PER           pctile.VSI.eo   P_VULEOPCT
# 17  R_D_INDEX_PER    region.pctile.VSI.eo         <NA>
# 18  S_D_INDEX_PER     state.pctile.VSI.eo         <NA>
# 19   RAW_D_INCOME               pctlowinc    LOWINCPCT
# ...

# Also note not all the fields in lookup.fieldnames() are in names.all below.

# *** after reading defaults or user versions from the default or specified map names file - Read into the reactive lookup.fieldnames()
# BUT note these friendly names aren't same as longname in default lookup.fieldnames() and 
# also they lack full unique name like 'Ozone US percentile' or 'Cancer risk state average'
# The ones below are short and friendly for graph labels. 

# if using names from data(names.evars, package='ejscreen') etc., 
# > ejscreenformulas$glossaryfieldname[match(names.d , ejscreenformulas$Rfieldname)]
# [1] "Demographic Index (based on 2 factors, % low-income and % minority)" 
# [2] "Supplementary Demographic Index (based on 6 factors)"                
# [3] "% minority"                                                          
# [4] "% low-income"                                                        
# [5] "% less than high school"                                             
# [6] "% of households (interpreted as individuals) in linguistic isolation"
# [7] "% under age 5"                                                       
# [8] "% over age 64"                                                       
# > ejscreenformulas$glossaryfieldname[match(names.e, ejscreenformulas$Rfieldname)]
# [1] "PM2.5 level in air"                                           
# [2] "Ozone level in air"                                           
# [3] "Air toxics cancer risk"                                       
# [4] "Air toxics neurological hazard index"                         
# [5] "Air toxics respiratory hazard index"                          
# [6] "Diesel particulate matter level in air"                       
# [7] "% pre-1960 housing (lead paint indicator)"                    
# [8] "Traffic proximity and volume"                                 
# [9] "Proximity to National Priorities List (NPL) sites"            
# [10] "Proximity to Risk Management Plan (RMP) facilities"           
# [11] "Proximity to Treatment Storage and Disposal (TSDF) facilities"
# [12] "Proximity to major direct dischargers to water"               
# > ejscreenformulas$glossaryfieldname[match(names.ej, ejscreenformulas$Rfieldname)]
# [1] "EJ Index for PM2.5 level in air"                                           
# [2] "EJ Index for Ozone level in air"                                           
# [3] "EJ Index for Air toxics cancer risk"                                       
# [4] "EJ Index for Air toxics neurological hazard index"                         
# [5] "EJ Index for Air toxics respiratory hazard index"                          
# [6] "EJ Index for Diesel particulate matter level in air"                       
# [7] "EJ Index for % pre-1960 housing (lead paint indicator)"                    
# [8] "EJ Index for Traffic proximity and volume"                                 
# [9] "EJ Index for Proximity to National Priorities List (NPL) sites"            
# [10] "EJ Index for Proximity to Risk Management Plan (RMP) facilities"           
# [11] "EJ Index for Proximity to Treatment Storage and Disposal (TSDF) facilities"
# [12] "EJ Index for Proximity to major direct dischargers to water"               

# Roughly the lists of fieldnames below, names.e.batch , names.d.batch , and names.ej.batch , are already available as
#  data(names.evars, names.dvars, names.ejvars, package = 'ejscreen')
# BUT the names.d in ejscreen pkg has had VSI.svi6, which is removed here and for 2016 anyway
# and sort order differs here
# 

# BUT THESE GLOSSARY NAMES WOULD BE TOO LONG FOR GRAPHICS:
# names.e.friendly  <- ejscreenformulas$glossaryfieldname[match(names.e.batch ,  ejscreenformulas$Rfieldname)]
# names.d.friendly  <- ejscreenformulas$glossaryfieldname[match(names.d.batch ,  ejscreenformulas$Rfieldname)]
# names.ej.friendly <- ejscreenformulas$glossaryfieldname[match(names.ej.batch , ejscreenformulas$Rfieldname)]

mywtsname <- 'pop'  # used for weighted means to get stats on the average person in all these zones (e.g. avg person nearby any site)

names.d.batch          <- c('VSI.eo', 'pctlowinc', 'pctmin', 'pctlths', 'pctlingiso', 'pctunder5', 'pctover64') 
names.d.friendly <- c('Demog.Ind.', '% Low-inc.', '% Minority', '% <High School', '% Linguistic Isol.', '% < age 5', '% > age 64')

names.e.batch           <- c("pm", "o3", "cancer", "neuro", "resp", "dpm", "pctpre1960", 
                             "traffic.score", "proximity.npl", "proximity.rmp", "proximity.tsdf", "proximity.npdes")
names.e.friendly <- c("PM2.5", "Ozone", "Cancer risk", "Neuro.", "Respiratory", "Diesel PM", "% built pre-1960", 
                      "Traffic", "NPL proximity", "RMP proximity", "TSDF proximity", "NPDES proximity")

names.ej.batch  <- c("EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.cancer.eo", "EJ.DISPARITY.neuro.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", "EJ.DISPARITY.pctpre1960.eo", 
                     "EJ.DISPARITY.traffic.score.eo", "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo")
names.ej.friendly <- paste('EJ:', names.e.friendly)

names.all <- c(names.d.batch , names.e.batch , names.ej.batch )
names.all.friendly <- c(names.d.friendly, names.e.friendly, names.ej.friendly)

##########################################################################################
# DEMOGRAPHIC US TOTALS/US OVERALL PERCENTS -- MUST BE UPDATED WHEN NEW ACS DATA IS USED !
##########################################################################################

message('Note: MUST UPDATE ACS DATA IN CODE, WHEN SWITCHING TO NEW ACS DATASET')
#popus <-  309138711 # 309,138,711 is from 
# http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/12_5YR/B01001/0100000US
popus <-  314107084 # from 2010-2014
# http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/B01001/0100000US

#browseURL('http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/B01001/0100000US')
pop.US <- 314107084
under5.US <- 10205881 + 9767830; pctunder5.US <- under5.US / pop.US
over64.US <- (2915262 + 3616479 + 4666294 + 3328014 + 2362325	+ 1936823) + 
  (3223178	+ 4077987 + 5494784	+ 4231547	+ 3442927+ 3882341)
pctover64.US <- over64.US / pop.US
# 65 and 66 years	2,915,262	+/-9,504
# 67 to 69 years	3,616,479	+/-9,539
# 70 to 74 years	4,666,294	+/-9,753
# 75 to 79 years	3,328,014	+/-9,188
# 80 to 84 years	2,362,325	+/-7,350
# 85 years and over	1,936,823	
# 65 and 66 years	3,223,178	+/-9,504
# 67 to 69 years	4,077,987	+/-10,262
# 70 to 74 years	5,494,784	+/-11,596
# 75 to 79 years	4,231,547	+/-8,771
# 80 to 84 years	3,442,927	+/-9,460
# 85 years and over	3,882,341	

#browseURL('http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/B03002/0100000US')
nhwa.US <- 197159492
mins.US <- pop.US - nhwa.US
pctmin.US <- mins.US / pop.US
#browseURL('http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/B15002/0100000US')
lths.US <- 28587748; age25up.US <- 209056129; pctlths.US <- lths.US / age25up.US
#browseURL('http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/B16002/0100000US')
lingiso.US <- 5275272; hhlds.US <- 116211092; pctlingiso.US <- lingiso.US / hhlds.US
#browseURL('http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/C17002/0100000US')
lowinc.US <- 105773407; povknownratio.US <- 306226394 ; pctlowinc.US <- lowinc.US / povknownratio.US
#browseURL('http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/14_5YR/B25034/0100000US')
pre1960.US <- 14374462 + 7119373 + 17665365; pctpre1960.US <- pre1960.US / 132741033	

VSI.eo.US <- (pctmin.US + pctlowinc.US) / 2

# THESE SHOULD BE AVAILABLE IN THE BATCH ANALYSIS OUTPUTS?

us.percents.2016 <- 100 * sapply( paste(names.d.batch , '.US', sep='') , get)
# Calculated here from Census website tables:
# > cbind(us.percents.2016)
#               us.percents.2016
# VSI.eo.US            35.886338
# pctlowinc.US         34.540918
# pctmin.US            37.231759
# pctlths.US           13.674676
# pctlingiso.US         4.539388
# pctunder5.US          6.358886
# pctover64.US         13.746255
#   those match the numbers 
# from LOOKUP TABLES IN GDB:
# VULEOPCT	0.358863383
# LOWINCPCT	0.345409178
# MINORPCT	0.372317588
# LESSHSPCT	0.136746759
# LINGISOPCT	0.045393877
# UNDER5PCT	0.063588859
# OVER64PCT	0.137462551
## VULSVI6PCT	0.183486469

us.percents.2015 <- 100 * c(0.35015068301904, 0.337245110039133, 0.363056255998945, 
                            0.147948736834136, 0.0514480674233393, 0.0651419032409694, 0.131563727067491)

us.percents <- us.percents.2016

# The actual exact counts are obtained above, like lowinc.US,
# but that is not quite the same as pctlowinc.US * pop.US, because denominator was not pop!
# Early versions of summarizer used the approximation of pct*pop, but might want to use actual count?
us.counts.names <- paste(gsub('pct', '', names.d.batch), '.US', sep='')
us.counts.names <- gsub('min.US', 'mins.US', us.counts.names)
us.counts <- sapply(us.counts.names, get)
#
# for count of VSI.eo.US, do we want true count or pct * pop? or is VSI.eo.US count even used here at all?
# us.counts['VSI.eo.US'] <- mins.US + lowinc.US # ???
us.counts['VSI.eo.US'] <- VSI.eo.US * pop.US   # ???
#us.counts <- popus *  us.percents   # was the prior version's approach
#
# rename them because later code refers to them using the simple names.d or names.d.batch, not with .US on the names
names(us.percents) <- names.d.batch 
names(us.counts) <- names.d.batch 

# usavg.e <-  c(x, x, x, x, x, x, x, x, x, x, xxxxxx) # should be in table??
# what is in lookup tables in gdb 2016:
# PM25	9.318031237
# OZONE	47.40943204
# DSLPM	0.936750327
# CANCER	40.03084161
# RESP	1.838053631
# PRE1960PCT	0.295004484
# PTRAF 	592.7652763
# PNPL 	0.129302536
# PRMP 	0.428815643
# PTSDF 	0.071994194
# PWDIS 	0.307163434




####################
# OTHER
####################

# define scaling for text on barplot
bar.cex <- 1.10 

# size of marker for sites on map
circle.marker.radius <- 6
meters.per.mile = 1609.344 # proxistat::convert(1, from = 'miles', towhat = 'meters') 

# defaults for quantiles summary stats, with character string version used in ui to present options in checkboxes
# (probs of 0, 0.50, 1 are redundant since min, median, max are already separately shown)
probs.default.choices <- c('0','0.25','0.50','0.75','0.80','0.90','0.95','0.99','1.00')
probs.default  <- c(0.25,0.75,0.95) 
# as.numeric(probs.default.choices) # to have it default to all of these summary stats of the distributions across sites and people
# c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1) 

# defaults for which of the predefined functions to use for summary stats (logical vector or vector of names?)
colfun.picked.default = 'all'
rowfun.picked.default = 'all'

# default for all functions so can get stats even if one site (or one indicator at one site) has no data
na.rm = TRUE  

