
# DEFINE GLOBAL VARIABLES FOR USE IN server or ui - default values, etc.

# for interactive plots/charts/graphs
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

# file that has default input and output and friendly fieldnames & var type & var category:
mynamesfile.default <- 'map batch to friendly fieldnames v1.csv' 

# file that has default example of export of batch results, for use in testing/demonstrating summarizer:
mydemofile <- 'Export_Output_Example2.txt' 
# mydemofile <- 'path/hugebatchoutput.csv' if running local copy for huge file too large for upload limit

# *** after reading defaults or user versions from the default or specified map names file - read into lookup.filenames() ***
# BUT note these friendly names aren't same as longname in default lookup.fieldnames() and 
# also they lack full unique name like 'Ozone US percentile' or 'Cancer risk state average'
# The ones below for graph labels. Also note not all the fields in lookup.fieldnames() are in names.all below - these are just for plots.

mywtsname <- 'pop'  # used for weighted means to get stats on the average person in all these zones (e.g. avg person nearby any site)

names.d          <- c('VSI.eo', 'pctlowinc', 'pctmin', 'pctlths', 'pctlingiso', 'pctunder5', 'pctover64') 
names.d.friendly <- c('Demog.Ind.', '% Low-inc.', '% Minority', '% <High School', '% Linguistic Isol.', '% < age 5', '% > age 64')

names.e          <- c("pm", "o3", "cancer", "neuro", "resp", "dpm", "pctpre1960", 
                      "traffic.score", "proximity.npl", "proximity.rmp", "proximity.tsdf", 
                      "proximity.npdes")
names.e.friendly <- c("PM2.5", "Ozone", "Cancer risk", "Neuro.", "Respiratory", "Diesel PM", "% built pre-1960", 
                      "Traffic", "NPL proximity", "RMP proximity", "TSDF proximity", 
                      "NPDES proximity")

names.ej <- c("EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.cancer.eo", 
              "EJ.DISPARITY.neuro.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", 
              "EJ.DISPARITY.pctpre1960.eo", "EJ.DISPARITY.traffic.score.eo", 
              "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", 
              "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo")
names.ej.friendly <- paste('EJ:', names.e.friendly)

names.all <- c(names.d, names.e, names.ej)
names.all.friendly <- c(names.d.friendly, names.e.friendly, names.ej.friendly)

##########################################################################################
# MUST BE UPDATED WHEN NEW ACS DATA IS USED !
##########################################################################################

popus <-  309138711 # 309,138,711 is from http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/12_5YR/B01001/0100000US 
# 307727594 was in sample reports # MUST BE UPDATED WHEN NEW ACS DATA IS USED ! 307727594 is from ???  
us.percents <- 100 * c(0.35015068301904, 0.337245110039133, 0.363056255998945, 
                       0.147948736834136, 0.0514480674233393, 0.0651419032409694, 0.131563727067491)
us.counts <- popus * (us.percents / 100)
names(us.percents) <- names.d
names(us.counts) <- names.d

##########################################################################################
# DEMOGRAPHIC US TOTALS/US OVERALL PERCENTS -- MUST BE UPDATED WHEN NEW ACS DATA IS USED !
##########################################################################################

popus <-  309138711 # 309,138,711 is from http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/12_5YR/B01001/0100000US 
# 307727594 was in sample reports # MUST BE UPDATED WHEN NEW ACS DATA IS USED ! 307727594 is from ???  
us.percents <- 100 * c(0.35015068301904, 0.337245110039133, 0.363056255998945, 
                       0.147948736834136, 0.0514480674233393, 0.0651419032409694, 0.131563727067491)
us.counts <- us.percents * popus
names(us.percents) <- names.d
names(us.counts) <- names.d

####################
# OTHER
####################

# define scaling for text on barplot
bar.cex <- 1.10 

# size of marker for sites on map
circle.marker.radius <- 6
meters.per.mile = 1609.34

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
na.rm=TRUE  
