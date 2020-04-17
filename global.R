rm(list = ls())

testing <- TRUE # ********** while debugging: testing <- FALSE
if (testing) {cat("Starting global\n")}

# **** MUST UPDATE ACS-related CODE BELOW YEARLY FOR NEW ACS DATASET!**

##'##########################################################################
# # *** To run this App locally and specify a local large custom file, 
# # uncomment one of the following lines: 
# mydemofile <- 'path/hugebatchoutput.csv' 
# # for example mydemofile <- 'ejtest2.csv'
#'###########################################################################

#'###########################################################################
# DATA ANALYSIS FUNCTIONS
#'###########################################################################

requireFewerPackages <- TRUE

# Get packages and source code to get functions needed ***
#
# Building them into this package or sourcing them in this package
# *** lets web app require less time and space than relying on the packages 
# ejscreen, ejanalysis, analyze.stuff, proxistat
# # available at
# # http://ejanalysis.github.io 
# *** BUT harder to keep in sync with latest updates in those packages.
# # ejscreen pkg provides popupunits, for example, and proxistat has counties

if (!requireFewerPackages) {
  require(analyze.stuff)
  analyze.stuff::required.packages()
  require(ejscreen); require(ejanalysis)  
} else {
  source("pct.above.R")  # returns percent of rows (or wtd people) that have value above specified cutoff (or mean as default), for each column of data.frame
  source("count.above.R")  # returns count of how many rows (or wtd people) have value above specified cutoff (or mean as default), for each column of data.frame
  source("cols.above.count.R")  # returns count of how many cols (e.g. how many variables or indicators) have value above specified cutoff (or mean as default), for each row of data.frame
  source("flagged.R")  # creates flag that is TRUE if at least one of 12 indicators is > cutoff (EJ index >50th or 80th or 95th%ile, for example), for each of many rows of a data.frame
  source("rowMaxs.R")  # returns Max of each row
  source("rowMins.R")  # returns Min of each row
  source("colMaxs.R")  # returns Max of each col
  source("colMins.R")  # returns Min of each col
  source("wtd.colMeans.R")  # returns wtd.mean of each col
  source("lead.zeroes.R")  # add leading zeroes as needed to fix FIPS that had been stored as numeric
  
  source("change.fieldnames.R")  # updated function that helps change or resort fieldnames using a map file mapping old to new names
  # **** Might shift to using the version from the packages
  # analyze.stuff::change.fieldnames	Change some or all of the colnames of a data.frame or matrix via a 1-1 map
  # ejscreen::change.fieldnames.ejscreen.csv		Change colnames of csv file on EJSCREEN FTP site to nicer colnames
  
  # source('pct.below.R') # returns percent of rows (or wtd people)
  # that have value below specified cutoff (or mean as default),
  # for each column of data.frame
  # source('pop.ecdf.R') # plot pop-wtd ecdf(s) for one
  # demographic group vs others, etc., for comparing conditions
  # between groups across many Census areal units (e.g. tracts)
}
#'###########################################################################

# COUNTY DATA

## *** could get this from proxistat package, 
##   or keep in this package and then could just use lazy loading
# data(counties, package='proxistat')
data(counties, package = 'batch.summarizer')
counties$nonwhite <- round(100 - counties$white, 1)



#'###########################################################################

require(batch.summarizer)
source("batch.summarize.R")  # will be part of package but had trouble with export tag bug
# FUNCTIONS only in this batch.summarizer package,
# now loaded as part of this package rather than sourced:
# source('batch.read.R') # now as package
# source('batch.clean.R') # now as package
# source("maphelpers.R")  # if we want percent choropleths of county data
# source("wilcoxon.pvalues.r")  # for stat significance testing - from air
# source("plotlyGraphWidget.R") ## for interactive plots/charts/graphs
# # see https://plot.ly/r/getting-started/
#
# load gomap.js ??

#'###########################################################################
#
# # OTHER PACKAGES USED:
library(Hmisc)  # various useful functions for data analysis
library(plotrix)  # for better weighted.hist than ggplot2 can provide.
library(ggplot2)  # for geom_histogram() that allows weights to be used. plotrix package also does wtd hist, but better.
library(shiny)  # http://shiny.rstudio.com
# library(data.table) # used by analyze.stuff or newer ver of wtd.colMeans()?
# library(dplyr) # might not need this

# MAPS PACKAGES TO OBTAIN THE leaflet PACKAGE (IF VERSION NOT ON CRAN?):
#
library(maps)  # for static maps; choropleth of counties, etc.
library(mapproj)
library(leaflet)  # for interactive maps
# # if need to get from github...
# library(devtools); devtools::install_github('rstudio/leaflet')
# library(leafletR) # for interactive maps? (different than leaflet)
#
# For leaflet maps, might need to define these here until
# latest version of leaflet package is on cran:
require(htmlwidgets)
leafletOutput = function(outputId, width = "100%", height = 400) {
  htmlwidgets::shinyWidgetOutput(outputId, "leaflet", width, 
                                 height, "leaflet")  #, error.label = NULL)
}
renderLeaflet = function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) 
    expr = substitute(expr)  # force quoted
  htmlwidgets::shinyRenderWidget(expr, leafletOutput, env, 
                                 quoted = TRUE)
}
########################################################################### #


# *** COULD SPECIFY LARGE FILE THAT IS OUTPUT OF BATCH RUN
# AND INPUT TO THIS SUMMARIZER here if huge
# file too large for Shiny's file upload limit - 
# For large file, edit code & run a local copy of this summarizer.


#'###################################################################################
# 
# *** SPECIFY YEAR/VERSION/VINTAGE 
# OF EJSCREEN & ACS DATA & BATCH TOOL (2019, 2020, etc.)
# and names of columns/fields/variables analyzed
# 
#'###################################################################################

suppressWarnings(rm(mydemofile))  # just in case testing and had already specified this
# Default example of export of batch results, for use in
# testing/demonstrating summarizer:
if (!exists("mydemofile")) {
  mydemofile <- 'default_example_2020-03_EJSCREEN_BATCH_Export_Output.csv' # "Export_Output_Example2.csv" # 'Export_Output_2019-08.txt' # 
}  
if (!exists("mydemofile.pop")) {
  mydemofile.pop <- 'default_example_2020-03_EJSCREEN_BATCH_Export_Output.csv' # "Export_Output_Example2.pop.csv" # 'Export_Output_2019-08.txt' #
}  # not realistic - just smaller pop numbers!!

# Default File that has default input and output and friendly
# fieldnames & var type & var category:
mynamesfile.default <- 'map_batch_to_friendly_fieldnames_2019.csv' 
# mynamesfile.default   <- 'map_batch_to_friendly_fieldnames_2018.csv'
# mynamesfile.default <- 'map_batch_to_friendly_fieldnames_2016.csv'

mywtsname <- "pop"  
# used for weighted means to get 
# stats on the average person in all these zones (e.g. avg person nearby any site)

#'########################################################
#
# *** These are all in package called ejscreen now and maybe can transition to that:

names.d.batch <- c("VSI.eo", "pctlowinc", "pctmin", "pctlths", 
                   "pctlingiso", "pctunder5", "pctover64")


source('ACS_US_TOTALS_2014-2018.R')


names.d.friendly <- c("Demog.Ind.", "% Low-inc.", "% Minority", 
                      "% <High School", "% Linguistic Isol.", "% < age 5", "% > age 64")
# *** should I add these:?  'ACSIPOVBAS', 'ACSTOTHH',
# 'ACSEDUCBAS', 'PRE1960'

names.e.batch <- c("pm", "o3", "cancer", "resp", "dpm", "pctpre1960", 
                   "traffic.score", "proximity.npl", "proximity.rmp", "proximity.tsdf", 
                   "proximity.npdes")
#'neuro', 
names.e.friendly <- c("PM2.5", "Ozone", "Cancer risk", "Respiratory", 
                      "Diesel PM", "% built pre-1960", "Traffic", "NPL proximity", 
                      "RMP proximity", "TSDF proximity", "NPDES proximity")
#'Neuro.',

names.ej.batch <- c("EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", 
                    "EJ.DISPARITY.cancer.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", 
                    "EJ.DISPARITY.pctpre1960.eo", "EJ.DISPARITY.traffic.score.eo", 
                    "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", 
                    "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo")
#'EJ.DISPARITY.neuro.eo', 
names.ej.friendly <- paste("EJ:", names.e.friendly)

names.all <- c(names.d.batch, names.e.batch, names.ej.batch)
names.all.friendly <- c(names.d.friendly, names.e.friendly, names.ej.friendly)

# ***************
# *** neuro-related fields dropped in 2016 may need to be removed from 
# these in batch summarizer global.R:
# names.e.batch names.e.friendly names.ej.batch names.ej.friendly
# ***************
#'########################################################
# WHICH FIELDS TO COMPARE TO THRESHOLDS

threshgroup.default <- list(
  "EJ US pctiles", "EJ Region pctiles", "EJ State pctiles"
)
threshold.default <- list(50, 50, 50)  
# a default for cutoff in at/above threshold stat summarizing EJ US percentiles
#
# Allows user to specify user-specified # of groups of
# user-specified fields to compare to user-specified
# thresholds.  Initially, just the 3 thresholds can be
# altered, not which fields are compared or how many groups
# or what the groups are called.
# 
# NOTE: server.R creates threshold names at the moment
# threshnames.default <- list( grep('^pctile.EJ.DISPARITY.',
# colnames(fulltabler()), value=TRUE) ,
# grep('regionpctile.EJ.DISPARITY.', colnames(fulltabler()),
# value=TRUE) , grep('statepctile.EJ.DISPARITY.',
# colnames(fulltabler()), value=TRUE) )

#'########################################################
# Specify units that go with each environmental indicator.
# This is also available via 
#   data(popupunits, package='ejscreen') 
# except that package has to be obtained from github.

popupunits <- structure(
  list(evar = c("pm", "o3", "cancer", "resp", 
                "dpm", "pctpre1960", "traffic.score", "proximity.npl", "proximity.rmp", 
                "proximity.tsdf", "proximity.npdes"), 
       units = c("ug/m3", 
                 "ppb", "lifetime risk per million", "", "ug/m3", "= fraction pre-1960", 
                 "daily vehicles/meters distance", "sites/km distance", "facilities/km distance", 
                 "facilities/km distance", 
                 "wtd facilities/km distance")), 
  .Names = c("evar", 
             "units"), row.names = c(NA, -12L), class = "data.frame")
#'neuro', # neuro was dropped from EJSCREEN around 2016
#'############################


#'########################'########################'#######################

#'############################
# Useful open map layers
#'############################
# layer.admin <- 'OpenMapSurfer.AdminBounds' # neighborhoods,
# counties, etc.  layer.houses <- 'HERE.hybridDay'
# layer.street1 <- 'Esri.WorldStreetMap' layer.street2 <-
# 'OpenStreetMap.HOT' layer.street3 <- 'Stamen.TonerHybrid'
# layer.topo <- 'Esri.WorldTopoMap' layer.delorme <-
# 'Esri.DeLorme' layer.natgeo <- 'Esri.NatGeoWorldMap'
# layer.sat1 <- 'Esri.WorldImagery' layer.sat2 <-
# 'MapQuestOpen.Aerial' layer.esrigray <-
# 'Esri.WorldGrayCanvas'

mapserver1 = "http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer"
mapserver2 = "http://services.arcgisonline.com/ArcGIS/rest/services/Reference/World_Boundaries_and_Places/MapServer"

# var MapQuestOpen_Aerial =
# L.tileLayer('http://oatile{s}.mqcdn.com/tiles/1.0.0/sat/{z}/{x}/{y}.jpg',
# { attribution: 'Tiles Courtesy of <a
# href='http://www.mapquest.com/'>MapQuest</a> &mdash;
# Portions Courtesy NASA/JPL-Caltech and U.S. Depart. of
# Agriculture, Farm Service Agency', subdomains: '1234' });

#'########################'########################'#######################

#'############################
# MISC DEFAULT VALUES
#'############################

default.tab <- "Details"  # for some reason this has to be used to update info so barplot etc works on first click
default.tab.start <- "Upload"

#'############################
# WHICH QUANTILES TO USE IN SUMMARY STATS
#'############################
# Defaults for quantiles summary stats, with character string
# version used in ui to present options in checkboxes (probs
# of 0, 0.50, 1 are redundant since min, median, max are
# already separately shown)
probs.default.choices <- c("0", "0.25", "0.50", "0.75", "0.80", 
                           "0.90", "0.95", "0.99", "1.00")
probs.default <- c(0.25, 0.75, 0.95)
# as.numeric(probs.default.choices) # to have it default to
# all of these summary stats of the distributions across
# sites and people c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1)

# Defaults for which of the predefined functions to use for
# summary stats (logical vector or vector of names?)
colfun.picked.default = "all"
rowfun.picked.default = "all"

# Default for all functions so can get stats even if one site
# (or one indicator at one site) has no data
na.rm = TRUE

#'############################
# MISC DEFAULTS
#'############################
pixels.per.char <- 10  #????
max.allowed = 30  # max length of site name we want displayed before wrapping in sites table
# define scaling for text on barplot
bar.cex <- 1.1
# size of marker for sites on map:
circle.marker.radius <- 6
meters.per.mile = 1609.344  # proxistat::convert(1, from = 'miles', towhat = 'meters') 

######################################################## #






############################################################### #
############################################################### #

# NOTES ON FIELD NAMES OF IMPORTED DATA - 
# batchfields2019 and others below are not actually used...
# csv files are used to specify mapping of field names (colnames) 
# from batch output to what is used in R here

############################################################### #
#
# 2020 
#
# "2020" DATA here means the EJSCREEN BATCH TOOL AS OF EARLY 2021
# (but uses ACS 2015-2019 which is released around Sep-Dec 2020)


#   TO UPDATE WHEN AVAILABLE?



############################################################### #
#
# 2019 
#
# "2019" DATA here means the EJSCREEN BATCH TOOL AS OF EARLY 2020 
# (but uses ACS 2014-2018 which was released around Sep-Dec 2019)
#
# the difference here between batchfields2018 and 2019 is just 
# 2018 had 'id' while 2019 used 'facid'

batchfields2019 <- c(
  'OBJECTID', 'id', 'name', 'lat', 'lon', 
  '_ACSTOTPOP', '_ACSIPOVBAS', '_ACSTOTHH', '_ACSEDUCBAS', '_PRE1960', 
  '_buff', '_stabbr', '_statename', '_region', 
  '_S_P_PM25', '_R_P_PM25', '_N_P_PM25', 
  '_S_P_O3', '_R_P_O3', '_N_P_O3', 
  '_S_P_DIESEL', '_R_P_DIESEL', '_N_P_DIESEL', 
  '_S_P_CANCER', '_R_P_CANCER', '_N_P_CANCER',
  '_S_P_RESP', '_R_P_RESP', '_N_P_RESP', 
  '_S_P_TRAFFIC', '_R_P_TRAFFIC', '_N_P_TRAFFIC',
  '_S_P_LEAD', '_R_P_LEAD', '_N_P_LEAD', 
  '_S_P_NPL', '_R_P_NPL', '_N_P_NPL', 
  '_S_P_RMP', '_R_P_RMP', '_N_P_RMP', 
  '_S_P_TSDF', '_R_P_TSDF', '_N_P_TSDF', 
  '_S_P_NPDES', '_R_P_NPDES', '_N_P_NPDES', 
  '_RAW_E_PM25', '_S_E_PM25_PER', '_R_E_PM25_PER', '_N_E_PM25_PER', '_S_E_PM25', '_R_E_PM25', '_N_E_PM25', 
  '_RAW_E_O3', '_S_E_O3_PER', '_R_E_O3_PER', '_N_E_O3_PER', '_S_E_O3', '_R_E_O3', '_N_E_O3',
  '_RAW_E_DIESEL', '_S_E_DIESEL_PER', '_R_E_DIESEL_PER', '_N_E_DIESEL_PER', '_S_E_DIESEL', '_R_E_DIESEL', '_N_E_DIESEL',
  '_RAW_E_CANCER', '_S_E_CANCER_PER', '_R_E_CANCER_PER', '_N_E_CANCER_PER', '_S_E_CANCER', '_R_E_CANCER', '_N_E_CANCER',
  '_RAW_E_RESP', '_S_E_RESP_PER', '_R_E_RESP_PER', '_N_E_RESP_PER', '_S_E_RESP', '_R_E_RESP', '_N_E_RESP', 
  '_RAW_E_TRAFFIC', '_S_E_TRAFFIC_PER', '_R_E_TRAFFIC_PER', '_N_E_TRAFFIC_PER', '_S_E_TRAFFIC', '_R_E_TRAFFIC', '_N_E_TRAFFIC',
  '_RAW_E_LEAD', '_S_E_LEAD_PER', '_R_E_LEAD_PER', '_N_E_LEAD_PER', '_S_E_LEAD', '_R_E_LEAD', '_N_E_LEAD', 
  '_RAW_E_NPL', '_S_E_NPL_PER', '_R_E_NPL_PER', '_N_E_NPL_PER', '_S_E_NPL', '_R_E_NPL', '_N_E_NPL',
  '_RAW_E_RMP', '_S_E_RMP_PER', '_R_E_RMP_PER', '_N_E_RMP_PER', '_S_E_RMP', '_R_E_RMP', '_N_E_RMP',
  '_RAW_E_TSDF', '_S_E_TSDF_PER', '_R_E_TSDF_PER', '_N_E_TSDF_PER', '_S_E_TSDF', '_R_E_TSDF', '_N_E_TSDF', 
  '_RAW_E_NPDES', '_S_E_NPDES_PER', '_R_E_NPDES_PER', '_N_E_NPDES_PER', '_S_E_NPDES', '_R_E_NPDES', '_N_E_NPDES',
  '_RAW_D_INDEX', '_S_D_INDEX_PER', '_R_D_INDEX_PER', '_N_D_INDEX_PER', '_S_D_INDEX', '_R_D_INDEX', '_N_D_INDEX', 
  '_RAW_D_MINOR', '_S_D_MINOR_PER', '_R_D_MINOR_PER', '_N_D_MINOR_PER', '_S_D_MINOR', '_R_D_MINOR', '_N_D_MINOR', 
  '_RAW_D_INCOME', '_S_D_INCOME_PER', '_R_D_INCOME_PER', '_N_D_INCOME_PER', '_S_D_INCOME', '_R_D_INCOME', '_N_D_INCOME', 
  '_RAW_D_LING', '_S_D_LING_PER', '_R_D_LING_PER', '_N_D_LING_PER', '_S_D_LING', '_R_D_LING', '_N_D_LING', 
  '_RAW_D_LESSHS', '_S_D_LESSHS_PER', '_R_D_LESSHS_PER', '_N_D_LESSHS_PER', '_S_D_LESSHS', '_R_D_LESSHS', '_N_D_LESSHS', 
  '_RAW_D_UNDER5', '_S_D_UNDER5_PER', '_R_D_UNDER5_PER', '_N_D_UNDER5_PER', '_S_D_UNDER5', '_R_D_UNDER5', '_N_D_UNDER5',
  '_RAW_D_OVER64', '_S_D_OVER64_PER', '_R_D_OVER64_PER', '_N_D_OVER64_PER', '_S_D_OVER64', '_R_D_OVER64', '_N_D_OVER64'
)

######################################################## #
#
# 2018 
#
# OBJECTID,facid,name,lat,lon,_ACSTOTPOP,_ACSIPOVBAS,_ACSTOTHH,_ACSEDUCBAS,_PRE1960,_buff,_stabbr,_statename,_region,_S_P_PM25,_R_P_PM25,_N_P_PM25,_S_P_O3,_R_P_O3,_N_P_O3,_S_P_DIESEL,_R_P_DIESEL,_N_P_DIESEL,_S_P_CANCER,_R_P_CANCER,_N_P_CANCER,_S_P_RESP,_R_P_RESP,_N_P_RESP,_S_P_TRAFFIC,_R_P_TRAFFIC,_N_P_TRAFFIC,_S_P_LEAD,_R_P_LEAD,_N_P_LEAD,_S_P_NPL,_R_P_NPL,_N_P_NPL,_S_P_RMP,_R_P_RMP,_N_P_RMP,_S_P_TSDF,_R_P_TSDF,_N_P_TSDF,_S_P_NPDES,_R_P_NPDES,_N_P_NPDES,_RAW_E_PM25,_S_E_PM25_PER,_R_E_PM25_PER,_N_E_PM25_PER,_S_E_PM25,_R_E_PM25,_N_E_PM25,_RAW_E_O3,_S_E_O3_PER,_R_E_O3_PER,_N_E_O3_PER,_S_E_O3,_R_E_O3,_N_E_O3,_RAW_E_DIESEL,_S_E_DIESEL_PER,_R_E_DIESEL_PER,_N_E_DIESEL_PER,_S_E_DIESEL,_R_E_DIESEL,_N_E_DIESEL,_RAW_E_CANCER,_S_E_CANCER_PER,_R_E_CANCER_PER,_N_E_CANCER_PER,_S_E_CANCER,_R_E_CANCER,_N_E_CANCER,_RAW_E_RESP,_S_E_RESP_PER,_R_E_RESP_PER,_N_E_RESP_PER,_S_E_RESP,_R_E_RESP,_N_E_RESP,_RAW_E_TRAFFIC,_S_E_TRAFFIC_PER,_R_E_TRAFFIC_PER,_N_E_TRAFFIC_PER,_S_E_TRAFFIC,_R_E_TRAFFIC,_N_E_TRAFFIC,_RAW_E_LEAD,_S_E_LEAD_PER,_R_E_LEAD_PER,_N_E_LEAD_PER,_S_E_LEAD,_R_E_LEAD,_N_E_LEAD,_RAW_E_NPL,_S_E_NPL_PER,_R_E_NPL_PER,_N_E_NPL_PER,_S_E_NPL,_R_E_NPL,_N_E_NPL,_RAW_E_RMP,_S_E_RMP_PER,_R_E_RMP_PER,_N_E_RMP_PER,_S_E_RMP,_R_E_RMP,_N_E_RMP,_RAW_E_TSDF,_S_E_TSDF_PER,_R_E_TSDF_PER,_N_E_TSDF_PER,_S_E_TSDF,_R_E_TSDF,_N_E_TSDF,_RAW_E_NPDES,_S_E_NPDES_PER,_R_E_NPDES_PER,_N_E_NPDES_PER,_S_E_NPDES,_R_E_NPDES,_N_E_NPDES,_RAW_D_INDEX,_S_D_INDEX_PER,_R_D_INDEX_PER,_N_D_INDEX_PER,_S_D_INDEX,_R_D_INDEX,_N_D_INDEX,_RAW_D_MINOR,_S_D_MINOR_PER,_R_D_MINOR_PER,_N_D_MINOR_PER,_S_D_MINOR,_R_D_MINOR,_N_D_MINOR,_RAW_D_INCOME,_S_D_INCOME_PER,_R_D_INCOME_PER,_N_D_INCOME_PER,_S_D_INCOME,_R_D_INCOME,_N_D_INCOME,_RAW_D_LING,_S_D_LING_PER,_R_D_LING_PER,_N_D_LING_PER,_S_D_LING,_R_D_LING,_N_D_LING,_RAW_D_LESSHS,_S_D_LESSHS_PER,_R_D_LESSHS_PER,_N_D_LESSHS_PER,_S_D_LESSHS,_R_D_LESSHS,_N_D_LESSHS,_RAW_D_UNDER5,_S_D_UNDER5_PER,_R_D_UNDER5_PER,_N_D_UNDER5_PER,_S_D_UNDER5,_R_D_UNDER5,_N_D_UNDER5,_RAW_D_OVER64,_S_D_OVER64_PER,_R_D_OVER64_PER,_N_D_OVER64_PER,_S_D_OVER64,_R_D_OVER64,_N_D_OVER64
# or # NOT USED: - SEE CSV FILES that provide these names

# NOTE THAT WHEN R IMPORTS A CSV WITH LEADING UNDERSCORES IN HEADER (COLUMN NAMES), IT ADDS X BEFORE EACH UNDERSCORE

batchfields2018 <- c(
  "OBJECTID", "facid", "name", "lat", "lon", 
  "_ACSTOTPOP", "_ACSIPOVBAS", "_ACSTOTHH", "_ACSEDUCBAS", 
  "_PRE1960", "_buff", "_stabbr", "_statename", "_region", 
  "_S_P_PM25", "_R_P_PM25", "_N_P_PM25", "_S_P_O3", "_R_P_O3", 
  "_N_P_O3", "_S_P_DIESEL", "_R_P_DIESEL", "_N_P_DIESEL", "_S_P_CANCER", 
  "_R_P_CANCER", "_N_P_CANCER", "_S_P_RESP", "_R_P_RESP", "_N_P_RESP", 
  "_S_P_TRAFFIC", "_R_P_TRAFFIC", "_N_P_TRAFFIC", "_S_P_LEAD", 
  "_R_P_LEAD", "_N_P_LEAD", "_S_P_NPL", "_R_P_NPL", "_N_P_NPL", 
  "_S_P_RMP", "_R_P_RMP", "_N_P_RMP", "_S_P_TSDF", "_R_P_TSDF", 
  "_N_P_TSDF", "_S_P_NPDES", "_R_P_NPDES", "_N_P_NPDES", "_RAW_E_PM25", 
  "_S_E_PM25_PER", "_R_E_PM25_PER", "_N_E_PM25_PER", "_S_E_PM25", 
  "_R_E_PM25", "_N_E_PM25", "_RAW_E_O3", "_S_E_O3_PER", "_R_E_O3_PER", 
  "_N_E_O3_PER", "_S_E_O3", "_R_E_O3", "_N_E_O3", "_RAW_E_DIESEL", 
  "_S_E_DIESEL_PER", "_R_E_DIESEL_PER", "_N_E_DIESEL_PER", 
  "_S_E_DIESEL", "_R_E_DIESEL", "_N_E_DIESEL", "_RAW_E_CANCER", 
  "_S_E_CANCER_PER", "_R_E_CANCER_PER", "_N_E_CANCER_PER", 
  "_S_E_CANCER", "_R_E_CANCER", "_N_E_CANCER", "_RAW_E_RESP", 
  "_S_E_RESP_PER", "_R_E_RESP_PER", "_N_E_RESP_PER", "_S_E_RESP", 
  "_R_E_RESP", "_N_E_RESP", "_RAW_E_TRAFFIC", "_S_E_TRAFFIC_PER", 
  "_R_E_TRAFFIC_PER", "_N_E_TRAFFIC_PER", "_S_E_TRAFFIC", "_R_E_TRAFFIC", 
  "_N_E_TRAFFIC", "_RAW_E_LEAD", "_S_E_LEAD_PER", "_R_E_LEAD_PER", 
  "_N_E_LEAD_PER", "_S_E_LEAD", "_R_E_LEAD", "_N_E_LEAD", "_RAW_E_NPL", 
  "_S_E_NPL_PER", "_R_E_NPL_PER", "_N_E_NPL_PER", "_S_E_NPL", 
  "_R_E_NPL", "_N_E_NPL", "_RAW_E_RMP", "_S_E_RMP_PER", "_R_E_RMP_PER", 
  "_N_E_RMP_PER", "_S_E_RMP", "_R_E_RMP", "_N_E_RMP", "_RAW_E_TSDF", 
  "_S_E_TSDF_PER", "_R_E_TSDF_PER", "_N_E_TSDF_PER", "_S_E_TSDF", 
  "_R_E_TSDF", "_N_E_TSDF", "_RAW_E_NPDES", "_S_E_NPDES_PER", 
  "_R_E_NPDES_PER", "_N_E_NPDES_PER", "_S_E_NPDES", "_R_E_NPDES", 
  "_N_E_NPDES", "_RAW_D_INDEX", "_S_D_INDEX_PER", "_R_D_INDEX_PER", 
  "_N_D_INDEX_PER", "_S_D_INDEX", "_R_D_INDEX", "_N_D_INDEX", 
  "_RAW_D_MINOR", "_S_D_MINOR_PER", "_R_D_MINOR_PER", "_N_D_MINOR_PER", 
  "_S_D_MINOR", "_R_D_MINOR", "_N_D_MINOR", "_RAW_D_INCOME", 
  "_S_D_INCOME_PER", "_R_D_INCOME_PER", "_N_D_INCOME_PER", 
  "_S_D_INCOME", "_R_D_INCOME", "_N_D_INCOME", "_RAW_D_LING", 
  "_S_D_LING_PER", "_R_D_LING_PER", "_N_D_LING_PER", "_S_D_LING", 
  "_R_D_LING", "_N_D_LING", "_RAW_D_LESSHS", "_S_D_LESSHS_PER", 
  "_R_D_LESSHS_PER", "_N_D_LESSHS_PER", "_S_D_LESSHS", "_R_D_LESSHS", 
  "_N_D_LESSHS", "_RAW_D_UNDER5", "_S_D_UNDER5_PER", "_R_D_UNDER5_PER", 
  "_N_D_UNDER5_PER", "_S_D_UNDER5", "_R_D_UNDER5", "_N_D_UNDER5", 
  "_RAW_D_OVER64", "_S_D_OVER64_PER", "_R_D_OVER64_PER", "_N_D_OVER64_PER", 
  "_S_D_OVER64", "_R_D_OVER64", "_N_D_OVER64")

############################ #
#
# 2016
#
# This is the output of the new faster batch tool with 2016
# dataset???: but it does not match the format of outputs
# from nonshiny version as delivered july 2016! (which is
# further down below)
#'############################
# NOTE batchfields2016 IS NOT USED - THEY ARE MAPPED IN THE
# FILE mynamesfile.default <-
# 'map_batch_to_friendly_fieldnames_2016.csv' 
# 
# batchfields2016 <- c( 
# 'OBJECTID', 'FACID', 'LAT','LONG',
# 'POP100','mins','pctmin','lowinc','pctlowinc','lths','pctlths','lingiso','pctlingiso','under5','pctunder5','over64','pctover64',
# 'traffic.score','pctpre1960','pm','o3','cancer','dpm','resp','proximity.tsdf','proximity.rmp','proximity.npl','proximity.npdes',
# 'VSI.eo','VSI.svi6',
# 'inedx_EJ_Traffic','inedx_EJ_Lead','inedx_EJ_PM','inedx_EJ_Ozone','inedx_EJ_Cancer','inedx_EJ_DPM','inedx_EJ_Resp','inedx_EJ_proximity.tsdf','inedx_EJ_proximity.rmp','inedx_EJ_proximity.npl',
# #'neuro','inedx_EJ_proximity.npdes',
# 'inedx_EJ_Neuro','N_E_NEURO_PER','N_E_NEURO','R_E_NEURO_PER','R_E_NEURO','S_E_NEURO_PER','S_E_NEURO',
# 'BLOCKID','Distance.x',
# 'BLOCKGROUPFIPS','STUSAB','STATE','COUNTY','TRACT','BLKGRP','BLOCK','REGION',
# 'N_D_INDEX_PER',
# 'N_E_NPDES_PER','N_E_TSDF_PER','N_E_RMP_PER','N_E_NPL_PER','N_E_LEAD_PER','N_E_TRAFFIC_PER','N_E_RESP_PER','N_E_CANCER_PER','N_E_DIESEL_PER','N_E_O3_PER','N_E_PM25_PER',
# 'N_D_MINOR_PER','N_D_INCOME_PER','N_D_LESSHS_PER','N_D_LING_PER','N_D_UNDER5_PER','N_D_OVER64_PER',
# 'N_D_INDEX',
# 'N_E_NPDES','N_E_TSDF','N_E_RMP','N_E_NPL','N_E_LEAD','N_E_TRAFFIC','N_E_RESP','N_E_CANCER','N_E_DIESEL','N_E_O3','N_E_PM25',
# 'N_D_MINOR','N_D_INCOME','N_D_LESSHS','N_D_LING','N_D_UNDER5','N_D_OVER64',
# 'R_D_INDEX_PER',
# 'R_E_NPDES_PER','R_E_TSDF_PER','R_E_RMP_PER','R_E_NPL_PER','R_E_LEAD_PER','R_E_TRAFFIC_PER','R_E_RESP_PER','R_E_CANCER_PER','R_E_DIESEL_PER','R_E_O3_PER','R_E_PM25_PER',
# 'R_D_MINOR_PER','R_D_INCOME_PER','R_D_LESSHS_PER','R_D_LING_PER','R_D_UNDER5_PER','R_D_OVER64_PER',
# 'R_D_INDEX',
# 'R_E_NPDES','R_E_TSDF','R_E_RMP','R_E_NPL','R_E_LEAD','R_E_TRAFFIC','R_E_RESP','R_E_CANCER','R_E_DIESEL','R_E_O3','R_E_PM25',
# 'R_D_MINOR','R_D_INCOME','R_D_LESSHS','R_D_LING','R_D_UNDER5','R_D_OVER64',
# 'S_D_INDEX_PER',
# 'S_E_NPDES_PER','S_E_TSDF_PER','S_E_RMP_PER','S_E_NPL_PER','S_E_LEAD_PER','S_E_TRAFFIC_PER','S_E_RESP_PER','S_E_CANCER_PER','S_E_DIESEL_PER','S_E_O3_PER','S_E_PM25_PER',
# 'S_D_MINOR_PER','S_D_INCOME_PER','S_D_LESSHS_PER','S_D_LING_PER','S_D_UNDER5_PER','S_D_OVER64_PER',
# 'S_D_INDEX',
# 'S_E_NPDES','S_E_TSDF','S_E_RMP','S_E_NPL','S_E_LEAD','S_E_TRAFFIC','S_E_RESP','S_E_CANCER','S_E_DIESEL','S_E_O3','S_E_PM25',
# 'S_D_MINOR','S_D_INCOME','S_D_LESSHS','S_D_LING','S_D_UNDER5','S_D_OVER64'
# ) # The other 2016 format, from last contractor fast batch
# tool not ESRI batch processing tool, I think was this: #
# '','FACID','FACID','LAT','LON','totpop','buff','stabbr','statename','region','RAW_E_PM25','S_E_PM25_PER','R_E_PM25_PER','N_E_PM25_PER','S_E_PM25','R_E_PM25','N_E_PM25','RAW_E_O3','S_E_O3_PER','R_E_O3_PER','N_E_O3_PER','S_E_O3','R_E_O3','N_E_O3','RAW_E_DIESEL','S_E_DIESEL_PER','R_E_DIESEL_PER','N_E_DIESEL_PER','S_E_DIESEL','R_E_DIESEL','N_E_DIESEL','RAW_E_CANCER','S_E_CANCER_PER','R_E_CANCER_PER','N_E_CANCER_PER','S_E_CANCER','R_E_CANCER','N_E_CANCER','RAW_E_RESP','S_E_RESP_PER','R_E_RESP_PER','N_E_RESP_PER','S_E_RESP','R_E_RESP','N_E_RESP','RAW_E_TRAFFIC','S_E_TRAFFIC_PER','R_E_TRAFFIC_PER','N_E_TRAFFIC_PER','S_E_TRAFFIC','R_E_TRAFFIC','N_E_TRAFFIC','RAW_E_LEAD','S_E_LEAD_PER','R_E_LEAD_PER','N_E_LEAD_PER','S_E_LEAD','R_E_LEAD','N_E_LEAD','RAW_E_NPL','S_E_NPL_PER','R_E_NPL_PER','N_E_NPL_PER','S_E_NPL','R_E_NPL','N_E_NPL','RAW_E_RMP','S_E_RMP_PER','R_E_RMP_PER','N_E_RMP_PER','S_E_RMP','R_E_RMP','N_E_RMP','RAW_E_TSDF','S_E_TSDF_PER','R_E_TSDF_PER','N_E_TSDF_PER','S_E_TSDF','R_E_TSDF','N_E_TSDF','RAW_E_NPDES','S_E_NPDES_PER','R_E_NPDES_PER','N_E_NPDES_PER','S_E_NPDES','R_E_NPDES','N_E_NPDES','RAW_D_INDEX','S_D_INDEX_PER','R_D_INDEX_PER','N_D_INDEX_PER','S_D_INDEX','R_D_INDEX','N_D_INDEX','RAW_D_MINOR','S_D_MINOR_PER','R_D_MINOR_PER','N_D_MINOR_PER','S_D_MINOR','R_D_MINOR','N_D_MINOR','RAW_D_INCOME','S_D_INCOME_PER','R_D_INCOME_PER','N_D_INCOME_PER','S_D_INCOME','R_D_INCOME','N_D_INCOME','RAW_D_LING','S_D_LING_PER','R_D_LING_PER','N_D_LING_PER','S_D_LING','R_D_LING','N_D_LING','RAW_D_LESSHS','S_D_LESSHS_PER','R_D_LESSHS_PER','N_D_LESSHS_PER','S_D_LESSHS','R_D_LESSHS','N_D_LESSHS','RAW_D_UNDER5','S_D_UNDER5_PER','R_D_UNDER5_PER','N_D_UNDER5_PER','S_D_UNDER5','R_D_UNDER5','N_D_UNDER5','RAW_D_OVER64','S_D_OVER64_PER','R_D_OVER64_PER','N_D_OVER64_PER','S_D_OVER64','R_D_OVER64','N_D_OVER64','N_P_NPDES','N_P_TSDF','N_P_RMP','N_P_NPL','N_P_LEAD','N_P_TRAFFIC','N_P_RESP','N_P_CANCER','N_P_DIESEL','N_P_O3','N_P_PM25','R_P_NPDES','R_P_TSDF','R_P_RMP','R_P_NPL','R_P_LEAD','R_P_TRAFFIC','R_P_RESP','R_P_CANCER','R_P_DIESEL','R_P_O3','R_P_PM25','S_P_NPDES','S_P_TSDF','S_P_RMP','S_P_NPL','S_P_LEAD','S_P_TRAFFIC','S_P_RESP','S_P_CANCER','S_P_DIESEL','S_P_O3','S_P_PM25'
# #
# '1',1,1,36.263333,-98.480833,1961.40424097441,NA,'OK','Oklahoma',6,'8.03',1.77771128179981,25,21,'11.0','98.0','35.9','56.1',322.246969303584,13.7462551,91,'50.0','76.0','59.0','0.203',0.182134659435296,6.3588859,7,'78.0','65.0','65.0','34.',0.0724048684886795,4.5393877,28,'27.','39.','100.','1.3',0.547464297686105,13.6746759,29,'91.','50.','99.','31.',0.0745478729955391,34.5409178,33,'40.','51.','100.','0.43',0.413336006369685,37.2317588,71,'0','9.0','99.','0',44.5587502520971,9.31803123693269,16,'29.','13.','100.','1.1',43,47.4094320438217,89,'12.','92.','100.','0.029',58,0.936750327315794,39,'7.2','8.0','100.','0',100,40.0308416122907,1,'6.0','12.','100.',25,99,1.83805363064454,41,'17.','22.','97.',11,42.3909505224415,1,26,'4.0','100.','0.31',39,0.72036093334196,39,61,'14.','100.','0.072',1,9.45314368072279,26,48,5,100,0,14,46.9739301923974,85,62,42,100,0,8,49.6663627814666,85,74,6,100,0,16,39.4511377227276,33,69,'51.','100.','590.',61,61,69,67,100,100,100,100,100,100,100,100,100,72,57,35.4792282684122,0.220137577563249,0.0659186952810612,0.383558587739272,0.0435088811568746,0.253614939269496,83.0005230043116,1.92944399583553,44.8221775748263,0.5259545310825,55.7806552664324,9.36848012374696,32.231684346941,38.7267721898835,13.3065062074557,2.15037821086161,6.93577727960583,13.9850441926119
# names1 <- c('','FACID','FACID','LAT','LON',
# 'totpop','buff', 'stabbr','statename','region',
# 'RAW_E_PM25','S_E_PM25_PER','R_E_PM25_PER','N_E_PM25_PER','S_E_PM25','R_E_PM25','N_E_PM25','RAW_E_O3','S_E_O3_PER','R_E_O3_PER','N_E_O3_PER','S_E_O3','R_E_O3','N_E_O3','RAW_E_DIESEL','S_E_DIESEL_PER','R_E_DIESEL_PER','N_E_DIESEL_PER','S_E_DIESEL','R_E_DIESEL','N_E_DIESEL','RAW_E_CANCER','S_E_CANCER_PER','R_E_CANCER_PER','N_E_CANCER_PER','S_E_CANCER','R_E_CANCER','N_E_CANCER','RAW_E_RESP','S_E_RESP_PER','R_E_RESP_PER','N_E_RESP_PER','S_E_RESP','R_E_RESP','N_E_RESP','RAW_E_TRAFFIC','S_E_TRAFFIC_PER','R_E_TRAFFIC_PER','N_E_TRAFFIC_PER','S_E_TRAFFIC','R_E_TRAFFIC','N_E_TRAFFIC','RAW_E_LEAD','S_E_LEAD_PER','R_E_LEAD_PER','N_E_LEAD_PER','S_E_LEAD','R_E_LEAD','N_E_LEAD','RAW_E_NPL','S_E_NPL_PER','R_E_NPL_PER','N_E_NPL_PER','S_E_NPL','R_E_NPL','N_E_NPL','RAW_E_RMP','S_E_RMP_PER','R_E_RMP_PER','N_E_RMP_PER','S_E_RMP','R_E_RMP','N_E_RMP','RAW_E_TSDF','S_E_TSDF_PER','R_E_TSDF_PER','N_E_TSDF_PER','S_E_TSDF','R_E_TSDF','N_E_TSDF','RAW_E_NPDES','S_E_NPDES_PER','R_E_NPDES_PER','N_E_NPDES_PER','S_E_NPDES','R_E_NPDES','N_E_NPDES','RAW_D_INDEX','S_D_INDEX_PER','R_D_INDEX_PER','N_D_INDEX_PER','S_D_INDEX','R_D_INDEX','N_D_INDEX','RAW_D_MINOR','S_D_MINOR_PER','R_D_MINOR_PER','N_D_MINOR_PER','S_D_MINOR','R_D_MINOR','N_D_MINOR','RAW_D_INCOME','S_D_INCOME_PER','R_D_INCOME_PER','N_D_INCOME_PER','S_D_INCOME','R_D_INCOME','N_D_INCOME','RAW_D_LING','S_D_LING_PER','R_D_LING_PER','N_D_LING_PER','S_D_LING','R_D_LING','N_D_LING','RAW_D_LESSHS','S_D_LESSHS_PER','R_D_LESSHS_PER','N_D_LESSHS_PER','S_D_LESSHS','R_D_LESSHS','N_D_LESSHS','RAW_D_UNDER5','S_D_UNDER5_PER','R_D_UNDER5_PER','N_D_UNDER5_PER','S_D_UNDER5','R_D_UNDER5','N_D_UNDER5','RAW_D_OVER64','S_D_OVER64_PER','R_D_OVER64_PER','N_D_OVER64_PER','S_D_OVER64','R_D_OVER64','N_D_OVER64','N_P_NPDES','N_P_TSDF','N_P_RMP','N_P_NPL','N_P_LEAD','N_P_TRAFFIC','N_P_RESP','N_P_CANCER','N_P_DIESEL','N_P_O3','N_P_PM25','R_P_NPDES','R_P_TSDF','R_P_RMP','R_P_NPL','R_P_LEAD','R_P_TRAFFIC','R_P_RESP','R_P_CANCER','R_P_DIESEL','R_P_O3','R_P_PM25','S_P_NPDES','S_P_TSDF','S_P_RMP','S_P_NPL','S_P_LEAD','S_P_TRAFFIC','S_P_RESP','S_P_CANCER','S_P_DIESEL','S_P_O3','S_P_PM25')
# dat1 <-
# c('1',1,1,36.263333,-98.480833,1961.40424097441,NA,'OK','Oklahoma',6,'8.03',1.77771128179981,25,21,'11.0','98.0','35.9','56.1',322.246969303584,13.7462551,91,'50.0','76.0','59.0','0.203',0.182134659435296,6.3588859,7,'78.0','65.0','65.0','34.',0.0724048684886795,4.5393877,28,'27.','39.','100.','1.3',0.547464297686105,13.6746759,29,'91.','50.','99.','31.',0.0745478729955391,34.5409178,33,'40.','51.','100.','0.43',0.413336006369685,37.2317588,71,'0','9.0','99.','0',44.5587502520971,9.31803123693269,16,'29.','13.','100.','1.1',43,47.4094320438217,89,'12.','92.','100.','0.029',58,0.936750327315794,39,'7.2','8.0','100.','0',100,40.0308416122907,1,'6.0','12.','100.',25,99,1.83805363064454,41,'17.','22.','97.',11,42.3909505224415,1,26,'4.0','100.','0.31',39,0.72036093334196,39,61,'14.','100.','0.072',1,9.45314368072279,26,48,5,100,0,14,46.9739301923974,85,62,42,100,0,8,49.6663627814666,85,74,6,100,0,16,39.4511377227276,33,69,'51.','100.','590.',61,61,69,67,100,100,100,100,100,100,100,100,100,72,57,35.4792282684122,0.220137577563249,0.0659186952810612,0.383558587739272,0.0435088811568746,0.253614939269496,83.0005230043116,1.92944399583553,44.8221775748263,0.5259545310825,55.7806552664324,9.36848012374696,32.231684346941,38.7267721898835,13.3065062074557,2.15037821086161,6.93577727960583,13.9850441926119)


# colnames in output of nonshiny tool as delivered july 2016
# (once rownames col removed) BUT FIXED MY COPY SO IT
# PROVIDES OBJECTID, THEN FACID [1] 'FACID' 'FACID.1' 'LAT'
# 'LON' [5] 'totpop' 'buff' 'stabbr' 'statename' [9] 'region'
# 'RAW_E_PM25' 'S_E_PM25_PER' 'R_E_PM25_PER' [13]
# 'N_E_PM25_PER' 'S_E_PM25' 'R_E_PM25' 'N_E_PM25' [17]
# 'RAW_E_O3' 'S_E_O3_PER' 'R_E_O3_PER' 'N_E_O3_PER' [21]
# 'S_E_O3' 'R_E_O3' 'N_E_O3' 'RAW_E_DIESEL' [25]
# 'S_E_DIESEL_PER' 'R_E_DIESEL_PER' 'N_E_DIESEL_PER'
# 'S_E_DIESEL' [29] 'R_E_DIESEL' 'N_E_DIESEL' 'RAW_E_CANCER'
# 'S_E_CANCER_PER' [33] 'R_E_CANCER_PER' 'N_E_CANCER_PER'
# 'S_E_CANCER' 'R_E_CANCER' [37] 'N_E_CANCER' 'RAW_E_NEURO'
# 'S_E_NEURO_PER' 'R_E_NEURO_PER' [41] 'N_E_NEURO_PER'
# 'S_E_NEURO' 'R_E_NEURO' 'N_E_NEURO' [45] 'RAW_E_RESP'
# 'S_E_RESP_PER' 'R_E_RESP_PER' 'N_E_RESP_PER' [49]
# 'S_E_RESP' 'R_E_RESP' 'N_E_RESP' 'RAW_E_TRAFFIC' [53]
# 'S_E_TRAFFIC_PER' 'R_E_TRAFFIC_PER' 'N_E_TRAFFIC_PER'
# 'S_E_TRAFFIC' [57] 'R_E_TRAFFIC' 'N_E_TRAFFIC' 'RAW_E_LEAD'
# 'S_E_LEAD_PER' [61] 'R_E_LEAD_PER' 'N_E_LEAD_PER'
# 'S_E_LEAD' 'R_E_LEAD' [65] 'N_E_LEAD' 'RAW_E_NPL'
# 'S_E_NPL_PER' 'R_E_NPL_PER' [69] 'N_E_NPL_PER' 'S_E_NPL'
# 'R_E_NPL' 'N_E_NPL' [73] 'RAW_E_RMP' 'S_E_RMP_PER'
# 'R_E_RMP_PER' 'N_E_RMP_PER' [77] 'S_E_RMP' 'R_E_RMP'
# 'N_E_RMP' 'RAW_E_TSDF' [81] 'S_E_TSDF_PER' 'R_E_TSDF_PER'
# 'N_E_TSDF_PER' 'S_E_TSDF' [85] 'R_E_TSDF' 'N_E_TSDF'
# 'RAW_E_NPDES' 'S_E_NPDES_PER' [89] 'R_E_NPDES_PER'
# 'N_E_NPDES_PER' 'S_E_NPDES' 'R_E_NPDES' [93] 'N_E_NPDES'
# 'RAW_D_INDEX' 'S_D_INDEX_PER' 'R_D_INDEX_PER' [97]
# 'N_D_INDEX_PER' 'S_D_INDEX' 'R_D_INDEX' 'N_D_INDEX' [101]
# 'RAW_D_MINOR' 'S_D_MINOR_PER' 'R_D_MINOR_PER'
# 'N_D_MINOR_PER' [105] 'S_D_MINOR' 'R_D_MINOR' 'N_D_MINOR'
# 'RAW_D_INCOME' [109] 'S_D_INCOME_PER' 'R_D_INCOME_PER'
# 'N_D_INCOME_PER' 'S_D_INCOME' [113] 'R_D_INCOME'
# 'N_D_INCOME' 'RAW_D_LING' 'S_D_LING_PER' [117]
# 'R_D_LING_PER' 'N_D_LING_PER' 'S_D_LING' 'R_D_LING' [121]
# 'N_D_LING' 'RAW_D_LESSHS' 'S_D_LESSHS_PER' 'R_D_LESSHS_PER'
# [125] 'N_D_LESSHS_PER' 'S_D_LESSHS' 'R_D_LESSHS'
# 'N_D_LESSHS' [129] 'RAW_D_UNDER5' 'S_D_UNDER5_PER'
# 'R_D_UNDER5_PER' 'N_D_UNDER5_PER' [133] 'S_D_UNDER5'
# 'R_D_UNDER5' 'N_D_UNDER5' 'RAW_D_OVER64' [137]
# 'S_D_OVER64_PER' 'R_D_OVER64_PER' 'N_D_OVER64_PER'
# 'S_D_OVER64' [141] 'R_D_OVER64' 'N_D_OVER64'

#########################'########################'###################### #
#
# 2015 
#
# Required format for input file (***for 2015 version): ***
#
# These seem to be all out of order ***:

batchfields2015 <- c(
  "OBJECTID", "FACID", "NAME", "LAT", "LON", 
  "totpop", "buff", "stabbr", "statename", "region", "S_E_TSDF_PER", 
  "R_P_TRAFFIC", "S_E_PM25_PER", "R_P_CANCER", "S_P_DIESEL", 
  "N_D_INDEX", "RAW_E_RMP", "R_E_PM25", "R_D_LESSHS", "R_E_DIESEL", 
  "RAW_D_OVER64", "N_E_TSDF", "R_E_LEAD_PER", "R_E_RMP_PER", 
  "S_E_DIESEL_PER", "RAW_E_RESP", "R_D_INDEX_PER", "RAW_D_LESSHS", 
  "N_E_TRAFFIC", "S_E_NEURO_PER", "N_P_NPL", "S_D_INDEX", "S_D_MINOR", 
  "S_D_LESSHS", "S_P_RESP", "N_E_PM25_PER", "RAW_D_INDEX", 
  "N_E_NEURO_PER", "RAW_D_UNDER5", "RAW_E_LEAD", "R_E_NPL_PER", 
  "S_E_RESP_PER", "S_E_O3_PER", "N_P_PM25", "S_D_LESSHS_PER", 
  "N_E_DIESEL_PER", "S_D_INCOME_PER", "RAW_E_NPL", "R_D_MINOR_PER", 
  "S_E_TRAFFIC", "R_P_TSDF", "RAW_E_TSDF", "N_P_CANCER", "RAW_E_NEURO", 
  "S_E_DIESEL", "RAW_D_INCOME", "N_P_RMP", "N_E_O3_PER", "S_E_O3", 
  "R_E_RESP", "S_E_RESP", "N_E_DIESEL", "N_D_INDEX_PER", "N_E_RMP_PER", 
  "RAW_D_MINOR", "N_E_CANCER_PER", "R_E_O3_PER", "S_D_INDEX_PER", 
  "N_E_RMP", "R_P_LEAD", "R_E_NEURO", "N_E_LEAD", "S_E_RMP_PER", 
  "R_E_RMP", "RAW_E_DIESEL", "R_D_LING_PER", "R_E_TRAFFIC", 
  "R_E_LEAD", "R_D_OVER64_PER", "N_P_NEURO", "R_E_CANCER_PER", 
  "R_E_NPDES_PER", "N_E_CANCER", "N_D_MINOR_PER", "S_E_TSDF", 
  "S_E_NPL", "R_D_OVER64", "S_D_MINOR_PER", "S_P_TSDF", "S_P_RMP", 
  "N_E_PM25", "R_E_TSDF", "S_E_RMP", "RAW_D_LING", "S_E_TRAFFIC_PER", 
  "S_P_PM25", "S_E_LEAD", "R_P_NEURO", "S_D_LING", "N_E_NPL", 
  "R_E_DIESEL_PER", "R_D_LESSHS_PER", "R_P_O3", "N_E_TRAFFIC_PER", 
  "RAW_E_NPDES", "N_E_NPDES", "N_E_NEURO", "R_P_DIESEL", "N_E_RESP_PER", 
  "R_E_TSDF_PER", "RAW_E_TRAFFIC", "R_D_INDEX", "R_P_PM25", 
  "N_D_UNDER5_PER", "N_D_LESSHS_PER", "R_E_NPDES", "N_D_LING", 
  "S_E_PM25", "N_E_NPL_PER", "R_E_NEURO_PER", "R_D_MINOR", 
  "N_P_TSDF", "S_D_LING_PER", "R_P_NPL", "S_P_NPDES", "S_E_NPDES_PER", 
  "N_D_UNDER5", "S_E_NPL_PER", "S_E_CANCER_PER", "N_E_RESP", 
  "N_D_LESSHS", "S_D_UNDER5", "N_P_LEAD", "RAW_E_CANCER", "S_P_TRAFFIC", 
  "N_E_NPDES_PER", "R_E_TRAFFIC_PER", "N_P_NPDES", "RAW_E_O3", 
  "N_P_O3", "R_E_O3", "N_E_O3", "N_E_TSDF_PER", "R_E_RESP_PER", 
  "S_D_OVER64", "N_D_INCOME", "R_E_NPL", "R_D_UNDER5_PER", 
  "R_P_RESP", "R_P_NPDES", "S_P_O3", "N_P_DIESEL", "N_D_OVER64_PER", 
  "R_P_RMP", "N_P_TRAFFIC", "N_E_LEAD_PER", "S_E_NPDES", "S_D_OVER64_PER", 
  "S_P_NPL", "N_D_MINOR", "RAW_E_PM25", "N_D_LING_PER", "S_D_INCOME", 
  "S_P_NEURO", "N_P_RESP", "N_D_OVER64", "S_D_UNDER5_PER", 
  "R_D_LING", "R_E_CANCER", "S_E_CANCER", "S_P_CANCER", "N_D_INCOME_PER", 
  "R_D_INCOME_PER", "S_E_NEURO", "R_D_INCOME", "R_E_PM25_PER", 
  "R_D_UNDER5", "S_E_LEAD_PER", "S_P_LEAD")
# #
# OBJECTID,FACID,NAME,LAT,LON,totpop,buff,stabbr,statename,region,S_E_TSDF_PER,R_P_TRAFFIC,S_E_PM25_PER,R_P_CANCER,S_P_DIESEL,N_D_INDEX,RAW_E_RMP,R_E_PM25,R_D_LESSHS,R_E_DIESEL,RAW_D_OVER64,N_E_TSDF,R_E_LEAD_PER,R_E_RMP_PER,S_E_DIESEL_PER,RAW_E_RESP,R_D_INDEX_PER,RAW_D_LESSHS,N_E_TRAFFIC,S_E_NEURO_PER,N_P_NPL,S_D_INDEX,S_D_MINOR,S_D_LESSHS,S_P_RESP,N_E_PM25_PER,RAW_D_INDEX,N_E_NEURO_PER,RAW_D_UNDER5,RAW_E_LEAD,R_E_NPL_PER,S_E_RESP_PER,S_E_O3_PER,N_P_PM25,S_D_LESSHS_PER,N_E_DIESEL_PER,S_D_INCOME_PER,RAW_E_NPL,R_D_MINOR_PER,S_E_TRAFFIC,R_P_TSDF,RAW_E_TSDF,N_P_CANCER,RAW_E_NEURO,S_E_DIESEL,RAW_D_INCOME,N_P_RMP,N_E_O3_PER,S_E_O3,R_E_RESP,S_E_RESP,N_E_DIESEL,N_D_INDEX_PER,N_E_RMP_PER,RAW_D_MINOR,N_E_CANCER_PER,R_E_O3_PER,S_D_INDEX_PER,N_E_RMP,R_P_LEAD,R_E_NEURO,N_E_LEAD,S_E_RMP_PER,R_E_RMP,RAW_E_DIESEL,R_D_LING_PER,R_E_TRAFFIC,R_E_LEAD,R_D_OVER64_PER,N_P_NEURO,R_E_CANCER_PER,R_E_NPDES_PER,N_E_CANCER,N_D_MINOR_PER,S_E_TSDF,S_E_NPL,R_D_OVER64,S_D_MINOR_PER,S_P_TSDF,S_P_RMP,N_E_PM25,R_E_TSDF,S_E_RMP,RAW_D_LING,S_E_TRAFFIC_PER,S_P_PM25,S_E_LEAD,R_P_NEURO,S_D_LING,N_E_NPL,R_E_DIESEL_PER,R_D_LESSHS_PER,R_P_O3,N_E_TRAFFIC_PER,RAW_E_NPDES,N_E_NPDES,N_E_NEURO,R_P_DIESEL,N_E_RESP_PER,R_E_TSDF_PER,RAW_E_TRAFFIC,R_D_INDEX,R_P_PM25,N_D_UNDER5_PER,N_D_LESSHS_PER,R_E_NPDES,N_D_LING,S_E_PM25,N_E_NPL_PER,R_E_NEURO_PER,R_D_MINOR,N_P_TSDF,S_D_LING_PER,R_P_NPL,S_P_NPDES,S_E_NPDES_PER,N_D_UNDER5,S_E_NPL_PER,S_E_CANCER_PER,N_E_RESP,N_D_LESSHS,S_D_UNDER5,N_P_LEAD,RAW_E_CANCER,S_P_TRAFFIC,N_E_NPDES_PER,R_E_TRAFFIC_PER,N_P_NPDES,RAW_E_O3,N_P_O3,R_E_O3,N_E_O3,N_E_TSDF_PER,R_E_RESP_PER,S_D_OVER64,N_D_INCOME,R_E_NPL,R_D_UNDER5_PER,R_P_RESP,R_P_NPDES,S_P_O3,N_P_DIESEL,N_D_OVER64_PER,R_P_RMP,N_P_TRAFFIC,N_E_LEAD_PER,S_E_NPDES,S_D_OVER64_PER,S_P_NPL,N_D_MINOR,RAW_E_PM25,N_D_LING_PER,S_D_INCOME,S_P_NEURO,N_P_RESP,N_D_OVER64,S_D_UNDER5_PER,R_D_LING,R_E_CANCER,S_E_CANCER,S_P_CANCER,N_D_INCOME_PER,R_D_INCOME_PER,S_E_NEURO,R_D_INCOME,R_E_PM25_PER,R_D_UNDER5,S_E_LEAD_PER,S_P_LEAD
# # 1,1000,BRASW Facility,32.4,-94.7,'7,946',3 miles,TX,
# Texas,6,97,74,44,75,61,35%,1.2,9.44,19%,0.733,13%,0.054,74,92,37,1.5,72,23%,110,99,95,47%,55%,20%,66,27,61%,92,7%,0.27,97,54,51,77,64,49,70,0.29,69,91,96,0.33,80,0.12,0.913,52%,95,34,42.9,1.4,1.5,0.824,83,95,69%,72,47,68,0.31,84,0.043,0.3,91,0.42,0.478,66,81,0.18,63,90,87,73,49,79,0.073,0.067,11%,62,94,86,10.7,0.062,0.47,6%,60,61,0.17,92,9%,0.096,50,67,68,60,0.32,0.25,0.063,70,43,97,55,44%,68,62,78,0.35,5%,9.63,94,99,49%,97,56,95,74,71,7%,97,84,2.3,15%,8%,84,56,69,81,64,89,43.8,79,43.6,46.3,98,63,10%,34%,0.063,52,73,79,62,78,55,89,80,57,0.38,69,93,36%,9.44,74,39%,89,76,13%,49,7%,42,44,68,79,71,0.044,39%,47,7%,76,82







#' ####################################################################################
# FIELDNAMES USED BY CODE TO REFER TO SPECIFIC TYPES OF
# FIELDS THIS COULD BE REPLACED BY JUST REQUIRING THE
# RELEVANT GROUPS OF NAMES BE PROVIDED IN A PARTICULAR SORT
# ORDER IN THE newnames COLUMN OF THE LOOKUP TABLE (CSV READ
# IN AS mynamesfile.default or user-uploaded version of that)
# 
# AS WRITTEN CURRENTLY, THE non-friendly versions of the
# NAMES BELOW *MUST* MATCH THOSE IN THE newnames COLUMN OF
# THE LOOKUP TABLE and the friendly versions are read from
# the csv file for table outputs but from the defaults below
# for use in the graphics (hist/bar) labeling I believe.
#' ####################################################################################

############################################################### #
############################################################### #

####### MORE NOTES 

# Only some of these maps between versions of field names
# could be handled using the ejscreen package now, via
# data('ejscreenformulas'), with e.g., gdbfieldname 'CANCER'
# Rfieldname 'cancer' ***** acsfieldname NA type
# 'Environmental' glossaryfieldname 'Air toxics cancer risk'
# ***** e.g., change.fieldnames(ejscreenformulas$) BUT, Many
# fields used just for batch processing outputs are not in
# ejscreenformulas at all, such as oldnames newnames
# gdbfieldname 1 OBJECTID OBJECTID OBJECTID ...  12
# RAW_D_INDEX VSI.eo VULEOPCT 13 N_D_INDEX us.avg.VSI.eo <NA>
# 14 R_D_INDEX region.avg.VSI.eo <NA> 15 S_D_INDEX
# state.avg.VSI.eo <NA> 16 N_D_INDEX_PER pctile.VSI.eo
# P_VULEOPCT 17 R_D_INDEX_PER region.pctile.VSI.eo <NA> 18
# S_D_INDEX_PER state.pctile.VSI.eo <NA> 19 RAW_D_INCOME
# pctlowinc LOWINCPCT ...

# Also note not all the fields in lookup.fieldnames() are in
# names.all below.

# *** after reading defaults or user versions from the
# default or specified map names file - Read into the
# reactive lookup.fieldnames() BUT note these friendly names
# aren't same as longname in default lookup.fieldnames() and
# also they lack full unique name like 'Ozone US percentile'
# or 'Cancer risk state average' The ones below are short and
# friendly for graph labels.

# *** if using names from data(names.evars,
# package='ejscreen') etc., >
# ejscreenformulas$glossaryfieldname[match(names.d ,
# ejscreenformulas$Rfieldname)] [1] 'Demographic Index (based
# on 2 factors, % low-income and % minority)' [2]
# 'Supplementary Demographic Index (based on 6 factors)' [3]
# '% minority' [4] '% low-income' [5] '% less than high
# school' [6] '% of households (interpreted as individuals)
# in linguistic isolation' [7] '% under age 5' [8] '% over
# age 64' > ejscreenformulas$glossaryfieldname[match(names.e,
# ejscreenformulas$Rfieldname)] [1] 'PM2.5 level in air' [2]
# 'Ozone level in air' [3] 'Air toxics cancer risk' [4] 'Air
# toxics neurological hazard index' [5] 'Air toxics
# respiratory hazard index' [6] 'Diesel particulate matter
# level in air' [7] '% pre-1960 housing (lead paint
# indicator)' [8] 'Traffic proximity and volume' [9]
# 'Proximity to National Priorities List (NPL) sites' [10]
# 'Proximity to Risk Management Plan (RMP) facilities' [11]
# 'Proximity to Treatment Storage and Disposal (TSDF)
# facilities' [12] 'Proximity to major direct dischargers to
# water' > ejscreenformulas$glossaryfieldname[match(names.ej,
# ejscreenformulas$Rfieldname)] [1] 'EJ Index for PM2.5 level
# in air' [2] 'EJ Index for Ozone level in air' [3] 'EJ Index
# for Air toxics cancer risk' [4] 'EJ Index for Air toxics
# neurological hazard index' [5] 'EJ Index for Air toxics
# respiratory hazard index' [6] 'EJ Index for Diesel
# particulate matter level in air' [7] 'EJ Index for %
# pre-1960 housing (lead paint indicator)' [8] 'EJ Index for
# Traffic proximity and volume' [9] 'EJ Index for Proximity
# to National Priorities List (NPL) sites' [10] 'EJ Index for
# Proximity to Risk Management Plan (RMP) facilities' [11]
# 'EJ Index for Proximity to Treatment Storage and Disposal
# (TSDF) facilities' [12] 'EJ Index for Proximity to major
# direct dischargers to water'

# Roughly the lists of fieldnames below, names.e.batch ,
# names.d.batch , and names.ej.batch , are already available
# as data(names.evars, names.dvars, names.ejvars, package =
# 'ejscreen') BUT the names.d in ejscreen pkg has had
# VSI.svi6, which is removed here and for 2016 anyway and
# sort order differs here

# BUT THESE GLOSSARY NAMES WOULD BE TOO LONG FOR GRAPHICS:
# names.e.friendly <-
# ejscreenformulas$glossaryfieldname[match(names.e.batch ,
# ejscreenformulas$Rfieldname)] names.d.friendly <-
# ejscreenformulas$glossaryfieldname[match(names.d.batch ,
# ejscreenformulas$Rfieldname)] names.ej.friendly <-
# ejscreenformulas$glossaryfieldname[match(names.ej.batch ,
# ejscreenformulas$Rfieldname)]


############################################################### #
############################################################### #


##################### #
# ### TESTING / DEBUGGING ###
##################### #

if (testing) {
  options(shiny.reactlog = TRUE)  # If TRUE, enable logging of reactive events, which can be 
  # viewed later with the showReactLog function. This incurs a
  # substantial performance penalty and should not be used in
  # production.
  options(shiny.trace = TRUE)
  options(shiny.stacktraceoffset = TRUE)  # name of function appears next to srcref where defined not where it is being called from - more intuitive view?
  options(shiny.fullstacktrace = TRUE)  # instead of shorter prettier view
  options(shiny.error = browser)  # or options(shiny.error = recover) gives debugger prompt on err
  options(error = recover)  # The functions dump.frames and recover provide alternatives that allow post-mortem debugging. 
  options(verbose = TRUE)
  options(shiny.deprecation.messages = TRUE)
} else {
  options(shiny.reactlog = FALSE)
  options(shiny.trace = FALSE)
  options(shiny.stacktraceoffset = FALSE)  # name of function appears next to srcref where defined not where it is being called from - more intuitive view?
  options(shiny.fullstacktrace = FALSE)  # instead of shorter prettier view
  options(shiny.error = NULL)  # or options(shiny.error = recover) gives debugger prompt on err
  options(error = NULL)  # resets behavior to normal when error hit
  options(verbose = FALSE)
  options(shiny.deprecation.messages = FALSE)
}
on.exit({
  options(shiny.reactlog = FALSE)
  options(shiny.trace = FALSE)
  options(shiny.stacktraceoffset = FALSE)  # name of function appears next to srcref where defined not where it is being called from - more intuitive view?
  options(shiny.fullstacktrace = FALSE)  # instead of shorter prettier view
  options(shiny.error = NULL)  # or options(shiny.error = recover) gives debugger prompt on err
  options(error = NULL)  # resets behavior to normal when error hit
  options(verbose = FALSE)
  options(shiny.deprecation.messages = FALSE)
})
