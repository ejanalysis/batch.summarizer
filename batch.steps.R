####################################################
#  R CODE TO SUMMARIZE OUTPUT OF EJSCREEN BATCH RESULTS 
#  (RESULTS OF BATCH ARE IN A TEXT/CSV FILE WITH ONE ROW PER SITE/BUFFER)
#
# Mark A. Corrales, corrales.mark@epa.gov, 2015
#
####################################################

require(Hmisc) # can this just be left outside this function or does it matter?

# Require a package or just source the code needed:
# THESE FUNCTIONS MUST BE IN THE SHINY APP'S BASE DIRECTORY
#source('pop.ecdf.R')  # plot pop-wtd ecdf(s) for one demographic group vs others, etc.,  for comparing conditions between groups across many Census areal units (e.g. tracts)
#source('pct.above.R')         # returns percent of rows (or wtd people) that have value above specified cutoff (or mean as default), for each column of data.frame
#source('pct.below.R')         # returns percent of rows (or wtd people) that have value below specified cutoff (or mean as default), for each column of data.frame
#source('count.above.R')        # returns count of how many rows (or wtd people) have value above specified cutoff (or mean as default), for each column of data.frame
source('cols.above.count.R')  # returns count of how many cols (e.g. how many variables or indicators) have value above specified cutoff (or mean as default), for each row of data.frame
source('flagged.R')      # creates flag that is TRUE if at least one of 12 indicators is > cutoff (EJ index >80th %ile, for example), for each of many rows of a data.frame
source('rowMaxs.R')     # returns Max of each row
source('rowMins.R')     # returns Min of each row
source('colMaxs.R')     # returns Max of each col
source('colMins.R')     # returns Min of each col
source('wtd.colMeans.R')     # returns wtd.mean of each col
source('change.fieldnames.R')  # helps change fieldnames using a map file mapping old to new names

source('batch.read.R')
source('batch.clean.R')
source('batch.summarize.R')

#######################
# REPLACE THESE LINES WITH DYNAMIC USER INPUT (E.G. USING shinyapp):

# ALLOW USER TO UPLOAD mybatchfile:
mybatchfile <- 'Export_Output1.txt'
#'SAMPLE OUTPUT OF BATCH TOOL 2015.csv'

# possibly allow user to upload mynames file (map of old to new names, but my code may assume new friendly names at some point)
mynamesfile <- 'map batch to friendly fieldnames v1.csv'

probs <- c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1)
mythreshold=80
na.rm=TRUE
mywts <- fulltable$pop
mycolnames <- colnames(fulltable)
mythreshnames <- grep('^pctile.EJ.DISPARITY.', colnames(fulltable), value=TRUE)
# threshnames
# [1] "pctile.EJ.DISPARITY.proximity.npl.eo"   "pctile.EJ.DISPARITY.pm.eo"              "pctile.EJ.DISPARITY.cancer.eo"         
# [4] "pctile.EJ.DISPARITY.proximity.rmp.eo"   "pctile.EJ.DISPARITY.neuro.eo"           "pctile.EJ.DISPARITY.proximity.tsdf.eo" 
# [7] "pctile.EJ.DISPARITY.pctpre1960.eo"      "pctile.EJ.DISPARITY.proximity.npdes.eo" "pctile.EJ.DISPARITY.o3.eo"             
# [10] "pctile.EJ.DISPARITY.dpm.eo"             "pctile.EJ.DISPARITY.traffic.score.eo"   "pctile.EJ.DISPARITY.resp.eo"      

colfun.picked <- 'all' # later can be a logical vector but length must equal # of such funs defined as options in batch.summarize()
rowfun.picked <- 'all' # later can be a logical vector but length must equal # of such funs defined as options in batch.summarize()

#######################

# START THE SUMMARY

# read.csv used to read text file (csv format) that was exported from ArcGIS batch tool
fulltable <- batch.read(file=mybatchfile)

# check & rename columns to friendly names specified in namesfile map, reorder columns, etc.
fulltable <- batch.clean(fulltable, namesfile=mynamesfile)

 # source('batch.summarize.R')

# create some basic summary row, with summary stats of distribution across people (not sites. population-weighted.)
out <- batch.summarize(fulltable, wts=mywts, cols=mycolnames, threshnames=mythreshnames, threshold=mythreshold, probs=probs, na.rm=na.rm, colfun.picked=colfun.picked, rowfun.picked=rowfun.picked)
colsout <- out$cols
rowsout <- out$rows

####################################################
# Display & allow download of results

#   str(rowsout); str(colsout)
#  colsout[1:5 , ]
#  rowsout[ , 1:9]
# t( rowsout )
# t( fulltable[ 1:12 ,  grep('^pctile.EJ.DISPARITY', names(fulltable), value=TRUE)] )

# round(  t( rowsout[c('Average site', 'Average person') , ] ),0)

# to view one or two of the rows of summary stats:

# > dim(fulltable)
# [1]  42 179
# > dim(rowsout)
# [1]   8 179
# > dim(colsout)
# [1] 42  2


####################################################


####################################################
# NOTES - WORK IN PROGRESS
####################################################

#  ALSO SEE RELATED CODE HERE, TO BE INCORPORATED/MERGED AT SOME POINT:
# ~\Dropbox\EJSCREEN\R analysis\RR and other EJ stats for USA Region State\2013 RR CALC AND PCT WITH E ABOVE A STD-CUTOFF.R
# ~\Dropbox\EJSCREEN\R analysis\NATA STATE VS US PCTILES\ANALYZED and plot top bins for NATA etc.R

# original names:


# OBJECTID
# Registry_I
# OLDFACID
# NAME
# LAT
# LON
# totpop
# buff
# stabbr
# statename
# region
# S_E_TSDF_PER
# R_P_TRAFFIC
# S_E_PM25_PER
# R_P_CANCER
# S_P_DIESEL
# N_D_INDEX
# RAW_E_RMP
# R_E_PM25
# R_D_LESSHS
# R_E_DIESEL
# RAW_D_OVER64
# N_E_TSDF
# R_E_LEAD_PER
# R_E_RMP_PER
# S_E_DIESEL_PER
# RAW_E_RESP
# R_D_INDEX_PER
# RAW_D_LESSHS
# N_E_TRAFFIC
# S_E_NEURO_PER
# N_P_NPL
# S_D_INDEX
# S_D_MINOR
# S_D_LESSHS
# S_P_RESP
# N_E_PM25_PER
# RAW_D_INDEX
# N_E_NEURO_PER
# RAW_D_UNDER5
# RAW_E_LEAD
# R_E_NPL_PER
# S_E_RESP_PER
# S_E_O3_PER
# N_P_PM25
# S_D_LESSHS_PER
# N_E_DIESEL_PER
# S_D_INCOME_PER
# RAW_E_NPL
# R_D_MINOR_PER
# S_E_TRAFFIC
# R_P_TSDF
# RAW_E_TSDF
# N_P_CANCER
# RAW_E_NEURO
# S_E_DIESEL
# RAW_D_INCOME
# N_P_RMP
# N_E_O3_PER
# S_E_O3
# R_E_RESP
# S_E_RESP
# N_E_DIESEL
# N_D_INDEX_PER
# N_E_RMP_PER
# RAW_D_MINOR
# N_E_CANCER_PER
# R_E_O3_PER
# S_D_INDEX_PER
# N_E_RMP
# R_P_LEAD
# R_E_NEURO
# N_E_LEAD
# S_E_RMP_PER
# R_E_RMP
# RAW_E_DIESEL
# R_D_LING_PER
# R_E_TRAFFIC
# R_E_LEAD
# R_D_OVER64_PER
# N_P_NEURO
# R_E_CANCER_PER
# R_E_NPDES_PER
# N_E_CANCER
# N_D_MINOR_PER
# S_E_TSDF
# S_E_NPL
# R_D_OVER64
# S_D_MINOR_PER
# S_P_TSDF
# S_P_RMP
# N_E_PM25
# R_E_TSDF
# S_E_RMP
# RAW_D_LING
# S_E_TRAFFIC_PER
# S_P_PM25
# S_E_LEAD
# R_P_NEURO
# S_D_LING
# N_E_NPL
# R_E_DIESEL_PER
# R_D_LESSHS_PER
# R_P_O3
# N_E_TRAFFIC_PER
# RAW_E_NPDES
# N_E_NPDES
# N_E_NEURO
# R_P_DIESEL
# N_E_RESP_PER
# R_E_TSDF_PER
# RAW_E_TRAFFIC
# R_D_INDEX
# R_P_PM25
# N_D_UNDER5_PER
# N_D_LESSHS_PER
# R_E_NPDES
# N_D_LING
# S_E_PM25
# N_E_NPL_PER
# R_E_NEURO_PER
# R_D_MINOR
# N_P_TSDF
# S_D_LING_PER
# R_P_NPL
# S_P_NPDES
# S_E_NPDES_PER
# N_D_UNDER5
# S_E_NPL_PER
# S_E_CANCER_PER
# N_E_RESP
# N_D_LESSHS
# S_D_UNDER5
# N_P_LEAD
# RAW_E_CANCER
# S_P_TRAFFIC
# N_E_NPDES_PER
# R_E_TRAFFIC_PER
# N_P_NPDES
# RAW_E_O3
# N_P_O3
# R_E_O3
# N_E_O3
# N_E_TSDF_PER
# R_E_RESP_PER
# S_D_OVER64
# N_D_INCOME
# R_E_NPL
# R_D_UNDER5_PER
# R_P_RESP
# R_P_NPDES
# S_P_O3
# N_P_DIESEL
# N_D_OVER64_PER
# R_P_RMP
# N_P_TRAFFIC
# N_E_LEAD_PER
# S_E_NPDES
# S_D_OVER64_PER
# S_P_NPL
# N_D_MINOR
# RAW_E_PM25
# N_D_LING_PER
# S_D_INCOME
# S_P_NEURO
# N_P_RESP
# N_D_OVER64
# S_D_UNDER5_PER
# R_D_LING
# R_E_CANCER
# S_E_CANCER
# S_P_CANCER
# N_D_INCOME_PER
# R_D_INCOME_PER
# S_E_NEURO
# R_D_INCOME
# R_E_PM25_PER
# R_D_UNDER5
# S_E_LEAD_PER
# S_P_LEAD
