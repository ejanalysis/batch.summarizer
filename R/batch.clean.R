#' @title Function that cleans up raw output of EJSCREEN batch processor
#' @description Takes the raw output version of batch buffer results and cleans it up
#'   to make it ready for batch.summarize function
#' @param x Required. The output of the batch processor that runs EJSCREEN buffer report once per site.
#' @param namesfile Must specify either namesfile, or both oldcolnames and newcolnames. A csv filename, of file that maps fieldnames from those found in raw output of batch processor 
#'   to more useful and friendly names that make more sense to me. Default had been 'map_batch_to_friendly_fieldnames_2016.csv'
#'   But may use 'map_batch_to_friendly_fieldnames_2018.csv' now.
#'   If function is called with the special value namesfile='keepnames' then the names are unchanged from those in x.
#' @param oldcolnames Optional. The names to be found in x, ignored if namesfile specified.
#' @param newcolnames Optional. The corresponding names to change them to, ignored if namesfile specified.
#' @author ejanalyst info@ejanalysis.com
#' @return the output is the output
#' @export
batch.clean <- function(x, namesfile='map_batch_to_friendly_fieldnames_2018.csv', oldcolnames, newcolnames) {

  x[x == 'NA'] <- NA  # it also fixes 'N/A' later 
  
  # namesfile='keepnames' is one way to specify user wants to keep same names.

  if (colnames(x)[1] == '') {colnames(x)[1] <- "hadnocolnameinheader"}
  
  if (missing(namesfile) & 1 == sum(missing(oldcolnames) + missing(newcolnames) )) {
    stop('must specify either namesfile, or both oldcolnames and newcolnames')
  }
  
  if (!missing(namesfile)) {
    if (!missing(oldcolnames) | !missing(newcolnames)) {warning('ignoring oldcolnames and newcolnames because namesfile was specified')}
    if (namesfile == 'keepnames') {
      # that is how user can specify they want no changes made to the names
    } else {
      x <- x[ , change.fieldnames(names(x), file = namesfile, sort = TRUE)]
      names(x) <- change.fieldnames(names(x), file = namesfile)
    }
  }
  
  if (missing(namesfile) & !missing(oldcolnames) & !missing(newcolnames) ) {
    
    # IMPROVE COLUMN ORDER
    x <- x[ , change.fieldnames(names(x), oldnames = oldcolnames, sort = TRUE)]
    # RENAME FIELDS TO FRIENDLIER NEW NAMES
    names(x) <- change.fieldnames(names(x), oldnames = oldcolnames, newnames = newcolnames)
  }
  
  # Problems? If rownames in file, no colname for 1st col so gets called X
  # FACID appearing twice in file, so 2d col named that gets called FACID.1
  # NEURO related indicators may still be in file, but neuro indicators were dropped in 2016 or so.
  # c("X", "FACID.1",
  # "RAW_E_NEURO", "S_E_NEURO_PER", "R_E_NEURO_PER", "N_E_NEURO_PER", "S_E_NEURO", "R_E_NEURO", "N_E_NEURO")
  # Might want to drop those fields in case they got created for some reason?
  x <- x[ , names(x) != "FACID.1"]
  x <- x[ , names(x) != "X"]
  x <- x[ , !grepl('NEURO', names(x))] 
  
  if (missing(namesfile) & missing(oldcolnames) & missing(newcolnames) ) {
    
    # use default fieldname changes if nothing is specified for column names
    #namesfile <- 'map batchtool to gdb to R fieldnames.csv' #'map batch to friendly fieldnames v1.csv'
    
    # IMPROVE COLUMN ORDER
    x <- x[ , change.fieldnames(names(x), file = namesfile, sort = TRUE)]
    names(x) <- change.fieldnames(names(x), file = namesfile)
    # names(x) <- change.fieldnames(names(x), oldnames=myoldnames, newnames=mynewnames)    
  }
  
  # try to convert fields from character to text by removing 
  # percent sign, comma, miles, and treat N/A as NA:
  makenum <- function(x) {as.data.frame( lapply(x, function(y) as.numeric(gsub('th', '', gsub('<', '', gsub(' miles', '', gsub('N/A','',gsub('%','', gsub(',','',y)) )))))) , stringsAsFactors = FALSE)}
  charcol <- names(x) %in% c('OBJECTID', 'FACID', 'name', 'ST', 'statename')
  x[ , !charcol] <- makenum(x[ , !charcol])
  if (!('OBJECTID' %in% names(x))) {x$OBJECTID <- 1:NROW(x)} # required by map and probably elsewhere
  return(x)
}

# ESRI batch tool was outputting this, I THINK? when? that's also like what is example on live ejanalysis.com:
 # OBJECTID,FACID,NAME,LAT,LON,totpop,buff,stabbr,statename,region,
#   S_E_TSDF_PER,R_P_TRAFFIC,S_E_PM25_PER,R_P_CANCER,S_P_DIESEL,N_D_INDEX,RAW_E_RMP,R_E_PM25,R_D_LESSHS,R_E_DIESEL,RAW_D_OVER64,N_E_TSDF,R_E_LEAD_PER,R_E_RMP_PER,S_E_DIESEL_PER,RAW_E_RESP,R_D_INDEX_PER,RAW_D_LESSHS,N_E_TRAFFIC,S_E_NEURO_PER,N_P_NPL,S_D_INDEX,S_D_MINOR,S_D_LESSHS,S_P_RESP,N_E_PM25_PER,RAW_D_INDEX,N_E_NEURO_PER,RAW_D_UNDER5,RAW_E_LEAD,R_E_NPL_PER,S_E_RESP_PER,S_E_O3_PER,N_P_PM25,S_D_LESSHS_PER,N_E_DIESEL_PER,S_D_INCOME_PER,RAW_E_NPL,R_D_MINOR_PER,S_E_TRAFFIC,R_P_TSDF,RAW_E_TSDF,N_P_CANCER,RAW_E_NEURO,S_E_DIESEL,RAW_D_INCOME,N_P_RMP,N_E_O3_PER,S_E_O3,R_E_RESP,S_E_RESP,N_E_DIESEL,N_D_INDEX_PER,N_E_RMP_PER,RAW_D_MINOR,N_E_CANCER_PER,R_E_O3_PER,S_D_INDEX_PER,N_E_RMP,R_P_LEAD,R_E_NEURO,N_E_LEAD,S_E_RMP_PER,R_E_RMP,RAW_E_DIESEL,R_D_LING_PER,R_E_TRAFFIC,R_E_LEAD,R_D_OVER64_PER,N_P_NEURO,R_E_CANCER_PER,R_E_NPDES_PER,N_E_CANCER,N_D_MINOR_PER,S_E_TSDF,S_E_NPL,R_D_OVER64,S_D_MINOR_PER,S_P_TSDF,S_P_RMP,N_E_PM25,R_E_TSDF,S_E_RMP,RAW_D_LING,S_E_TRAFFIC_PER,S_P_PM25,S_E_LEAD,R_P_NEURO,S_D_LING,N_E_NPL,R_E_DIESEL_PER,R_D_LESSHS_PER,R_P_O3,N_E_TRAFFIC_PER,RAW_E_NPDES,N_E_NPDES,N_E_NEURO,R_P_DIESEL,N_E_RESP_PER,R_E_TSDF_PER,RAW_E_TRAFFIC,R_D_INDEX,R_P_PM25,N_D_UNDER5_PER,N_D_LESSHS_PER,R_E_NPDES,N_D_LING,S_E_PM25,N_E_NPL_PER,R_E_NEURO_PER,R_D_MINOR,N_P_TSDF,S_D_LING_PER,R_P_NPL,S_P_NPDES,S_E_NPDES_PER,N_D_UNDER5,S_E_NPL_PER,S_E_CANCER_PER,N_E_RESP,N_D_LESSHS,S_D_UNDER5,N_P_LEAD,RAW_E_CANCER,S_P_TRAFFIC,N_E_NPDES_PER,R_E_TRAFFIC_PER,N_P_NPDES,RAW_E_O3,N_P_O3,R_E_O3,N_E_O3,N_E_TSDF_PER,R_E_RESP_PER,S_D_OVER64,N_D_INCOME,R_E_NPL,R_D_UNDER5_PER,R_P_RESP,R_P_NPDES,S_P_O3,N_P_DIESEL,N_D_OVER64_PER,R_P_RMP,N_P_TRAFFIC,N_E_LEAD_PER,S_E_NPDES,S_D_OVER64_PER,S_P_NPL,N_D_MINOR,RAW_E_PM25,N_D_LING_PER,S_D_INCOME,S_P_NEURO,N_P_RESP,N_D_OVER64,S_D_UNDER5_PER,R_D_LING,R_E_CANCER,S_E_CANCER,S_P_CANCER,N_D_INCOME_PER,R_D_INCOME_PER,S_E_NEURO,R_D_INCOME,R_E_PM25_PER,R_D_UNDER5,S_E_LEAD_PER,S_P_LEAD
# 1,1000,BRASW Facility,32.4,-94.7,"7,946",3 miles,TX,Texas,6,
#   97,74,44,75,61,35%,1.2,9.44,19%,0.733,13%,0.054,74,92,37,1.5,72,23%,110,99,95,47%,55%,20%,66,27,61%,92,7%,0.27,97,54,51,77,64,49,70,0.29,69,91,96,0.33,80,0.12,0.913,52%,95,34,42.9,1.4,1.5,0.824,83,95,69%,72,47,68,0.31,84,0.043,0.3,91,0.42,0.478,66,81,0.18,63,90,87,73,49,79,0.073,0.067,11%,62,94,86,10.7,0.062,0.47,6%,60,61,0.17,92,9%,0.096,50,67,68,60,0.32,0.25,0.063,70,43,97,55,44%,68,62,78,0.35,5%,9.63,94,99,49%,97,56,95,74,71,7%,97,84,2.3,15%,8%,84,56,69,81,64,89,43.8,79,43.6,46.3,98,63,10%,34%,0.063,52,73,79,62,78,55,89,80,57,0.38,69,93,36%,9.44,74,39%,89,76,13%,49,7%,42,44,68,79,71,0.044,39%,47,7%,76,82


# # default was to use the mapping in this file:  'map batch to friendly fieldnames v1.csv'
# setwd('C:/Users/xyz/Documents/GitHub/batch.summarizer')
# y = names(read.csv(file='Export_Output_Example2.csv', stringsAsFactors = FALSE))
# x=    read.csv('map batch to friendly fieldnames v1.csv', stringsAsFactors = FALSE)
# cbind(batchoutputnames=y, friendlynames=x$Rfieldname[match(y, x$batchname)]) 
#   batchoutputnames  friendlynames                                  
#   [1,] "OBJECTID"        "OBJECTID"                                     
#   [2,] "Registry_I"      "registryID"                                   
#   [3,] "OLDFACID"        "OLDFACID"                                     
#   [4,] "NAME"            "name"                                         
#   [5,] "LAT"             "lat"                                          
#   [6,] "LON"             "lon"                                          
#   [7,] "totpop"          "pop"                                          
#   [8,] "buff"            "radius.miles"                                 
#   [9,] "stabbr"          "ST"                                           
#   [10,] "statename"       "statename"                                    
#   [11,] "region"          "REGION"                                       
#   [12,] "S_E_TSDF_PER"    "state.pctile.proximity.tsdf"                  
#   [13,] "R_P_TRAFFIC"     "region.pctile.EJ.DISPARITY.traffic.score.eo"  
#   [14,] "S_E_PM25_PER"    "state.pctile.pm"                             
# ETC. ETC.

#     oldcolnames <- c("OBJECTID", 
#                      "Registry_I", "OLDFACID", "NAME", "LAT", "LON", 
#                      "totpop", "buff", "stabbr", "statename", "region", "S_E_TSDF_PER", 
#                      "R_P_TRAFFIC", "S_E_PM25_PER", "R_P_CANCER", "S_P_DIESEL", "N_D_INDEX", 
#                      "RAW_E_RMP", "R_E_PM25", "R_D_LESSHS", "R_E_DIESEL", "RAW_D_OVER64", 
#                      "N_E_TSDF", "R_E_LEAD_PER", "R_E_RMP_PER", "S_E_DIESEL_PER", 
#                      "RAW_E_RESP", "R_D_INDEX_PER", "RAW_D_LESSHS", "N_E_TRAFFIC", 
#                      "S_E_NEURO_PER", "N_P_NPL", "S_D_INDEX", "S_D_MINOR", "S_D_LESSHS", 
#                      "S_P_RESP", "N_E_PM25_PER", "RAW_D_INDEX", "N_E_NEURO_PER", "RAW_D_UNDER5", 
#                      "RAW_E_LEAD", "R_E_NPL_PER", "S_E_RESP_PER", "S_E_O3_PER", "N_P_PM25", 
#                      "S_D_LESSHS_PER", "N_E_DIESEL_PER", "S_D_INCOME_PER", "RAW_E_NPL", 
#                      "R_D_MINOR_PER", "S_E_TRAFFIC", "R_P_TSDF", "RAW_E_TSDF", "N_P_CANCER", 
#                      "RAW_E_NEURO", "S_E_DIESEL", "RAW_D_INCOME", "N_P_RMP", "N_E_O3_PER", 
#                      "S_E_O3", "R_E_RESP", "S_E_RESP", "N_E_DIESEL", "N_D_INDEX_PER", 
#                      "N_E_RMP_PER", "RAW_D_MINOR", "N_E_CANCER_PER", "R_E_O3_PER", 
#                      "S_D_INDEX_PER", "N_E_RMP", "R_P_LEAD", "R_E_NEURO", "N_E_LEAD", 
#                      "S_E_RMP_PER", "R_E_RMP", "RAW_E_DIESEL", "R_D_LING_PER", "R_E_TRAFFIC", 
#                      "R_E_LEAD", "R_D_OVER64_PER", "N_P_NEURO", "R_E_CANCER_PER", 
#                      "R_E_NPDES_PER", "N_E_CANCER", "N_D_MINOR_PER", "S_E_TSDF", "S_E_NPL", 
#                      "R_D_OVER64", "S_D_MINOR_PER", "S_P_TSDF", "S_P_RMP", "N_E_PM25", 
#                      "R_E_TSDF", "S_E_RMP", "RAW_D_LING", "S_E_TRAFFIC_PER", "S_P_PM25", 
#                      "S_E_LEAD", "R_P_NEURO", "S_D_LING", "N_E_NPL", "R_E_DIESEL_PER", 
#                      "R_D_LESSHS_PER", "R_P_O3", "N_E_TRAFFIC_PER", "RAW_E_NPDES", 
#                      "N_E_NPDES", "N_E_NEURO", "R_P_DIESEL", "N_E_RESP_PER", "R_E_TSDF_PER", 
#                      "RAW_E_TRAFFIC", "R_D_INDEX", "R_P_PM25", "N_D_UNDER5_PER", "N_D_LESSHS_PER", 
#                      "R_E_NPDES", "N_D_LING", "S_E_PM25", "N_E_NPL_PER", "R_E_NEURO_PER", 
#                      "R_D_MINOR", "N_P_TSDF", "S_D_LING_PER", "R_P_NPL", "S_P_NPDES", 
#                      "S_E_NPDES_PER", "N_D_UNDER5", "S_E_NPL_PER", "S_E_CANCER_PER", 
#                      "N_E_RESP", "N_D_LESSHS", "S_D_UNDER5", "N_P_LEAD", "RAW_E_CANCER", 
#                      "S_P_TRAFFIC", "N_E_NPDES_PER", "R_E_TRAFFIC_PER", "N_P_NPDES", 
#                      "RAW_E_O3", "N_P_O3", "R_E_O3", "N_E_O3", "N_E_TSDF_PER", "R_E_RESP_PER", 
#                      "S_D_OVER64", "N_D_INCOME", "R_E_NPL", "R_D_UNDER5_PER", "R_P_RESP", 
#                      "R_P_NPDES", "S_P_O3", "N_P_DIESEL", "N_D_OVER64_PER", "R_P_RMP", 
#                      "N_P_TRAFFIC", "N_E_LEAD_PER", "S_E_NPDES", "S_D_OVER64_PER", 
#                      "S_P_NPL", "N_D_MINOR", "RAW_E_PM25", "N_D_LING_PER", "S_D_INCOME", 
#                      "S_P_NEURO", "N_P_RESP", "N_D_OVER64", "S_D_UNDER5_PER", "R_D_LING", 
#                      "R_E_CANCER", "S_E_CANCER", "S_P_CANCER", "N_D_INCOME_PER", "R_D_INCOME_PER", 
#                      "S_E_NEURO", "R_D_INCOME", "R_E_PM25_PER", "R_D_UNDER5", "S_E_LEAD_PER", 
#                      "S_P_LEAD")
