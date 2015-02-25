batch.clean <- function(x, namesfile, oldcolnames, newcolnames) {
  
  # namesfile='keepnames' is one way to specify user wants to keep same names.

  if (missing(namesfile) & 1==sum(missing(oldcolnames) + missing(newcolnames) )) {
    stop('must specify either namesfile, or both oldcolnames and newcolnames')
  }
  
  if (!missing(namesfile)) {
    if (!missing(oldcolnames) | !missing(newcolnames)) {warning('ignoring oldcolnames and newcolnames because namesfile was specified')}
    if (namesfile=='keepnames') {
      # that is how user can specify they want no changes made to the names
    } else {
      x <- x[ , change.fieldnames(names(x), file=namesfile, sort=TRUE)]
      names(x) <- change.fieldnames(names(x), file=namesfile)
    }
  }
  
  if (missing(namesfile) & !missing(oldcolnames) & !missing(newcolnames) ) {
    
    # IMPROVE COLUMN ORDER
    x <- x[ , change.fieldnames(names(x), oldnames=oldcolnames, sort=TRUE)]
    # RENAME FIELDS TO FRIENDLIUER NEW NAMES
    names(x) <- change.fieldnames(names(x), oldnames=oldcolnames, newnames=newcolnames)
  }
  
  if (missing(namesfile) & missing(oldcolnames) & missing(newcolnames) ) {
    
    # use default fieldname changes if nothing is specified for column names
    namesfile <- 'map batchtool to gdb to R fieldnames.csv'
    
    # IMPROVE COLUMN ORDER
    x <- x[ , change.fieldnames(names(x), file=namesfile, sort=TRUE)]
    names(x) <- change.fieldnames(names(x), file=namesfile)
    # names(x) <- change.fieldnames(names(x), oldnames=myoldnames, newnames=mynewnames)    
  }
  
  # try to convert fields from character to text by removing percent sign, comma, miles, and treat N/A as NA:
  makenum <- function(x) {as.data.frame( lapply(x, function(y) as.numeric(gsub('th', '', gsub('<', '', gsub(' miles', '', gsub('N/A','',gsub('%','', gsub(',','',y)) )))))) , stringsAsFactors=FALSE)}
  charcol <- names(x) %in% c('OBJECTID', 'FACID', 'name', 'ST', 'statename')
  x[ , !charcol] <- makenum(x[ , !charcol])
  
  return(x)
}

# # default is to use the mapping in this file:  'map batchtool to gdb to R fieldnames.csv'
# setwd('~/Dropbox/......./batch summary')
# y= names( read.csv('SAMPLE OUTPUT OF BATCH TOOL 2015.csv', stringsAsFactors = FALSE) )
# x=    read.csv('map batchtool to gdb to R fieldnames.csv', stringsAsFactors = FALSE)
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
