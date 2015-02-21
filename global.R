#############################
# DEFAULT VALUES, possibly could recode to allow user to change them, and/or read fieldnames from the csv file: 
#############################

threshold.default  <- list(80, 80, 80)  # a default for cutoff in at/above threshold stat summarizing EJ US percentiles

# threshnames.default <- list( 
#   grep('^pctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) , 
#   grep('regionpctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) , 
#   grep('statepctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) 
# )
# server.R creates threshold names this way at the moment:
# mythreshnames <- reactive({ grep('^pctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) })

threshgroup.default <- list('EJ US pctiles', 'EJ Region pctiles', 'EJ State pctiles')

mynamesfile.default <- 'map batch to friendly fieldnames v1.csv' # has default input and output and friendly fieldnames & var type & var category
mydemofile <- 'Export_Output_Example2.txt' # example of export of batch results, for use in testing/demonstrating summarizer

####################
# *** SPECIFY MORE PARAMETERS - USE DEFAULTS FOR NOW, POSSIBLY RECODE LATER TO LET USER CHANGE THESE 
# *** after reading defaults or user versions from the default or specified map names file - read into lookup.filenames() ***
# BUT note these friendly names aren't same as longname in default lookup.fieldnames() and 
# also they lack full unique name like 'Ozone US percentile' or 'Cancer risk state average'
# so might need paste(longname, vartype) for some friendly unique names, but the ones below for graph labels.
# Also note not all the fields in lookup.fieldnames() are in names.all below - these are just for plots.
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

####################

# which functions to use for summary stats
colfun.picked.default = 'all'
rowfun.picked.default = 'all'

probs.default.choices <- c('0','0.25','0.50','0.75','0.80','0.90','0.95','0.99','1.00')  # defaults for quantiles summary stats, used in ui
probs.default  <- as.numeric(probs.default.choices) # c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1)  # defaults for quantiles summary stats

na.rm=TRUE  # default for all functions so can get stats even if one site (or one indicator at one site) has no data

##########################################################################################
# MUST BE UPDATED WHEN NEW ACS DATA IS USED !
##########################################################################################

popus <-  309138711 # 309,138,711 is from http://factfinder2.census.gov/bkmk/table/1.0/en/ACS/12_5YR/B01001/0100000US 
# 307727594 was in sample reports # MUST BE UPDATED WHEN NEW ACS DATA IS USED ! 307727594 is from ???  
us.percents <- 100 * c(0.35015068301904, 0.337245110039133, 0.363056255998945, 
                       0.147948736834136, 0.0514480674233393, 0.0651419032409694, 0.131563727067491)
us.counts <- us.percents * popus
names(us.percents) <- names.d
names(us.counts) <- names.d

bar.cex <- 1.10 # defines scaling for text on barplot
