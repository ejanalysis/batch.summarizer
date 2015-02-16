# options(error=browser)
# options(shiny.error=browser)

library(shiny) # http://shiny.rstudio.com

require(Hmisc) 
require(ggplot2) # for geom_histogram() that allows weights to be used. plotrix package also does wtd hist
# ggplot2:  library(ggplot2); ggplot( fulltable, aes(pm, weight=pop) ) + geom_histogram()

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
source('lead.zeroes.R')
source('wtd.colMeans.R')     # returns wtd.mean of each col
source('change.fieldnames.R')  # updated function that helps change or resort fieldnames using a map file mapping old to new names

source('batch.read.R')
source('batch.clean.R')
source('batch.summarize.R')

#############################
# DEFAULT VALUES, possibly could recode to allow user to change them, and/or read fieldnames from the csv file: 
#############################
probs.default <- c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1)  # defaults for quantiles summary stats
#mythreshold=80  # default for cutoff in at/above threshold stat summarizing the EJ Index US percentiles
na.rm=TRUE  # default for all functions so can get stats even if one site (or one indicator at one site) has no data
#
mydemofile <- 'Export_Output_Example2.txt' # example of export of batch results, for use in testing/demonstrating summarizer
mynamesfile <- 'map batch to friendly fieldnames v1.csv' # has default input and output and friendly fieldnames & var type & var category
# *** SPECIFY MORE PARAMETERS - USE DEFAULTS FOR NOW, POSSIBLY RECODE LATER TO LET USER CHANGE THESE
# default Demog vars for now:
mywtsname <- 'pop'  # used for weighted means to get stats on the average person in all these zones (e.g. avg person nearby any site)
names.d          <- c('VSI.eo', 'pctlowinc', 'pctmin', 'pctlths', 'pctlingiso', 'pctunder5', 'pctover64') 
names.d.friendly <- c('Demog.Ind.', '% Low-inc.', '% Minority', '% <High School', '% Linguistic Isol.', '% < age 5', '% > age 64')
# default Envt vars for now:
names.e          <- c("pm", "o3", "cancer", "neuro", "resp", "dpm", "pctpre1960", 
             "traffic.score", "proximity.npl", "proximity.rmp", "proximity.tsdf", 
             "proximity.npdes")
names.e.friendly <- c("PM2.5", "Ozone", "NATA Cancer risk", "NATA Neuro", "NATA Respiratory", "NATA Diesel PM", "% built pre-1960", 
                      "Traffic", "NPL proximity", "RMP proximity", "TSDF proximity", 
                      "NPDES proximity")
# default EJ vars for now:
names.ej <- c("EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.cancer.eo", 
              "EJ.DISPARITY.neuro.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", 
              "EJ.DISPARITY.pctpre1960.eo", "EJ.DISPARITY.traffic.score.eo", 
              "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", 
              "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo")
names.ej.friendly <- paste('EJ Ind.-', names.e.friendly)
names.all <- c(names.d, names.e, names.ej)
names.all.friendly <- c(names.d.friendly, names.e.friendly, names.ej.friendly)

#######################################################################################

shinyServer(function(input, output) {
  
  mybatchname <- renderText(input$batchname)
  output$name1 <- renderText(input$batchname)
  output$name2 <- renderText(input$batchname)
  output$name3 <- renderText(input$batchname)
  output$name4 <- renderText(input$batchname)
  output$name5 <- renderText(input$batchname)
  output$titletext <- renderText(paste("Batch Results Summarizer:", as.character(input$batchname)))

  output$download.batchdata <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'batchdata.csv')
    },
    contentType='text/csv',
    content = function(file) {
      write.csv( fulltabler(), file)
    }
  )
  
  output$download.rowsout <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'rowsout.csv')
    },
    contentType='text/csv',
    content = function(file) {
      write.csv( rbind( outlist()$rows, fulltabler()), file)
    }
  )
  
  output$download.colsout <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'colsout.csv')
    },
    contentType='text/csv',
    content = function(file) {
      write.csv( cbind( outlist()$cols, fulltabler()), file)
    }
  )
  
  output$download.barplot <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'barplot.png')
    },
    contentType='image/png',
    content = function(file) {
#      png(filename=file,width = 480, height = 480, units = "px", pointsize = 12); barplots.reactive()  ; dev.off()
    }
  )
  
  output$download.histogram <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'histogram.png')
    },
    contentType='image/png',
    content = function(file) {
#      png(file=file,width = 480, height = 480, units = "px", pointsize = 12); histograms.reactive()  ; dev.off()
    }
  )
  
  #############################
  
  fulltabler <- reactive({
    
    # input$file1 will be NULL initially, or can use the example file and preload it?
    # After the user selects and uploads a file, it will be a data frame with 
    # 'name', 'size', 'type', and 'datapath' columns. 
    # The 'datapath' column will contain the local filenames where the data can be found.
    inFile <- input$file1
    if (is.null(inFile)) {
      myfile <- mydemofile
    } else {
      myfile <- inFile$datapath
    }
    
    # Read the uploaded batch results. read.csv used to read text file (csv format) that was exported from ArcGIS batch tool
    # To allow user to specify parameters of upload file format:
    #fulltable <- read.csv(myfile, header=input$header, sep=input$sep, quote=input$quote, stringsAsFactors=FALSE)
    fulltable <- read.csv(myfile, stringsAsFactors = FALSE)
    # Clean the uploaded batch results. Check & rename columns to friendly names specified in namesfile map, reorder columns, etc.
    fulltable <- batch.clean(fulltable, namesfile=mynamesfile)
    fulltable
  })
  
  # *** SPECIFY MORE PARAMETERS HERE THAT RELY ON fulltable
  mythreshnames <- reactive({ grep('^pctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) })
  mycolnames <- reactive({ colnames(fulltabler()) })
  # could replace mycolnames with friendly names at some point.
  colfun.picked <- 'all' # later can be a logical vector but length must equal # of such funs defined as options in batch.summarize()
  rowfun.picked <- 'all' # later can be a logical vector but length must equal # of such funs defined as options in batch.summarize()
  
  output$fulltableout <- renderDataTable({fulltabler()} )
  
  outlist <- reactive({ 
    batch.summarize(
      fulltabler(), 
      wts=fulltabler()[ , mywtsname], cols=mycolnames(), 
      threshnames=mythreshnames(), threshold=input$threshold, 
      colfun.picked=colfun.picked, rowfun.picked=rowfun.picked,
      probs=as.numeric( input$probs ), na.rm=na.rm
    )
  })
  
  output$rowsout <- renderDataTable({
    
    # Create summary stats from uploaded batch results. outlist() is a list of 2 elements: 
    #   rows (rows of summary stats), & cols (columns of summary stats)
    # DISPLAY THE SUMMARY ROWS AS A TABLE BUT TRANSPOSED SO EASIER TO SEE
    output$colsout <- renderDataTable( cbind(outlist()$cols, fulltabler() ))
    if (input$transpose.rowsout) {
      cbind(IndicatorSort=lead.zeroes(1:length(mycolnames()), nchar(max(length(mycolnames())))), 
            t(  rbind(  Indicator=mycolnames(), outlist()$rows, fulltabler())))  
    } else {
         cbind( Stat_or_Site=c(rownames(outlist()$rows), fulltabler()[ , 1] )   , 
                rbind( outlist()$rows, fulltabler()) )
    }
  })
  
  ##################################################################################################
  
  
  ##################################################################################################
  # BARPLOTS
  
  output$barplots <- renderPlot({
    # One set of bars per each of the myvars
    print('done again')
    # *** possibly allow these to be set by user instead of hard-coded names:
    mybarvars <- switch(input$bartype,
                        'Demographic' = names.d,
                        'Environmental' = names.e,
                        'EJ' = names.ej
    )
    mybarvars.friendly <- switch(input$bartype,
                                 'Demographic' = names.d.friendly,
                                 'Environmental' = names.e.friendly,
                                 'EJ' = names.ej.friendly
    )
    mybarvars.refzone <- switch(input$bartype,
                                'Demographic' = paste('us.avg.',names.d,sep=''),
                                'Environmental' = paste('us.avg.',names.e,sep=''),
                                'EJ' = names.ej
    )
    
    if (input$barvartype=='pctile' | input$bartype=='EJ') {
      mybarvars <- paste('pctile.', mybarvars, sep='')
      mybarvars.friendly <- paste('US%ile ', mybarvars.friendly, sep='')
      mybarvars.refzone <- paste('pctile.', mybarvars, sep='')
    }
    
    mybarvars.sumstat <- c( 'Average site','Average person')
    mybarvars.refzone.row <- 'Average person'  # 'Average person' is just a convenient way to refer to a row that has the summary stat that is just the reference zone's value (average for the zone, same in various rows)
    if (input$barvartype=='pctile' | input$bartype=='EJ') {
      # use 50th percentile person as US overall benchmark
      plotdata <- rbind( outlist()$rows[ mybarvars.sumstat, mybarvars ], 
                         rep(50, length(mybarvars.refzone)) ) 
    } else {
      # use actual US avg person's indicator score as US overall benchmark
      plotdata <- rbind( outlist()$rows[ mybarvars.sumstat, mybarvars ], 
                         outlist()$rows[ mybarvars.refzone.row, mybarvars.refzone] ) 
    }
    barplot( plotdata, beside=TRUE, legend.text=c(  'Average site here', 'Average person here', 'Avg. person in US'), col=c('yellow', 'green', 'blue'),
             names.arg=mybarvars.friendly, ylab=ifelse( (input$barvartype=='pctile' | input$bartype=='EJ'), 'US Percentile','Raw Indicator Value') )
  })
  
  
  ##################################################################################################
  # HISTOGRAMS
  
  output$histograms <- renderPlot({
    # e.g., draw histogram of selected variable's US percentiles, distribution over sites, vs expected distribution
    
    # *** User will be able to define these using checkboxes:
    # (this code presumes new variable names are as in default file)
    #       myvar.base <- 'VSI.eo'  # *** BUT IF IT IS A SUMMARY STAT LIKE ??? this won't work in hist(fulltable[ , myvar]) since it is in outlist()$rows not in fulltable
    #       myvar.full <- paste(refzone, refstat, myvar.base, sep='.')  # this presumes new variable names are as in default file
    #       myvar.full <- gsub('us.pctile', 'pctile', myvar.full)  # us.avg. is used but not us.pctile... it is just pctile for us! # this presumes new variable names are as in default file
    #       myvar.friendly.base <- 'Demographic Index'
    #       myvar.friendly.full <- paste(myvar.friendly.base, ', as ', refzone.friendly, ' ', refstat.friendly, ' across ', sites.or.people, sep='')
    
    refzone.friendly <- switch(input$refzone, 
                               'us'='US',
                               'region'='Region',
                               'state'='State')
    refstat.friendly <- switch(input$refstat,
                               'pctile'='Percentile',
                               'raw'='Indicator Value (not percentile)')
    
    # *** Should make this more generic/ flexible, not hard-coded names:
    # *** SHOULD USE FRIENDLY NAMES IN UI LIST AND PASS myvar.friendly.base 
    # and then HERE IN SERVER SHOULD fix to use that to get non friendly base for plot
    
    # get long name of field selected to plot, then convert to short name
    myvar.friendly.base <- input$myvar.friendly.base
    myvar.base <- names.all[match(myvar.friendly.base, names.all.friendly)]
    cat(myvar.base,' 0 \n')
    if (substr(myvar.base,1,2)=='EJ') {
      myrefstat <- 'pctile'
      refstat.friendly <- 'Percentile'
    } else {
      myrefstat <- input$refstat
    }
    if (myrefstat=='raw' ) {
      myvar.full <- myvar.base
      myvar.friendly.full <- paste(myvar.friendly.base, ', as ', refstat.friendly, ' across ', input$sites.or.people, sep='')
    } else {
      myvar.full <- paste(input$refzone, myrefstat, myvar.base, sep='.')  # this presumes new variable names are as in default file
      
      cat(myvar.full,' 1 \n') #  **** fails to reach here when EJ moved from pctile to raw selection !!!
      
      myvar.full <- gsub('us.pctile', 'pctile', myvar.full)  # us.avg. is used but not us.pctile... it is just pctile for us! # this presumes new variable names are as in default file
      cat(myvar.full,' 2 \n')
      myvar.friendly.full <- paste(myvar.friendly.base, ', as ', refzone.friendly, ' ', refstat.friendly, sep='')
    }
    cat(myvar.full,' 3 \n')
    if (myrefstat=='raw' ) {
      sitecount <- 0 # suppress horizontal line benchmark when viewing raw data- it only applies to percentiles. Correct histo benchmark for raw would be the US overall histogram of that raw value in the selected # of bins, which is hard to provide here.
      popcount <- 0
    } else {
      sitecount <- length( fulltabler()[ , myvar.full] ) # but for popwtd hist, use popcount!
      #popcount < - sum( fulltable[ , mywtsname], na.rm=TRUE ) # assumes 'pop' is colname for weights, for now. fails.
      popcount <- outlist()$rows[ 'Sum','pop' ]
    }
    
    mybincount <- input$bincount # (0:10)*10 # assumes you want to see sites in 10 bins, 0-10th percentile, 10-20, etc.
    expected.sites.per.bin= sitecount / mybincount # assumes you want to see sites in 10 bins  # but for popwtd hist, use popcount?!
    expected.pop.per.bin=   popcount  / mybincount  # but the horizontal line from this doesn't look right so don't graph it for now ****** 
    
    # HISTOGRAM plotted here
    
    if (input$sites.or.people=='Sites') {
      # see for formatting nicely:  http://docs.ggplot2.org/0.9.3.1/geom_bar.html
      
      ggplot( fulltabler(), aes_string( myvar.full) ) + 
        geom_histogram(fill='white', colour='darkgreen', binwidth = diff(range( fulltabler()[ , myvar.full] ,na.rm=TRUE))/mybincount) +
        geom_hline(aes_string(yintercept=expected.sites.per.bin)) +
        xlab(myvar.friendly.full) + ylab(input$sites.or.people) + 
        ggtitle( paste(myvar.friendly.full,': Distribution across ', input$sites.or.people, sep=''))
      
    } else {
      
      # *** hard coded to use mywtsname as weights for now:
      
      ggplot( fulltabler(), aes_string( myvar.full, weight=fulltabler()[ , mywtsname] ) ) + 
        geom_histogram(fill='white', colour='darkgreen', binwidth = diff(range( fulltabler()[ , myvar.full] ,na.rm=TRUE))/mybincount) +
        #geom_hline(aes_string(yintercept=expected.pop.per.bin)) +
        xlab(myvar.friendly.full) + ylab(input$sites.or.people) + 
        ggtitle( paste(myvar.friendly.full,': Distribution across ', input$sites.or.people, sep='')) 
    }
  })
})

