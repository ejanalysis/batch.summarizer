# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# http://shiny.rstudio.com
library(shiny)

require(Hmisc) 
require(ggplot2) #   
# require(plotrix) # for weighted.hist() # or use ggplot2:  library(ggplot2); ggplot( fulltable, aes(pm, weight=pop) ) + geom_histogram()

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
source('change.fieldnames.R')  # updated function that helps change or resort fieldnames using a map file mapping old to new names

source('batch.read.R')
source('batch.clean.R')
source('batch.summarize.R')

# DEFAULT VALUES, possibly could recode to allow user to change them: 
mynamesfile <- 'map batch to friendly fieldnames v1.csv'
probs <- c(0,0.25,0.50,0.75,0.80,0.90,0.95,0.99,1)
mythreshold=80
na.rm=TRUE
# *** SPECIFY MORE PARAMETERS - USE DEFAULTS FOR NOW, POSSIBLY RECODE LATER TO LET USER CHANGE THESE
mywtsname <- 'pop'
names.d <- c('VSI.eo', 'pctlowinc', 'pctmin', 'pctlths', 'pctlingiso', 'pctunder5', 'pctover64')
names.d.friendly <- c('Demog.Ind.', '% Low-inc.', '% Minority', '% <High School', '% Linguistic Isol.', '% < age 5', '% > age 64')
names.e <- c("pm", "o3", "cancer", "neuro", "resp", "dpm", "pctpre1960", 
             "traffic.score", "proximity.npl", "proximity.rmp", "proximity.tsdf", 
             "proximity.npdes")
names.e.friendly <- c("PM2.5", "Ozone", "NATA Cancer risk", "NATA Neuro", "NATA Respiratory", "NATA Diesel PM", "% built pre-1960", 
                      "Traffic", "NPL proximity", "RMP proximity", "TSDF proximity", 
                      "NPDES proximity")
names.ej <- c("EJ.DISPARITY.pm.eo", "EJ.DISPARITY.o3.eo", "EJ.DISPARITY.cancer.eo", 
              "EJ.DISPARITY.neuro.eo", "EJ.DISPARITY.resp.eo", "EJ.DISPARITY.dpm.eo", 
              "EJ.DISPARITY.pctpre1960.eo", "EJ.DISPARITY.traffic.score.eo", 
              "EJ.DISPARITY.proximity.npl.eo", "EJ.DISPARITY.proximity.rmp.eo", 
              "EJ.DISPARITY.proximity.tsdf.eo", "EJ.DISPARITY.proximity.npdes.eo"
)
names.ej.friendly <- paste('EJ Index for',names.e.friendly)


shinyServer(function(input, output) {
  
  output$download.batchdata <- downloadHandler(
    filename = function() { 
      'batchdata.csv'
    },
    content = function(file) {
      write.csv( datasetInput.batchdata(), file)
    }
  )
  
  output$download.rowsout <- downloadHandler(
    filename = function() { 
      'rowsout.csv'
    },
    content = function(file) {
      write.csv( datasetInput.rowsout(), file)
    }
  )

  
  output$rowsout <- renderTable({
    
    # input$file1 will be NULL initially. 
    # After the user selects and uploads a file, it will be a data frame with 
    # 'name', 'size', 'type', and 'datapath' columns. 
    # The 'datapath' column will contain the local filenames where the data can be found.
    
    inFile <- input$file1
    
    if (is.null(inFile)) {
      #output$rowsout <- NULL
      return(NULL)
    }
        
    # Read the uploaded batch results. read.csv used to read text file (csv format) that was exported from ArcGIS batch tool
    fulltable <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, stringsAsFactors=FALSE)
    
    # Clean the uploaded batch results. Check & rename columns to friendly names specified in namesfile map, reorder columns, etc.
    fulltable <- batch.clean(fulltable, namesfile=mynamesfile)
    
    output$fulltableout <- renderTable(  fulltable )
    
    
#     NOT CORRECT - NOT WORKING YET:
#     datasetInput.batchname() <- reactive <- ({
#       input$dataset <- fulltable
#     })
#     # download.batchdata
#     
#     datasetInput.rowsout() <- reactive <- ({
#       input$dataset <- outlist$rows
#     })
    
    
    # SPECIFY MORE PARAMETERS HERE THAT RELY ON fulltable     
    mythreshnames <- grep('^pctile.EJ.DISPARITY.', colnames(fulltable), value=TRUE)
    mycolnames <- colnames(fulltable)
    colfun.picked <- 'all' # later can be a logical vector but length must equal # of such funs defined as options in batch.summarize()
    rowfun.picked <- 'all' # later can be a logical vector but length must equal # of such funs defined as options in batch.summarize()
    
    ##################################################################################################
    # Create summary stats from uploaded batch results. outlist is a list of 2 elements: 
    #   rows (rows of summary stats), & cols (columns of summary stats)
    
    outlist <- batch.summarize(fulltable, 
      wts=fulltable[ , mywtsname], cols=mycolnames, 
      threshnames=mythreshnames, threshold=mythreshold, 
      colfun.picked=colfun.picked, rowfun.picked=rowfun.picked,
      probs=probs, na.rm=na.rm
      )
    
    # Transpose summary stats rows for dispaly so browser can fit it easily, by showing all the summary stats for a single input column as one row on the screen instead of as one column.
    # This does not currently display the columns of summary stats like # of EJ indicator uspctiles at/above threshold at each site.
    output$colsout <- renderTable(  outlist$cols)
    ##################################################################################################
    
      
    ##################################################################################################
    # BARPLOTS
    
    output$barplots <- renderPlot({
      # One set of bars per each of the myvars
      
      # *** possibly allow these to be set by user instead of hard-coded names:
      mybarvars <- switch(input$bartype,
             'Demographic' = names.d,
             'Environmental' = names.e,
             'EJ' = names.ej
             )
      mybarvars.friendly <- switch(input$bartype,
                          'Demographic' = names.d.friendly,
                          'Environmental' = names.e.friendly,
                          'EJ (US %ile)' = names.ej.friendly
      )
      mybarvars.refzone <- switch(input$bartype,
                          'Demographic' = paste('us.avg.',names.d,sep=''),
                          'Environmental' = paste('us.avg.',names.e,sep=''),
                          'EJ (US %ile)' = paste('pctile.',names.ej,sep='')
      )

      #mybarvars          <- c('VSI.eo', 'pctlowinc', 'pctmin', 'pctlths', 'pctlingiso', 'pctunder5', 'pctover64')
      #mybarvars.friendly <- c('Demog.Ind.', '% Low-inc.', '% Minority', '% <High School', '% Linguistic Isol.', '% < age 5', '% > age 64')
      #mybarvars.refzone  <- c('us.avg.VSI.eo', 'us.avg.pctlowinc', 'us.avg.pctmin', 'us.avg.pctlths','us.avg.pctlingiso', 'us.avg.pctunder5', 'us.avg.pctover64' )
            
      mybarvars.sumstat <- c('Average person', 'Average site')
      mybarvars.refzone.row <- 'Average person'  # 'Average person' is just a convenient way to refer to a row that has the summary stat that is just the reference zone's value (average for the zone, same in various rows)
      plotdata <- rbind( outlist$rows[ mybarvars.sumstat, mybarvars ], 
                         outlist$rows[ mybarvars.refzone.row, mybarvars.refzone] ) 
      barplot( plotdata, beside=TRUE,
               legend.text=c('Average person', 'Average site', 'US Overall'),
               names.arg=mybarvars.friendly )
    })
    
    ##################################################################################################
    # HISTOGRAMS
    
    output$histograms <- renderPlot({
      # e.g., draw histogram of selected variable's US percentiles, distribution over sites, vs expected distribution
      
      # User will be able to define these using checkboxes:
      # (this code presumes new variable names are as in default file)
      
      # insert checkboxes code here?
      
#       sites.or.people='Sites' # or 'People'
#       refzone<- 'us' # or 'region' or 'state' # this presumes new variable names are as in default file
#       refzone.friendly='US' # or 'Region' or 'State' 
#       refstat <- 'pctile'  # or 'avg' # this presumes new variable names are as in default file
#       refstat.friendly='Percentile'  # or 'Average' 
#       myvar.base <- 'VSI.eo'  # *** BUT IF IT IS A SUMMARY STAT LIKE ??? this won't work in hist(fulltable[ , myvar]) since it is in outlist$rows not in fulltable
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
      myvar.friendly.base <- switch(input$myvar.base,
                                    'VSI.eo'='Demographic Indicator', 
                                    'pctmin'='% Minority', 
                                    'pctlowinc'='% Low-Income', 
                                    'traffic.score'='Traffic Proximity', 
                                    'proximity.tsdf'='Proximity to TSDF',
                                    'cancer'='NATA Cancer risk',
                                    'EJ.DISPARITY.traffic.score.eo'='EJ Index for Traffic',
                                    'EJ.DISPARITY.proximity.tsdf.eo'='EJ Index for TSDF proximity',
                                    'EJ.DISPARITY.cancer.eo'='EJ Index for NATA cancer risk')

      if (input$refstat=='raw') {
        myvar.full <- input$myvar.base
        myvar.friendly.full <- paste(myvar.friendly.base, ', as ', refstat.friendly, ' across ', input$sites.or.people, sep='')
      } else {
        myvar.full <- paste(input$refzone, input$refstat, input$myvar.base, sep='.')  # this presumes new variable names are as in default file
        myvar.full <- gsub('us.pctile', 'pctile', myvar.full)  # us.avg. is used but not us.pctile... it is just pctile for us! # this presumes new variable names are as in default file
        myvar.friendly.full <- paste(myvar.friendly.base, ', as ', refzone.friendly, ' ', refstat.friendly, ' across ', input$sites.or.people, sep='')
      }
      
      sitecount <- length( fulltable[ , myvar.full] ) # but for popwtd hist, use popcount!
      #popcount < - sum( fulltable[ , mywtsname], na.rm=TRUE ) # assumes 'pop' is colname for weights, for now. fails.
      popcount <- outlist$rows[ 'Sum','pop' ]

      mybincount <- input$bincount # (0:10)*10 # assumes you want to see sites in 10 bins, 0-10th percentile, 10-20, etc.
      expected.sites.per.bin= sitecount / mybincount # assumes you want to see sites in 10 bins  # but for popwtd hist, use popcount!
      expected.pop.per.bin=   popcount  / mybincount  # but the horizontal line from this doesn't look right ****** 
      
      # HISTOGRAM plotted here

      if (input$sites.or.people=='Sites') {
        # see for formatting nicely:  http://docs.ggplot2.org/0.9.3.1/geom_bar.html
        
        ggplot( fulltable, aes_string( myvar.full) ) + 
          geom_histogram(fill='white', colour='darkgreen', binwidth = diff(range( fulltable[ , myvar.full] ,na.rm=TRUE))/mybincount) +
          geom_hline(aes_string(yintercept=expected.sites.per.bin)) +
          xlab(myvar.friendly.full) + ylab(input$sites.or.people) + 
          ggtitle( paste(myvar.friendly.full,': Distribution across ', input$sites.or.people, sep='')) 
        
        #hist(fulltable[ , myvar.full], breaks = mybincount, col = 'darkgray', border = 'white', 
        #     main=paste(myvar.friendly.full,':
        #              Distribution across ', input$sites.or.people, sep=''), xlab=myvar.friendly.full, ylab=input$sites.or.people)
        #abline(h=expected.sites.per.bin)
        
      } else {
        
        # hard coded to use 'pop' as weights for now:
        
        ggplot( fulltable, aes_string( myvar.full, weight=fulltable[ , mywtsname] ) ) + 
          geom_histogram(fill='white', colour='darkgreen', binwidth = diff(range( fulltable[ , myvar.full] ,na.rm=TRUE))/mybincount) +
          #geom_hline(aes_string(yintercept=expected.pop.per.bin)) +
          xlab(myvar.friendly.full) + ylab(input$sites.or.people) + 
          ggtitle( paste(myvar.friendly.full,': Distribution across ', input$sites.or.people, sep='')) 

        #         weighted.hist(fulltable[ , myvar.full], w=fulltable[ , mywtsname],  breaks = mybincount, col = 'darkgray', border = 'white', 
        #              main=paste(myvar.friendly.full,':
        #                       Distribution across ', input$sites.or.people, sep=''), xlab=myvar.friendly.full, ylab=input$sites.or.people)
        #         abline(h=expected.pop.per.bin)
      }
    })

    ##################################################################################################
    # DISPLAY THE SUMMARY ROWS AS A TABLE BUT TRANSPOSED SO EASIER TO SEE

    t( outlist$rows )

  })
})

