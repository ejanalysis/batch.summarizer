# for debugging:
# options(shiny.trace=FALSE)
# options(error=browser)
# options(shiny.error=browser)

library(shiny) # http://shiny.rstudio.com

# to DISPLAY A SIMPLE COUNTY CHOROPLETH MAP
# http://shiny.rstudio.com/tutorial/lesson5/
library(maps)
library(mapproj)
# need to load gomap.js ??
source("maphelpers.R")  # if we want percent choropleths of county data
counties <- readRDS(file='data/counties.rds') # if we want county data
counties$nonwhite <- round( 100 - counties$white, 1)

# library(dplyr) # might not need this

require(Hmisc) 
require(ggplot2) # for geom_histogram() that allows weights to be used. plotrix package also does wtd hist
# ggplot2:  library(ggplot2); ggplot( fulltable, aes(pm, weight=pop) ) + geom_histogram()

# Require a package or just source the code needed:
# THESE FUNCTIONS MUST BE IN THE SHINY APP'S BASE DIRECTORY
#source('pop.ecdf.R')  # plot pop-wtd ecdf(s) for one demographic group vs others, etc.,  for comparing conditions between groups across many Census areal units (e.g. tracts)
source('pct.above.R')         # returns percent of rows (or wtd people) that have value above specified cutoff (or mean as default), for each column of data.frame
#source('pct.below.R')         # returns percent of rows (or wtd people) that have value below specified cutoff (or mean as default), for each column of data.frame
source('count.above.R')        # returns count of how many rows (or wtd people) have value above specified cutoff (or mean as default), for each column of data.frame
source('cols.above.count.R')  # returns count of how many cols (e.g. how many variables or indicators) have value above specified cutoff (or mean as default), for each row of data.frame
source('flagged.R')      # creates flag that is TRUE if at least one of 12 indicators is > cutoff (EJ index >80th %ile, for example), for each of many rows of a data.frame
source('rowMaxs.R')     # returns Max of each row
source('rowMins.R')     # returns Min of each row
source('colMaxs.R')     # returns Max of each col
source('colMins.R')     # returns Min of each col
source('lead.zeroes.R')
source('wtd.colMeans.R')     # returns wtd.mean of each col
source('change.fieldnames.R')  # updated function that helps change or resort fieldnames using a map file mapping old to new names
source('wilcoxon.pvalues.r')  # for statistical significance testing

source('batch.read.R')
source('batch.clean.R')
source('batch.summarize.R')

##################################################################################################
pixels.per.char <- 10 #????
max.allowed=30 # max length of site name we want displayed before wrapping in sites table

##################################################################################################

shinyServer(function(input, output) {
  
  # User-defined name for this dataset / analysis
  
  mybatchname  <-  renderText(input$batchname)
  output$name1 <-  renderText(input$batchname)
  output$name2 <-  renderText(input$batchname)
  output$name3 <-  renderText(input$batchname)
  output$name4 <-  renderText(input$batchname)
  output$name5 <-  renderText(input$batchname)
  output$titletext <- renderText(paste("Batch Results Summarizer:", as.character(input$batchname)))
  

  # Code enabling Download of tables and charts
  

  output$download.rowsout <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'summary stats on each indicator -plus site data.csv')
    },
    contentType='text/csv',
    content = function(file) {
      write.csv( make.colnames.friendly.complete( rbind( outlist()$rows, fulltabler() )  ), file)
    }
  )
  
  output$download.colsout <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'summary stats on each site -plus site data.csv')
    },
    contentType='text/csv',
    content = function(file) {
      write.csv( cbind( outlist()$cols,  make.colnames.friendly.complete( fulltabler() ) ), file)
    }
  )
  
  output$download.table1 <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'Table 1 Demographic Summary.csv')
    },
    contentType='text/csv',
    content = function(file) {
      write.csv( table1(), file)
    }
  )

  output$download.table2 <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'Table 2 Demographic Summary.csv')
    },
    contentType='text/csv',
    content = function(file) {
      write.csv( table2(), file)
    }
  )
  
  output$download.table3 <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), 'Table 3 Demographic Summary.csv')
    },
    contentType='text/csv',
    content = function(file) {
      write.csv( table3(), file)
    }
  )
    
  output$download.barplot <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), barplotkind(), 'barplot comparing sites to US.png')
    },
    contentType='image/png',
    content = function(file) {
      #ggsave(file, plot=barplots(), device=png, width = 1200, height = 768, units = "px", pointsize = 12)
      png(filename=file, width = 1400, height = 768, units = "px", pointsize = 12)
      # THIS WORKAROUND DOES WORK FOR NOW:
      barplots.NONreactive()
      dev.off()
    }
  )
  
  output$download.histogram <- downloadHandler(
    filename = function() { 
      paste(mybatchname(), histogramkind(), 'histogram across sites or people.png')
    },
    contentType='image/png',
    content = function(file) {
      #ggsave(file, plot=histograms.react(), device=png, width = 12.00, height = 7.68, units = "cm") # width = 1200, height = 768, units = "mm",
      png(filename=file, width = 1200, height = 768, units = "px", pointsize = 12)
      print(histograms.react())
      dev.off()
    }
  )
  
  # Notes on how shiny handles rendering plots, downloading them, and reactive expressions: 
  #    TO DRAW AND/OR DOWNLOAD PLOTS, NOTE:
  #    must say
  #        myplot <- reactive(  ....   p <- barplot(); return(p)   ); then in download handler, do 
  #          content = function(file) {
  #          ggsave(file, plot=PLOT1(), device=png, width=800, height=800, limitsize=FALSE)
  #          }
  #    In general, plot rendering is not something you want to do in a reactive expression; 
  #    the reason is because reactive expressions cache their calculated values and 
  #    only actually re-execute when one of the reactive inputs they depend on change.
  #    So if you are calling the same reactive that does a plot from two places in your code,
  #    then only one of those will have the behavior you want and the other will not get a plot rendered.
  #       The changes that Stéphane recommended ensure that the "side effect free" calculations happen inside the reactive, while the operations that have side effects (printing a ggplot object) only occurs in the output and content functions where they always belong.
  #    see https://groups.google.com/forum/#!searchin/shiny-discuss/Error$20opening$20file/shiny-discuss/79fiwAp80S4/SVibuhZTO4oJ
  #    and see https://groups.google.com/forum/#!msg/shiny-discuss/u7gwXc8_vyY/IZK_o7b7I8gJ 
  #    and see http://stackoverflow.com/questions/26764481/downloading-png-from-shiny-r
  
  
  #############################
  # OLD VS NEW VS FRIENDLIER FIELDNAMES
  
  lookup.fieldnames <- reactive({
    # THIS LETS USER UPLOAD A CUSTOM LOOKUP TABLE/MAPPING OF OLD TO NEW FIELDNAMES TO RENAME THEM AND CATEGORIZE THEM, 
    # BUT I BELIEVE THE UPLOADED BATCH DATA WILL NOT BE RENAMED UNLESS/UNTIL IT IS UPLOADED AGAIN.
    # input$file2 will be NULL initially, or can use the example file and preload it
    # After the user selects and uploads a file, it will be a data frame with 
    # 'name', 'size', 'type', and 'datapath' columns. 
    # The 'datapath' column will contain the local filenames where the data can be found.
    # THIS IS USED ALSO BY vartype() and varcategory() to classify the indicators in useful ways for filtering tables.
    inFile2 <- input$file2
    if (is.null(inFile2)) {
      myfile <- mynamesfile.default
    } else {
      myfile <- inFile2$datapath
    }
    output$infilename2 <- renderText(inFile2$name)
    fieldnamesmap <- read.csv(myfile, stringsAsFactors = FALSE)
    fieldnamesmap
  })
  
  vartype <- reactive({
    if ('vartype' %in% colnames(lookup.fieldnames())) {
      # for each of the mycolnames(), find the corresponding vartype by looking in the lookup.fieldnames() table
      lookup.fieldnames()$vartype[match(mycolnames(), lookup.fieldnames()$newnames)]
    } else {
      NA
    }
  })
  
  varcategory <- reactive({
    if ('varcategory' %in% colnames(lookup.fieldnames())) {
      lookup.fieldnames()$varcategory[match(mycolnames(), lookup.fieldnames()$newnames)]
    } else {
      NA
    }
  })
  
 
  
  ##################################################################################################
  # UPLOADED DATASET AS A TABLE

    # This (the reactive expression called fulltabler) is the uploaded dataset that 
  # is the output of the batch processing and input to the batch summarizer:
  
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
    
    output$infilename <- renderText(inFile$name)
    
    # Read the uploaded batch results. read.csv used to read text file (csv format) that was exported from ArcGIS batch tool
    # To allow user to specify parameters of upload file format:
    #fulltable <- read.csv(myfile, header=input$header, sep=input$sep, quote=input$quote, stringsAsFactors=FALSE)
    fulltable <- read.csv(myfile, stringsAsFactors = FALSE)
    # Clean the uploaded batch results. Check & rename columns to friendly names specified in namesfile map, reorder columns, etc.
    # fulltable <- batch.clean(fulltable, namesfile=mynamesfile)
    fulltable <- batch.clean(fulltable, oldcolnames=lookup.fieldnames()$oldnames, newcolnames=lookup.fieldnames()$newnames )
    if ('name' %in% colnames(fulltable) ) { rownames(fulltable) <- fulltable[ , 'name'] } # become colnames when transposed for viewing? no.
    fulltable
  })
  
  #output$fulltableout <- renderDataTable({fulltabler()})
  
  #####################
  # *** SPECIFY MORE PARAMETERS HERE THAT RELY ON fulltable 
  # - Hard coded for the moment but can be generalized at some point
  #
  # Specify names of indicators (columns) that will be compared to the threshold when doing a threshold check, 
  # summarizing for each site how many of those indicators are at/above some specified threshold.
  # Let user specify these at some point via selectize multiple selections pull down on tab1, using renderUI or something to show list of current set of fields as options
  # make this the default but offer them ui that shows colnames(fulltabler()) and lets them hit multiple checkboxes or something to specify 3+ groups and thresholds for those.
  mythreshnames <- reactive({
    list( 
      grep('^pctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) , 
      grep('region.pctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) , 
      grep('state.pctile.EJ.DISPARITY.', colnames(fulltabler()), value=TRUE) 
    )
  })
  
  #   threshnamePicker <- renderUI({
  #     selectInput("mythreshnames", "Choose fields to compare to threshold(s):", as.list(camps)) 
  #   })
  
  
  
  
  mycolnames <- reactive({ colnames(fulltabler()) })
  # *** could replace mycolnames with friendly names at some point, or at least do that just before rendering or downloading,
  # by using lookup.fieldnames()$longnames to replace corresponding $newnames

  ######## probably redundant:

    mycolnames.friendly <- reactive({
    # lookup the unfriendly colnames in the lookup table to get longname 
    # For default summary cols view, this is good- not unique names but other cols are there so OK.
    # For transposed view, need headers to be full unique names, so need vartype and longname...paste them together to create a unique friendlier name
    lookup.fieldnames()$longname[ match(mycolnames(), lookup.fieldnames()$newname) ] 
  })
  
  mycolnames.friendly.complete <- reactive({
    # lookup the unfriendly colnames in the lookup table to get longname 
    # For default summary cols view, this is good- not unique names but other cols are there so OK.
    # For transposed view, need headers to be full unique names, so need vartype and longname...paste them together to create a unique friendlier name
    paste( 
      lookup.fieldnames()$longname[ match(mycolnames(), lookup.fieldnames()$newname) ] , 
      lookup.fieldnames()$vartype[ match(mycolnames(), lookup.fieldnames()$newname) ] 
      )
  })
  
  make.colnames.friendly <- function(df) {
    # function of fulltabler()
    colnames(df) <- change.fieldnames(colnames(df), oldnames= lookup.fieldnames()$newname, newnames = lookup.fieldnames()$longname)
    df
  }
  
  make.colnames.friendly.complete <- function(df) {
    # function of fulltabler()
    colnames(df) <- change.fieldnames(colnames(df), oldnames= lookup.fieldnames()$newname, newnames = paste(lookup.fieldnames()$longname, lookup.fieldnames()$vartype) )
    df
  }
  ########
  
  namecolpixels <- reactive({
    namecolchars <- min(max.allowed, max(nchar(fulltabler()$name)) )
    namecolpixels <- pixels.per.char * namecolchars
    namecolpixels
  })
  
  
  colfun.picked <- colfun.picked.default # 'all' # later can be a logical vector but length must equal # of such funs defined as options in batch.summarize()
  rowfun.picked <- rowfun.picked.default # 'all' # later can be a logical vector but length must equal # of such funs defined as options in batch.summarize()
  
  ##################################################################################################
  # CREATE TABLES OF SUMMARY STATS
  
  # Create the reactive expression providing summary rows and cols, the key output of the batch summarizer:
  # Create summary stats from uploaded batch results. 
  # outlist() is a list of 2 elements: 
  #   rows (rows of summary stats), & cols (columns of summary stats)
  
  outlist <- reactive({ 
    x= batch.summarize(
      fulltabler(), 
      wts=fulltabler()[ , mywtsname], cols=mycolnames(), 
      threshnames=mythreshnames(), threshold=list(input$threshold1, input$threshold2, input$threshold3), threshgroup=threshgroup.default,
      colfun.picked=colfun.picked, rowfun.picked=rowfun.picked,
      probs=as.numeric( input$probs ), na.rm=na.rm
    )
    
    # For summary cols, put a duplicate column of user's site names field first if it exists, so can freeze it when seeing summary stat columns view
    if ('name' %in% colnames(fulltabler())) {x$cols <- cbind(Sitename=fulltabler()$name, x$cols) }
    
    # FOR DOWNLOAD, ONLY FORMAT SOME KEY STATS BETTER - but want to round only for web display not download
    vars.NA <- c('OBJECTID',	'FACID',	'name',	'lat',	'lon',	'radius.miles',	'ST',	'statename',	'REGION')
    x$rows[ , vars.NA] <- NA
    vars.round0 <- 'pop'
    x$rows[ , vars.round0] <- round( x$rows[ , vars.round0], 0) 
    # stats.round2 <- c('Average site', 'Average person')
    #x$rows[ stats.round2, ] <- round( x$rows[ stats.round2, ] , 2)
    #vars.comma  <- 'pop'
    #x$rows[ , vars.comma]  <- format( x$rows[ , vars.comma], big.mark=',') 
    x
  })
  
  ###########################
  # Render comprehensive output/result rows & cols of the batch summarizer as an interactive datatable for the webpage:
  # this recreates the output cols AND rows each time any inputs/settings change, which might be slow for a huge dataset,
  # but it is unlikely you would ever want to recalculate ONLY the colsout, so not a big deal
  
  ###########################
  # one summary stat per site
  ###########################

    # RENDER THE SUMMARY *COLS* AS AN INTERACTIVE DATA TABLE FOR WEB
  
  output$colsout <- renderDataTable( 
    
    cbind(outlist()$cols, make.colnames.friendly.complete( fulltabler()  ) ), 
    options=list(
      lengthMenu = list(c(10, 100, -1), c('10', '100', 'All')),
      pageLength = 100,  # -1 loads all the rows into page 1, which might be too slow if huge # of sites is uploaded
      scrollX= TRUE,
      scrollY= "340px", # 440px is enough for 12 rows on my browser but headers wrap to use up lots of space
      dom = 'rtip',
      columnDefs = list(list(width="420px", targets=list(0, length(outlist()$cols[1,]) + 
                                                           which(mycolnames()=='name')
                                                         -1))),  # makes the 1st column wide & the one called name
      scrollCollapse= TRUE   #, 
      # callback = "function(oTable) {}"    # work in progress - see same issue on FixedColumns in rowsout, below
      #     columnDefs = list(list(width="440px", targets=list(3))) #,  # *** ??? this doesn't seem to get applied until after filter is used!?  # Right now this worked for one view but not as well for transposed view... quick workaround is to add a col to transposed one
      #     #callback = I("new $.fn.dataTable.FixedColumns( table, {leftColumns: 5} );")
      #     #callback = "function(table) {}"  # work in progress
    )
  )
  
  ###########################
  # one summary stat per indicator
  ###########################
  
  # RENDER THE SUMMARY *ROWS* AS AN INTERACTIVE DATA TABLE FOR WEB 

  output$rowsout <- renderDataTable({
    
    # prepare to display table of summary stats which is outlist()$rows, along with the full table of facility-specific batch results
    
    x <- outlist()$rows
    
    # FORMAT FOR DISPLAY
    stats.round2 <- c('Average site', 'Average person')
    x[ stats.round2, ] <- round( x[ stats.round2, ] , 2)

    vars.round0 <- unique( c( names.d, grep('VSI.eo', mycolnames(), value=TRUE), grep('pct', mycolnames(), value=TRUE) ) ) # intended to find pctile and pct and VSI.eo to get the ones that are integer 0-100 
    x[ , vars.round0]  <- round( x[ , vars.round0 ] , 0)
    
    vars.comma  <- 'pop'
    x[ , vars.comma]  <- format( x[ , vars.comma], big.mark=',')

    # widerows=c(4, which(=='name'))  # to be completed...
    
   # if (input$transpose.rowsout) {

      # one row per indicator, one col per stat or site
      cbind(
        n=lead.zeroes(1:length(mycolnames()), nchar(max(length(mycolnames())))),
        Category= varcategory(),
        Type= vartype(),
        Indicator=mycolnames.friendly(),
        t(  rbind(   x, fulltabler() ))
      )  
    #} else {
      # one col per indicator, one row per  site
    #}
  }, options=list(
    scrollX= TRUE,
    scrollY= "440px", # 440px is enough for 12 rows on my browser
    lengthMenu = list(c(10, 200, -1), c('10', '200', 'All')),
    pageLength = 200,  # -1 would mean all of the rows of summary stats are in the window
    scrollCollapse= TRUE,
    dom = 'rtip',
    columnDefs = list(list(width="440px", targets=list(3))) #,  # *** ??? this doesn't seem to get applied until after filter is used!? 
    #callback = I("new $.fn.dataTable.FixedColumns( table, {leftColumns: 5} );")
    #callback = "function(table) {}"  # work in progress
  )
  )
  
  # notes on trying to get FixedColumns plugin for DataTable to work in shiny:
  #
  #https://github.com/DataTables/FixedColumns/blob/master/README.md
  #
  # callback = "function(oTable) {}",   
  # is the example in shiny documentation
  # from stack.exchange:
  #   I("new $.fn.dataTable.FixedColumns( table, {leftColumns: 5} );")
  # 
  #
  # https://datatables.net/release-datatables/extensions/FixedColumns/examples/two_columns.html
  # The example of javascript is:
  #     new $.fn.dataTable.FixedColumns( table, {
  #       leftColumns: 2
  #     } );
  #
  # https://datatables.net/extensions/fixedcolumns/
  # The example of javascript is:
  #     /*
  #       * Example initialisation
  #     */
  #       $(document).ready( function () {
  #         var table = $('#example').DataTable( {
  #           "scrollY": "300px",
  #           "scrollX": "100%",
  #           "scrollCollapse": true,
  #           "paging": false
  #         } );
  #         new $.fn.dataTable.FixedColumns( table );
  #       } );
  #     
  
  ###########################################
  # Create some summary tables of summary statistics & significance testing, comparing sites to US etc.
  
  table1 <- reactive({
    # table summarizing demog stats nearby and in US overall
    popnear = outlist()$rows[ 'Sum', mywtsname]
    mytable <- cbind(
      Location=c('Total near these sites', 'US total', 'Overall near these sites (avg. person)',  'US overall', 'Avg person, ratio to US'), 
      Pop= c(format( c(popnear,  popus, popnear, popus), big.mark=','), round(popnear/popus,4))
      )
    othercols <- rbind( format(outlist()$rows['Sum', names.d ], big.mark = ','), 
                        format(us.counts[names.d], big.mark=','),
                        paste( round(row3<-outlist()$rows['Average person' , names.d], 0), '%',sep=''),
                        paste( round(row4<-us.percents[names.d], 0), '%',sep=''),
                        format( round(row3/row4,2)))
    colnames(othercols) <- names.d.friendly
    mytable <- cbind(mytable, othercols)
  })
  
  table2 <- reactive({
    # table of significance tests for avg site's D being above US avg
    mytable <- cbind(Statistic=c('At the average site', 'standard deviation', 't-statistic',  'p-value from Wilcoxon test', 'Avg site, ratio to US'))
    othercols <- rbind( paste( round(row1<- outlist()$rows['Average site', names.d ], 0), '%',sep = ''), 
                        round(sapply(fulltabler()[ , names.d], FUN=function(x) sd(x,na.rm=TRUE)), 2),
                        0, # t stat ***
                        0, #  wilcoxon.test(x= , n=length(fulltabler()[,1]) ) , # p value ***
                        round( row1 / us.percents[names.d],2))
    colnames(othercols) <- names.d.friendly
    mytable <- cbind(mytable, othercols)
  })

  table3 <- reactive({
    # table of significance tests for # of sites with D above US avg
    pct.above.usavg <- pct.above(fulltabler()[ , names.d ], benchmarks=us.percents[names.d], benchnames='cutoff', na.rm=TRUE, or.tied=FALSE, below=FALSE, wts=1, of.what='all')
    count.above.usavg <- count.above(fulltabler()[ , names.d ], benchmarks=us.percents[names.d], benchnames='cutoff', or.tied=FALSE, below=FALSE, wts=1)
    # sum( fulltabler()[ , names.d ] > us.percents[names.d]) / length(fulltablr()[,1]
    mytable <- cbind(Statistic=c(paste('% (#) of sites where demog. > US avg. (of ',length(outlist()$cols[,1] ),'sites)'), 'standard deviation', 't-statistic',  'p-value'))
    othercols <- rbind( paste( round( 100*  pct.above.usavg   , 0), '% (', count.above.usavg,')',sep = ''), 
                        #round(sapply( fulltabler()[ , names.d], FUN=function(x) sd(x,na.rm=TRUE)), 2),
                        0, # need standard deviation that is relevant to this statistic...
                        0, # t stat ***
                        0 ) # p value ***
    colnames(othercols) <- names.d.friendly
    mytable <- cbind(mytable, othercols)
  })
  
  output$table1 <- renderTable({table1()} )
  output$table2 <- renderTable({table2()} )
  output$table3 <- renderTable({table3()} )
  

  ##################################################################################################
  # BARPLOTS
  
  # for use in name of file when saving plot
  barplotkind <- reactive({
    paste(input$bartype, input$barvartype, input$barvarmean, sep='_' )
  })
  #as.character(input$barplot.title)
  
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
                                 'EJ' = names.ej.friendly
    )
    mybarvars.refzone <- switch(input$bartype,
                                'Demographic' = paste('us.avg.',names.d,sep=''),
                                'Environmental' = paste('us.avg.',names.e,sep=''),
                                'EJ' = names.ej
    )
    
    if (input$barvartype=='pctile' | input$bartype=='EJ') {
      mybarvars <- paste('pctile.', mybarvars, sep='')
      # mybarvars.friendly <- paste('US%ile ', mybarvars.friendly, sep='') # removed since takes up space and already in header
      mybarvars.refzone <- paste('pctile.', mybarvars, sep='')
    }
    
    if (input$barvarmean=='med') {
      mybarvars.sumstat <- c( 'Median site','Median person')
      mybarvars.refzone.row <- 'Median person'  # 'Median person' is just a convenient way to refer to a row that has the summary stat that is just the reference zone's value (average for the zone, same in various rows)
      mylegend <- c(  'Median site here', 'Median person here', 'Avg. person in US')
    } else {
      # input$barvarmean=='avg'
      mybarvars.sumstat <- c( 'Average site','Average person')
      mybarvars.refzone.row <- 'Average person'  # 'Average person' is just a convenient way to refer to a row that has the summary stat that is just the reference zone's value (average for the zone, same in various rows)
      mylegend <- c(  'Average site here', 'Average person here', 'Avg. person in US')
    }
    
    if (input$barvartype=='pctile' | input$bartype=='EJ') {
      # use 50th percentile person as US overall benchmark in this case
      plotdata <- rbind( outlist()$rows[ mybarvars.sumstat, mybarvars ], 
                         rep(50, length(mybarvars.refzone)) ) 
    } else {
      # use actual US avg person's indicator score as US overall benchmark
      plotdata <- rbind( outlist()$rows[ mybarvars.sumstat, mybarvars ], 
                         outlist()$rows[ mybarvars.refzone.row, mybarvars.refzone] ) 
    }
    
    if ( input$barvartype=='raw' & input$bartype=='Environmental') {myylims <- NULL} else {myylims <-  c(0, 100) }
    if ( input$bartype %in% c('Environmental', 'EJ')) {mycex=bar.cex * 0.7} else {mycex=bar.cex} # to see the long labels
    # as.character(input$barplot.title)  # was a way to just let user specify title
    
    barplot( plotdata, beside=TRUE, ylim=myylims, cex.axis = bar.cex, cex.names=mycex, 
             main= paste(mybatchname(), '-', input$bartype, input$barvartype, 'values for', paste(mylegend, collapse = ', ') , sep=' ' ) ,
             col=c('yellow', 'green', 'blue'),
             names.arg=mybarvars.friendly, 
             ylab=ifelse( (input$barvartype=='pctile' | input$bartype=='EJ'), 'US Percentile','Raw Indicator Value') )
    legend(x='topright', legend=mylegend, fill=c('yellow', 'green', 'blue'), 
           cex=bar.cex)
    
  })
  

  # EXACT COPY OF above renderPlot function , but not reactive, to address apparent bug in downloading png:  
  # see https://groups.google.com/forum/#!searchin/shiny-discuss/Error$20opening$20file/shiny-discuss/79fiwAp80S4/SVibuhZTO4oJ
  
  barplots.NONreactive <- function(){
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
                                 'EJ' = names.ej.friendly
    )
    mybarvars.refzone <- switch(input$bartype,
                                'Demographic' = paste('us.avg.',names.d,sep=''),
                                'Environmental' = paste('us.avg.',names.e,sep=''),
                                'EJ' = names.ej
    )
    
    if (input$barvartype=='pctile' | input$bartype=='EJ') {
      mybarvars <- paste('pctile.', mybarvars, sep='')
      # mybarvars.friendly <- paste('US%ile ', mybarvars.friendly, sep='') # removed since takes up space and already in header
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
    
    if ( input$barvartype=='raw' & input$bartype=='Environmental') {myylims <- NULL} else {myylims <-  c(0, 100) }
    if ( input$bartype %in% c('Environmental', 'EJ')) {mycex=bar.cex * 0.7} else {mycex=bar.cex} # to see the long labels
    # as.character(input$barplot.title)  # was a way to just let user specify title
    
    barplot( plotdata, beside=TRUE, ylim=myylims, cex.axis = bar.cex, cex.names=mycex,  
             main= paste(mybatchname(), '-', input$bartype, input$barvartype, 'values for avg site, for avg resident near any site, and in US overall',sep=' ' ) ,
             col=c('yellow', 'green', 'blue'),
             names.arg=mybarvars.friendly, 
             ylab=ifelse( (input$barvartype=='pctile' | input$bartype=='EJ'), 'US Percentile','Raw Indicator Value') )
    
    legend(x='topright', legend=c(  'Average site here', 'Average person here', 'Avg. person in US'), fill=c('yellow', 'green', 'blue'), 
           cex=bar.cex)
    
  }
  

  ##################################################################################################
  # HISTOGRAMS
  
  # for use in name of file when saving plot
  histogramkind <- reactive({
    paste(input$myvar.friendly.base, input$refstat, input$refzone, input$sites.or.people, sep='_' )
  })
  
  
  histograms.react <- reactive({
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
    # *** could later change this to be more generic and use lookup.fieldnames() sort of like this:
    #   lookup.fieldnames()[match(input$myvar.friendly.base, lookup.fieldnames()[ , paste(c('longname', 'vartype'))]), 'newname' ] 
    myvar.friendly.base <- input$myvar.friendly.base
    myvar.base <- names.all[match(myvar.friendly.base, names.all.friendly)]
    
    if (substr(myvar.base, 1, 2)=='EJ') {
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
      myvar.full <- gsub('us.pctile', 'pctile', myvar.full)  # us.avg. is used but not us.pctile... it is just pctile for us! # this presumes new variable names are as in default file
      myvar.friendly.full <- paste(myvar.friendly.base, ', as ', refzone.friendly, ' ', refstat.friendly, sep='')
    }
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
      
      myplot <- ggplot( fulltabler(), aes_string( myvar.full) ) + 
        geom_histogram(fill='white', colour='darkgreen', binwidth = diff(range( fulltabler()[ , myvar.full] ,na.rm=TRUE))/mybincount) +
        geom_hline(aes_string(yintercept=expected.sites.per.bin)) +
        xlab(myvar.friendly.full) + ylab(input$sites.or.people) + 
        ggtitle( paste(mybatchname(), ', ', myvar.friendly.full,': Distribution across ', input$sites.or.people, sep=''))
      
    } else {
      
      # *** hard coded to use mywtsname as weights for now:
      
      myplot <- ggplot( fulltabler(), aes_string( myvar.full, weight=fulltabler()[ , mywtsname] ) ) + 
        geom_histogram(fill='white', colour='darkgreen', binwidth = diff(range( fulltabler()[ , myvar.full] ,na.rm=TRUE))/mybincount) +
        #geom_hline(aes_string(yintercept=expected.pop.per.bin)) +
        xlab(myvar.friendly.full) + ylab(input$sites.or.people) + 
        ggtitle( paste(mybatchname(), ', ', myvar.friendly.full,': Distribution across ', input$sites.or.people, sep=''))
    }
    return(myplot)
  })
  
  output$histograms <- renderPlot( histograms.react() )
  
  ##################################################################################################
  # MAPS
  
  output$map <- renderPlot({
    args <- switch(input$mapvar,
                   "Percent Non-White" = list(counties$nonwhite, "darkblue", "% Non-White"),
                   "Percent White" = list(counties$white, "darkgreen", "% White"),
                   "Percent Black" = list(counties$black, "black", "% Black"),
                   "Percent Hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
                   "Percent Asian" = list(counties$asian, "darkviolet", "% Asian"))
    # or hardcoded for now ***
    # data, color, legend params re passed using args above 
    args$min <- input$range[1]
    args$max <- input$range[2]
    
    do.call(percent_map, args)
  })
  
  # notes from superzip map example... work in progress
  #   allzips <- readRDS("data/superzip.rds")
  #   allmap$latitude <- jitter(fulltabler()$lat)
  #   allmap$longitude <- jitter(fulltabler()$lon)
  #   #allmap$college <- allzips$college * 100
  #   #allmap$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
  #   row.names(allmap) <- allmap$zipcode
  #   cleantable <- allmap[ , c('ST', 'lat', 'lon', 'pop', names.d, )]

})

