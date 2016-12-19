# This script will produce scatterplots in html comparing 2040 percent shares from runs that are specified in inputs.txt

library(plotly) # version 4.5.6
library(htmlwidgets)
library(data.table)
library(dplyr)

#environment inputs
attribute <- c("population", "households","employment", "residential_units")
geography <- c("tractcity")
year1 <- (2040)
year2 <- (2040)
extension <- ".csv"

trim <- function (x) gsub("^\\s+|\\s+$", "", x) # function for triming whitespace 

make <- !interactive()
if(make) {
  base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
  run1 <- Sys.getenv('QC_RUN1')
  run2.all <- Sys.getenv('QC_RUN2')
  run2.all <- trim(unlist(strsplit(run2.all, ","))) # run2 can have multiple directories; split by comma
  result.dir <- Sys.getenv('QC_RESULT_PATH')
  lookup <- read.table(file.path("data", "tractcity.csv"), header =TRUE, sep = ",")
  city.lookup <- read.table(file.path("data", 'cities.csv'), header=TRUE, sep=',')
  source('templates/create_Rmd_blocks.R')
  luv1.comments <- read.table(file.path("data", "luv1_comments_tc14_part.csv"), header=TRUE, sep=",")
} else {
  base.dir <- "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  #base.dir <- "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  run1 <- "run_82.run_2016_07_11_16_07"
  #run1 <- "run_81.run_2016_07_05_16_00"
  run2.all <- c("run_81.run_2016_07_05_16_00")#, "luv_1.compiled")
  run.name <- 'run82'
  wrkdir <- "C:/Users/clam/Desktop/luv/QC"
  #wrkdir <- "/Users/hana/ForecastProducts/LUV/QC"
  result.dir <- file.path(wrkdir, "results", run.name)
  lookup <- read.table(file.path(wrkdir, "data/tractcity.csv"), header =TRUE, sep = ",")
  city.lookup <- read.table(file.path(wrkdir, "data/cities.csv"), header =TRUE, sep = ",")
  source(file.path(wrkdir, 'templates/create_Rmd_blocks.R'))
  luv1.comments <- read.table(file.path(wrkdir, "data/luv1_comments_tc14_part.csv"), header=TRUE, sep=",")
}

runname1 <- unlist(strsplit(run1,"[.]"))[[1]]
runnames2 <- sapply(strsplit(run2.all,"[.]"), function(x) x[1]) # can have multiple values
if(!dir.exists(result.dir)) dir.create(result.dir)

# put a header into the index file
index.file <- file.path(result.dir, 'rplots_scatter_tractcity.Rmd')
if(file.exists(index.file)) unlink(index.file)
create.section(index.file, title="Scatterplots of city shares by tractcity")

luv1.elements <- sapply(strsplit(trim(as.character(luv1.comments$element)), "[.]"), function(x) x[1])

for (irun in 1:length(run2.all)) {
	run2 <- run2.all[irun]
	runname2 <- runnames2[irun]
	#add.text(index.file, paste("####", runname1, "vs.", runname2, "\n"))

# build plotly table for subplot
for (a in 1:length(geography)){
  
  indicators.table <- NULL
  
  for (i in 1:length(attribute)){
    #run1
    filename1 <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
    datatable1 <- read.csv(file.path(base.dir, run1,"indicators",filename1), header = TRUE, sep = ",")
    column_id <- colnames(datatable1)[grepl("_id",names(datatable1))]
    column_est <-colnames(datatable1)[grepl(year1,names(datatable1))]
    table1 <- datatable1[,c(column_id,column_est)]
    colnames(table1)[2] <- paste0("estrun1_raw")
    table1 <- merge(table1, lookup[,c('city_id', 'tractcity_id','census_2010_tract_id')], by='tractcity_id')
    table1.city <- data.table(table1)[,list(estrun1.city=sum(estrun1_raw)), by='city_id']
    table1 <- merge(table1, table1.city, by='city_id')
    table1$estrun1 <- round(100*table1$estrun1_raw/table1$estrun1.city,1)
    table1 <- table1[,-which(colnames(table1) %in% c('estrun1.city'))]
    
    #run2
    filename2 <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
    datatable2 <- read.csv(file.path(base.dir, run2,"indicators",filename2), header = TRUE, sep = ",")
    column_id2 <- colnames(datatable2)[grepl("_id",names(datatable2))]
    column_est2 <-colnames(datatable2)[grepl(year2,names(datatable2))]
    table2 <- datatable2[,c(column_id2,column_est2)]
    colnames(table2)[2] <- paste0("estrun2_raw")
    table2 <- merge(table2, lookup[,c('city_id', 'tractcity_id')], by='tractcity_id')
    table2.city <- data.table(table2)[,list(estrun2.city=sum(estrun2_raw)), by='city_id']
    table2 <- merge(table2, table2.city, by='city_id')
    table2$estrun2 <- round(100*table2$estrun2_raw/table2$estrun2.city,1)
    table2 <- table2[,-which(colnames(table2) %in% c('city_id', 'estrun2.city'))]
    
    #merge tables
    merge.table <- merge(table1, table2, by = 'tractcity_id')
    merge.table <- merge(merge.table, city.lookup, "city_id")

    merge.table$indicator <- switch(attribute[i],"population"="Population", "households"="Households", "employment"="Employment", "residential_units"="Residential Units")
    luv1c <- subset(luv1.comments, luv1.elements == switch(attribute[i], "households"="household", "employment"="job", "XXX"))
    #if(nrow(luv1c) > 0)
    	merge.table <- merge(merge.table, luv1c, by="tractcity_id", all.x=TRUE)
    
    indicators.table <- if(is.null(indicators.table)) merge.table else rbind(indicators.table,merge.table)
  
  } # end of attribute loop
  
  # id for anchoring traces on different plots
  indicators.table$id <- as.integer(factor(indicators.table$indicator))
  indicators.table$name <- paste(substr(indicators.table$census_2010_tract_id, 6, 11), indicators.table$city_name)
  #indicators.table$has_comment <- ifelse(!is.na(indicators.table$target_value), 10,3)
  indicators.table$sym <- ifelse(!is.na(indicators.table$target_value), 2, 1)
  #shapes <- c("circle-open","cross")
  #indicators.table$shapes <- shapes[indicators.table$sym]

  # plot function
  one_plot <- function(dat){
    plot_ly(dat,
            x = ~estrun1,
            y = ~estrun2,
            hoverinfo = "text",
            text = ~paste0( "Percentage: (", estrun1, ", ", estrun2,")<br>Totals: (", estrun1_raw, ", ", estrun2_raw,"), Target: ", target_value,
                            "<br>ID: ", tractcity_id, " Name: ", name),
            type = 'scatter',
            mode = 'markers',
            symbol = ~sym,
            symbols = c("circle-open","cross"),
            split = ~indicator
            )%>%
      add_trace(
        x = c(0, ~max(estrun1)),
        y = c(0, ~max(estrun1)),
        color = I("grey"),
        marker = list(size = 0),
        type = 'scatter',
        mode = 'lines',
        showlegend = F
      )
  }
  
  # plot
  p <- indicators.table %>%
    group_by(indicator) %>%
    do(p = one_plot(.)) %>%
    subplot(nrows = 2) %>%
    layout(xaxis = list(domain = c(0, .45), showgrid=TRUE),
           yaxis = list(title = runname2, domain = c(.55, 1)),
           
           xaxis2 = list(domain = c(.55, 1), showgrid=TRUE),
           yaxis2 = list(domain = c(.55, 1)),
           
           xaxis3 = list(title = runname1, domain = c(0, .45), showgrid=TRUE),
           yaxis3 = list(title = runname2, domain = c(0, .45)),
           
           xaxis4 = list(title = runname1, domain = c(.55, 1), showgrid=TRUE),
           yaxis4 = list(domain = c(0, .45)),
           
           font = list(family="Segoe UI", size = 13.5),
           title = paste0(runname1," and ", runname2, " 2040 by ", as.name(geography[a]), ": percent shares within cities"),
           margin = list(l=100, b=50, t=90, r=100)
           )
  
  print(p)
  
  subtitle <- paste(runname1, "vs.", runname2)
  print (paste0("Plotting ", subtitle))
  html.file <- paste0("rplots_", runname2, "_", as.name(geography[a]), "_scatterplot.html")
  htmlwidgets::saveWidget(p, file.path(result.dir, html.file))

  # add text into the index file
  add.text(index.file, paste0("* [", subtitle, "](", html.file, ")"))
  
} # end of geography loop
#add.text(index.file, "\n\n")
} # end of run2 loop

print ("Plotting complete! Check results directory.")

