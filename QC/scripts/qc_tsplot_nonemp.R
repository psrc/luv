#This script will produce a timeseries plot for households and total population by county and special places
library(data.table)
library(plotly)

# environment inputs
attribute <- c("population", "households", "residential_units")
geography <- c("city")#, "tractcity") 
years <- c(2014, 2015, 2020, 2025, 2030, 2035, 2040)
extension <- ".csv"

trim <- function (x) gsub("^\\s+|\\s+$", "", x) # function for triming whitespace 

make <- nchar(Sys.getenv('QC_NAME')) > 0 

if(make) {
  base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
  run1 <- Sys.getenv('QC_RUN1')
  run2.all <- Sys.getenv('QC_RUN2')
  run2.all <- trim(unlist(strsplit(run2.all, ","))) # run2 can have multiple directories; split by comma
  result.dir <- Sys.getenv('QC_RESULT_PATH')
  result.name <- Sys.getenv('QC_NAME')
  wrkdir <- file.path(Sys.getenv('QC_SCRIPT_PATH'), "..")
} else {
  base.dir <- "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  #base.dir <- "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  run1 <- "run_99.run_2016_08_15_17_11"
  run2.all <- c("run_81.run_2016_07_05_16_00","luv_1.compiled")
  run.name <- 'run99'
  wrkdir <- "C:/Users/Christy/Desktop/luv/QC"
  result.dir <- file.path(wrkdir, "results", run.name)
}

dsn <- file.path(wrkdir, "data")
city.lookup <- read.table(file.path(dsn, "cities.csv"), header =TRUE, sep = ",")
runname1 <- unlist(strsplit(run1,"[.]"))[[1]]
runnames2 <- sapply(strsplit(run2.all,"[.]"), function(x) x[1]) # can have multiple values
runs <- c(runname1, unlist(runnames2))
rundir <- c(run1, run2.all)

#initialize master table
alldt <- NULL 

for (a in 1:length(geography)){
  #initialize table
  geog.table <- NULL
  
  for (r in 1:length(rundir)) {
    #initialize table
    indicators.table <- NULL
    
    for (i in 1:length(attribute)){
      filename <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
      datatable <- read.csv(file.path(base.dir, rundir[r],"indicators",filename), header = TRUE, sep = ",")
      column_id <- colnames(datatable)[grepl("_id",names(datatable))]
      column_est <- NULL
      for (y in 1: length(years)){
        column_est1 <- colnames(datatable)[grepl((years[y]),names(datatable))]
        ifelse (is.null(column_est1), 
                column_est <- column_est1,
                column_est <- cbind(column_est, column_est1))
      }
      table <- datatable[,c(column_id,column_est)]
      colnames(table)[2:ncol(table)] <- paste0("yr", sapply(years, function(x) x[1]))
      colnames(table)[1] <- "name_id"
      table$indicator <- switch(attribute[i],
                                "population"="Total Population", 
                                "households"="Households", 
                                "residential_units"="Residential Units")
      
      table$run <- runs[r]
      
      if (geography[a] == "city") {
        table <- merge(table, city.lookup, by.x = "name_id", by.y = "city_id")
        table <- table[,c(1, (ncol(table)-4):(ncol(table)-2),2:(ncol(table)-5))]
      }
      
      ifelse (is.null(indicators.table),
              indicators.table <- table, 
              indicators.table <- rbind(indicators.table, table))
      
    } # end of attribute loop
    
    ifelse (is.null(alldt),
            alldt <- indicators.table,
            alldt <- rbind(alldt, indicators.table))
    
    
  } # end of runnames loop 
  alldt <- as.data.table(alldt)

  # transform master table
  ptable <- NULL
  for (y in 1:length(years)){
    subtable <- subset(alldt, select = c(1:4, grep(years[y], names(alldt))))
    setnames(subtable, names(subtable)[ncol(subtable)], "estimate")
    subtable[, year := years[y]]
    ifelse (is.null(ptable),
            ptable <- subtable,
            ptable <- rbind(ptable, subtable))
  }

  #test using only first 25 city_ids
  ptable <- subset(ptable, (name_id %in% c(seq(1,13))))
  
  #create one plot per city_id & indicator, write it to plot_list, print plots 
  N <- levels(factor(ptable$name_id))
  indname <- levels(factor(ptable$indicator))

  plot_list <- vector("list")
  plot_list_cnt <- 0
  
  for (n in 1:length(N)){ # for each name_id
    for (ii in 1: length(indname)) { # for each indicator
      p <- plot_ly(data = subset(ptable, name_id == as.integer(N[n]) & indicator == indname[ii]), 
                   x = year,
                   y = estimate,
                   color = run,
                   #group = run,
                   #hoverinfo = "text",
                   #text = " ",
                   type = 'scatter',
                   mode = 'lines+markers',
                   showlegend = FALSE
                   ) 
        
      plot_list_cnt <- plot_list_cnt + 1
      plot_list[[plot_list_cnt]] <- p
    } # end indicator loop
  } # end cityid loop

  q <- subplot(plot_list, nrows = 13) #change back to 140

  print(q)





} # end of geography loop







