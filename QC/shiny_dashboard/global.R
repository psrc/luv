library(shiny)
library(plotly) # version 4.5.6
library(leaflet)
library(rgdal)
library(sp)
library(data.table)
library(magrittr)
library(shinythemes)

# environment inputs
attribute <- c("population", "households","employment", "residential_units")
geography <- c( "zone", "faz", "city")
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
  run1 <- "luv2.1draft"
  run2.all <- c("run_32.run_2016_10_17_15_00", "run_81.run_2016_07_05_16_00", "luv_1.compiled")
  run.name <- 'run32ref_test'
  wrkdir <- "C:/Users/clam/Desktop/luv/QC"
  result.dir <- file.path(wrkdir, "results", run.name)
}
dsn <- file.path(wrkdir, "data")

# find text files from results dir and copy to www dir
flist <- list.files('www', glob2rx('*.txt|*.html'), full.names = TRUE, include.dirs=TRUE, ignore.case=TRUE)
if (length(flist) > 0) file.remove(flist)
unlink(file.path('www', 'index_files'), recursive = TRUE)
flist <- list.files(result.dir, glob2rx('*.txt|*.html'), full.names = TRUE, include.dirs=TRUE, ignore.case=TRUE)
if (length(flist) > 0) file.copy(flist, 'www')

# remove index.html from www dir
fn <- list.files('www', glob2rx('index.html'), full.names = TRUE, include.dirs=TRUE, ignore.case=TRUE)
if (length(fn) > 0 && file.exists(fn)) file.remove(fn)
indexf.dir <- file.path(result.dir,"index_files")
if(file.exists(indexf.dir)) {
	file.copy(indexf.dir, 'www', recursive = TRUE)
	indexdirs <- c('bootstrap-3.3.5', 'jquery-1.11.3')
	for (dir in indexdirs)
  		unlink(file.path('www', 'index_files', dir), recursive = TRUE)
}


# lookup tables and shape names
faz.lookup <- read.table(file.path(dsn, "faz_names.txt"), header =TRUE, sep = "\t")
zone.lookup <- read.table(file.path(dsn, "zones.txt"), header =TRUE, sep = "\t")
city.lookup <- read.table(file.path(dsn, "cities.csv"), header =TRUE, sep = ",")

layer_zone <- "TAZ_2010_WGS84"
layer_faz <- "FAZ_2010_WGS84"
layer_city <- "JURIS_2014_WGS84"
layer_centers <- "centers_WGS84"

zone.shape <- readOGR(dsn=dsn,layer=layer_zone) 
faz.shape <- readOGR(dsn=dsn,layer=layer_faz) 
city.shape <- readOGR(dsn=dsn,layer=layer_city) 
centers <- readOGR(dsn=dsn, layer=layer_centers)

zone.shape$name_id <- zone.shape$TAZ
faz.shape$name_id <- faz.shape$FAZ10
city.shape$name_id <- city.shape$city_id

runname1 <- unlist(strsplit(run1,"[.]"))[[1]]
runnames2 <- sapply(strsplit(run2.all,"[.]"), function(x) x[1]) # can have multiple values
runs <- c(runname1, unlist(runnames2))
runnames <- c(run1, run2.all)

# build source table
alldata.table <- NULL 

for (r in 1:length(runnames)) {
  run2 <- run2.all[r] #change
  runname2 <- runnames2[r]
  
  geog.table <- NULL
  
  for (a in 1:length(geography)){
    indicators.table <- NULL
    
    for (i in 1:length(attribute)){
      filename <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
      datatable <- read.csv(file.path(base.dir, runnames[r],"indicators",filename), header = TRUE, sep = ",")
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
                                "employment"="Employment", 
                                "residential_units"="Residential Units")
      
      ifelse (is.null(indicators.table),
              indicators.table <- table, 
              indicators.table <- rbind(indicators.table, table))
      
    } # end of attribute loop
    indicators.table$geography <- geography[a]
    
    # append records by geography  
    ifelse (is.null(geog.table),
            geog.table <- indicators.table,
            geog.table <- rbind(geog.table, indicators.table))
    
  } # end of geography loop   
  geog.table$run <- runs[r]
  
  ifelse (is.null(alldata.table),
          alldata.table <- geog.table,
          alldata.table <- rbind(alldata.table, geog.table))
  
  alldt <- as.data.table(alldata.table)
  
} # end of runnames loop       



