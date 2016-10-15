library(shiny)
library(plotly)
library(leaflet)
library(rgdal)
library(sp)
library(data.table)
library(magrittr)

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
  dsn <- file.path(wrkdir, "data")
} else {
  base.dir <- "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  #base.dir <- "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  run1 <- "run_99.run_2016_08_15_17_11"
  run2.all <- c("run_81.run_2016_07_05_16_00","luv_1.compiled")
  run.name <- 'run99'
  dsn <- "C:/Users/Christy/Desktop/luv/QC/data"
  #dsn <- "/Users/hana/ForecastProducts/LUV/QC/data"
  result.dir <- file.path("C:/Users/Christy/Desktop/luv/QC/results", run.name)
}

# find text files from results dir and copy to www dir
flist <- list.files('www', glob2rx('*.txt|*.html'), full.names = TRUE, include.dirs=TRUE, ignore.case=TRUE)
file.remove(flist)
flist <- list.files(result.dir, glob2rx('*.txt|*.html'), full.names = TRUE, include.dirs=TRUE, ignore.case=TRUE)
file.copy(flist, 'www')

# remove index.html from www dir
fn <- list.files('www', glob2rx('index*'), full.names = TRUE, include.dirs=TRUE, ignore.case=TRUE)
if (file.exists(fn)) file.remove(fn)

# lookup tables and shape names
faz.lookup <- read.table(file.path(dsn, "faz_names.txt"), header =TRUE, sep = "\t")
zone.lookup <- read.table(file.path(dsn, "zones.txt"), header =TRUE, sep = "\t")
city.lookup <- read.table(file.path(dsn, "cities.csv"), header =TRUE, sep = ",")

layer_faz <- "FAZ_2010_WGS84"
layer_zone <- "TAZ_2010_WGS84"
layer_city <- "JURIS_2014_WGS84"
layer_centers <- "centers_WGS84"

centers <- readOGR(dsn=dsn, layer=layer_centers)

runname1 <- unlist(strsplit(run1,"[.]"))[[1]]
runnames2 <- sapply(strsplit(run2.all,"[.]"), function(x) x[1]) # can have multiple values
runs <- c(runname1, unlist(runnames2))
runnames <- c(run1, run2.all)

# build source table
alldata.table <- NULL 

for (r in 1:length(runnames)) {
  run2 <- run2.all[r] #change?
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



