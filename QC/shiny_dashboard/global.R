library(shiny)
library(plotly) # version 4.0 and up
library(leaflet)
library(rgdal)
library(sp)
library(data.table)
library(magrittr)
library(shinythemes)
library(stringr)
library(DT)
library(scales)
library(lemon)

enableBookmarking(store = "server")

# environment inputs
attribute <- c("population", "households","employment", "residential_units")
geography <- c( "zone", "faz", "city")
years <- seq(2014, 2050) 
luv.years <- c(2014, 2015, 2020, 2025, 2030, 2035, 2040)
addn.yrs <- setdiff(years, luv.years) %>% paste0("yr", .)
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
  rund <- "/media/aws-prod-file01/model\ data/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"
  # rund <- "L:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs"
  base <- list(L-Aws01 = file.path(rund, "awsmodel01"),
               L-Aws02 = file.path(rund, "awsmodel02"),
               L-Aws03 = file.path(rund, "awsmodel03"),
               L-Aws04 = file.path(rund, "awsmodel04"),
               L-Aws05 = file.path(rund, "awsmodel05"),
               L-Aws06 = file.path(rund, "awsmodel06"),
               L-Aws07 = file.path(rund, "awsmodel07"),
               L-Aws08 = file.path(rund, "awsmodel08"),
               # Aws01 = "/media/aws-model01e/opusgit/urbansim_data/data/psrc_parcel/runs",
               # Aws02 = "/media/aws-model02e/opusgit/urbansim_data/data/psrc_parcel/runs",
               # Aws03 = "/media/aws-model03e/opusgit/urbansim_data/data/psrc_parcel/runs",
               # Aws04 = "/media/aws-model04e/opusgit/urbansim_data/data/psrc_parcel/runs",
               # Aws05 = "/media/aws-model05e/opusgit/urbansim_data/data/psrc_parcel/runs",
               # Aws06 = "/media/aws-model06e/opusgit/urbansim_data/data/psrc_parcel/runs",
               # Aws07 = "/media/aws-model07e/opusgit/urbansim_data/data/psrc_parcel/runs",
               # Aws08 = "/media/aws-model08e/opusgit/urbansim_data/data/psrc_parcel/runs",
               # Aws09 = "/media/aws-model09e/opusgit/urbansim_data/data/psrc_parcel/runs",
               Modelsrv5 = "/media/modelsrv5d/opusgit/urbansim_data/data/psrc_parcel/runs",
               Modelsrv6 = "/media/modelsrv6d/opusgit/urbansim_data/data/psrc_parcel/runs",
               Modelsrv8 = "/media/modelsrv8d/opusgit/urbansim_data/data/psrc_parcel/runs",
               Modelsrv3 = "/media/modelsrv3e/opusgit/urbansim_data/data/psrc_parcel/runs"
              )
  # base <- list(Aws01 = file.path(rund, "awsmodel01"),
  #              Aws02 = file.path(rund, "awsmodel02"),
  #              Aws03 = file.path(rund, "awsmodel03"),
  #              Aws04 = file.path(rund, "awsmodel04"),
  #              Aws05 = file.path(rund, "awsmodel05"),
  #              Aws06 = file.path(rund, "awsmodel06"),
  #              Aws07 = file.path(rund, "awsmodel07"),
  #              Aws08 = file.path(rund, "awsmodel08"),
  #              # Aws01 = "//aws-model01/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
  #              # Aws02 = "//aws-model02/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
  #              # Aws03 = "//aws-model03/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
  #              # Aws04 = "//aws-model04/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
  #              # Aws05 = "//aws-model05/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
  #              # Aws06 = "//aws-model06/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
  #              # Aws07 = "//aws-model07/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
  #              # Aws08 = "//aws-model08/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
  #              # Aws09 = "//aws-model09/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
  #              Modelsrv5 = "//modelsrv5/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
  #              Modelsrv6 = "//modelsrv6/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
  #              Modelsrv8 = "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs",
  #              Modelsrv3 = "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  #              )
  # base <- list(Modelsrv3 = "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs",
  #             Modelsrv8 = "/Volumes/d$/opusgit/urbansim_data/data/psrc_parcel/runs")
  # wrkdir <- "C:/Users/CLam/Desktop/luv/QC"
  wrkdir <- "/home/shiny/apps/luv/QC"
  #wrkdir <- "/Users/hana/ForecastProducts/LUV/QC"
}

dsn <- file.path(wrkdir, "data")

# scan all modelservers for runs and setnames
allruns <- list()
for (b in 1:length(base)) {
  fdirlist <- list.dirs(base[[b]], full.names = TRUE, recursive = FALSE)
  ndirlist <- list.dirs(base[[b]], full.names = FALSE, recursive = FALSE)
  dirlist <- setNames(fdirlist, ndirlist)
  allruns[[length(allruns)+1]] <- dirlist
}
names(allruns) <- names(base) %>% toupper

# lookup tables and shape names
faz.lookup <- read.table(file.path(dsn, "faz_names.txt"), header =TRUE, sep = "\t")
zone.lookup <- read.table(file.path(dsn, "zones.txt"), header =TRUE, sep = "\t")
city.lookup <- read.table(file.path(dsn, "cities.csv"), header =TRUE, sep = ",")
zonecnty.lookup <- read.table(file.path(dsn, "zonecnty.txt"), header =TRUE, sep = "\t")
rgc.lookup <- read.table(file.path(dsn, "growth_centers.csv"),header=TRUE, sep=',') %>% subset(growth_center_id >= 500)
splaces.lookup <- read.table(file.path(dsn, 'SpecialPlaces.csv'), header=TRUE, sep=',')
demog.lookup <- read.csv(file.path(dsn, "demographic_groups.csv"))
faz_lgarea.lookup <- read.table(file.path(dsn, "cities_faz_lgarea.csv"), header =TRUE, sep = ",")

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
centers$name_id <- centers$ID