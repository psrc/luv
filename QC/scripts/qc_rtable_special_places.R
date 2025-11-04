# Check special places
library(data.table)
source('templates/create_Rmd_blocks.R')
if(!interactive()) { # running using Makefile
	# Obtain inputs from the environment
	run1 <- Sys.getenv('QC_RUN1')
	base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
	result.dir <- Sys.getenv('QC_RESULT_PATH')
} else { # running interactively
	run1 <- "run_81.run_2016_07_05_16_00"
	base.dir <- "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
	run.name <- "run81"
	result.dir <- file.path("results", run.name)
}
year <- 2050
base.year <- 2023
if(!dir.exists(result.dir)) dir.create(result.dir)
indicator.path.run1 <- file.path(base.dir, run1, 'indicators')
freport <- file.path(result.dir, paste0('rtables_special_places.Rmd'))
if(file.exists(freport)) unlink(freport)
create.section(freport, title=paste('QC Special Places'))


lookup.table <- read.table('data/SpecialPlaces.csv', sep=',', header=TRUE)
places <- unique(lookup.table[,c('place_id', 'name')])
for (ind in c('households', 'population', 'employment')) {
	zone.values <- read.table(file.path(indicator.path.run1, paste0('zone__table__', ind, '.csv')), sep=',', header=TRUE)[,c('zone_id', paste(ind,c(base.year, year),sep='_'))]
	zone.values <- data.table(merge(zone.values, lookup.table[,c('zone_id', 'place_id')], by='zone_id'))
	place.data <- as.data.frame(zone.values[,list(base=sum(get(paste(ind, base.year,sep='_'))), total=sum(get(paste(ind, year,sep='_')))), by=place_id])
	place.data <- cbind(place.data, change=place.data$total - place.data$base, percent=round(100*(place.data$total-place.data$base)/place.data$base,1))
	place.data <- merge(places, place.data, by='place_id')
	place.data$name <- as.character(place.data$name)
	place.data <- place.data[,-1]
	colnames(place.data) <- c('special place', base.year, year, paste("change", base.year, "-", year), "percent change")
	add.text(freport, paste("####", ind, "\n"))
	add.table(freport, place.data)
}


