# Check activity units in RGCs

if(!interactive()) { # running using Makefile
	# Obtain inputs from the environment
	run1 <- Sys.getenv('QC_RUN1')
	base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
	result.dir <- Sys.getenv('QC_RESULT_PATH')
} else { # running interactively
	run1 <- "run_71.run_2016_05_26_12_41"
	base.dir <- "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
	run.name <- "run71"
	result.dir <- file.path("results", run.name)
}
year <- 2040
base.year <- 2014
if(!dir.exists(result.dir)) dir.create(result.dir)
indicator.path.run1 <- file.path(base.dir, run1, 'indicators')

# compute activity units
rgc.values <- city.values <- NULL
for (ind in c('population', 'employment')) {
	rgc.values[[ind]] <- read.table(file.path(indicator.path.run1, paste0('growth_center__table__', ind, '.csv')), sep=',', header=TRUE)[,c('growth_center_id', paste(ind,c(base.year, year),sep='_'))]
	city.values[[ind]] <- read.table(file.path(indicator.path.run1, paste0('city__table__', ind, '.csv')), sep=',', header=TRUE)[,c('city_id', paste(ind,c(base.year, year),sep='_'))]
}
rgc.acres <- read.table(file.path(indicator.path.run1, 'growth_center__table__acres.csv'), sep=',', header=TRUE)[,c('growth_center_id', paste('acres',base.year,sep='_'))]
colnames(rgc.acres)[2] <- 'acres'
rgc.total <- merge(merge(rgc.values$population, rgc.values$employment, by='growth_center_id'), rgc.acres, by='growth_center_id')
rgc.total <- cbind(rgc.total, total.base=rgc.total[[paste('population',base.year,sep='_')]] + rgc.total[[paste('employment',base.year,sep='_')]], 
								total=rgc.total[[paste('population',year,sep='_')]] + rgc.total[[paste('employment',year,sep='_')]])
rgc.total <- subset(rgc.total, growth_center_id >= 500)
rgc.total <- cbind(rgc.total, au.base=round(rgc.total$total.base/rgc.total$acres,1), au=round(rgc.total$total/rgc.total$acres,1))

city.acres <- read.table(file.path(indicator.path.run1, 'city__table__acres.csv'), sep=',', header=TRUE)[,c('city_id', paste('acres',base.year,sep='_'))]
colnames(city.acres)[2] <- 'acres'
city.total <- merge(merge(city.values$population, city.values$employment, by='city_id'), city.acres, by='city_id')
city.total <- cbind(city.total, total.base=city.total[[paste('population',base.year,sep='_')]] + city.total[[paste('employment',base.year,sep='_')]], 
								total=city.total[[paste('population',year,sep='_')]] + city.total[[paste('employment',year,sep='_')]])
city.total <- cbind(city.total, au.base=city.total$total.base/city.total$acres, au=city.total$total/city.total$acres)

lookup.rgc <- read.table('data/growth_centers.csv', sep=',', header=TRUE)
rgc.total <- merge(rgc.total, lookup.rgc, by='growth_center_id')

au.table.all <- rgc.total[,c('growth_center_id', 'name', 'total.base', 'au.base', 'total', 'au')]
au.table.all$name <- as.character(au.table.all$name)

# AU per unit for RGCs
au.table <- subset(au.table.all, growth_center_id < 600)
au.table.output <- au.table
colnames(au.table.output)[3:ncol(au.table.output)] <- c(paste(c('AU', 'AU/acre'), base.year), paste(c('AU', 'AU/acre'), year))
source('templates/create_Rmd_blocks.R')
freport <- file.path(result.dir, paste0('rtables_rgc_auperacre.Rmd'))
if(file.exists(freport)) unlink(freport)
create.section(freport, title=paste('QC Regional Growth Centers'))
create.subsection(freport, title='Activity Units per Acre - RGCs')
add.table.highlight(freport, au.table.output, which(au.table$au < 45))

# AU per unit for MICs
au.mic.table <- subset(au.table.all, growth_center_id >= 600)
au.mic.table.output <- au.mic.table
colnames(au.mic.table.output) <- colnames(au.table.output)
create.subsection(freport, title='Activity Units per Acre - MICs')
add.table.highlight(freport, au.mic.table.output, which(au.mic.table$au < 45))

# Job loss in MIC
mic.jobs <- subset(rgc.values$employment, growth_center_id >= 600)
mic.jobs <- cbind(mic.jobs, difference=mic.jobs[,3] - mic.jobs[,2])
mic.jobs <- merge(lookup.rgc[,c('growth_center_id', 'name')], mic.jobs,  by='growth_center_id')
mic.jobs$name <- as.character(mic.jobs$name)
idx1 <- which(mic.jobs$difference <= -10000)
idx2 <- which(mic.jobs$difference > -10000 & mic.jobs$difference < 0)
create.subsection(freport, title='Job Loss in MICs')
add.table.highlight(freport, mic.jobs, highlight=idx1, highlight2=idx2, color='pink', color2='yellow')
