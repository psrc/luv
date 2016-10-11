# Check activity units in RGCs

source('templates/create_Rmd_blocks.R')
if(!interactive()) { # running using Makefile
	# Obtain inputs from the environment
	run1 <- Sys.getenv('QC_RUN1')
	base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
	result.dir <- Sys.getenv('QC_RESULT_PATH')
} else { # running interactively
	run1 <- "run_81.run_2016_07_05_16_00"
	base.dir <- "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
	run.name <- "run81_test"
	result.dir <- file.path("results", run.name)
}
year <- 2040
base.year <- 2014
if(!dir.exists(result.dir)) dir.create(result.dir)
indicator.path.run1 <- file.path(base.dir, run1, 'indicators')
freport <- file.path(result.dir, paste0('rtables_rgc.Rmd'))
if(file.exists(freport)) unlink(freport)
create.section(freport, title=paste('QC Regional Growth Centers'))

# compute activity units
rgc.values <- city.values <- all.values <- NULL
for (ind in c('population', 'employment', 'residential_units', 'households')) {
	rgc.values[[ind]] <- read.table(file.path(indicator.path.run1, paste0('growth_center__table__', ind, '.csv')), sep=',', header=TRUE)[,c('growth_center_id', paste(ind,c(base.year, year),sep='_'))]
	city.values[[ind]] <- read.table(file.path(indicator.path.run1, paste0('city__table__', ind, '.csv')), sep=',', header=TRUE)[,c('city_id', paste(ind,c(base.year, year),sep='_'))]
	all.values[[ind]] <- read.table(file.path(indicator.path.run1, paste0('alldata__table__', ind, '.csv')), sep=',', header=TRUE)[,c('alldata_id', paste(ind,c(base.year, year),sep='_'))]
}
lookup.rgc <- read.table('data/growth_centers.csv', sep=',', header=TRUE)
lookup.rgc <- subset(lookup.rgc, growth_center_id >= 500)
#rgc.acres <- read.table(file.path(indicator.path.run1, 'growth_center__table__acres.csv'), sep=',', header=TRUE)[,c('growth_center_id', paste('acres',base.year,sep='_'))]
#colnames(rgc.acres)[2] <- 'acres'

# Shares of RGCs within region and city
#====================================
rgc.total <- merge(merge(rgc.values$households, rgc.values$residential_units, by='growth_center_id'), rgc.values$employment, by='growth_center_id')
city.total <- merge(merge(city.values$households, city.values$residential_units, by='city_id'), city.values$employment, by='city_id')
all.total <- merge(merge(all.values$households, all.values$residential_units, by='alldata_id'), all.values$employment, by='alldata_id')
for (ind in c('households', 'residential_units', 'employment')) {
	rgc.total <- cbind(rgc.total, change=rgc.total[[paste(ind, year, sep="_")]] - rgc.total[[paste(ind, base.year, sep="_")]])
	colnames(rgc.total)[ncol(rgc.total)] <- paste0(ind,"_change")
	city.total <- cbind(city.total, change=city.total[[paste(ind, year, sep="_")]] - city.total[[paste(ind, base.year, sep="_")]])
	colnames(city.total)[ncol(city.total)] <- paste0(ind,"_change")
	all.total <- cbind(all.total, change=all.total[[paste(ind, year, sep="_")]] - all.total[[paste(ind, base.year, sep="_")]])
	colnames(all.total)[ncol(all.total)] <- paste0(ind,"_change")
}
rgc.total <- subset(rgc.total, growth_center_id >= 500)
rgc.total.nomic <- subset(rgc.total, growth_center_id < 600)
rgc.total.table <- colSums(rgc.total.nomic[,2:ncol(rgc.total.nomic)])
lookup.rgc.nomic <- subset(lookup.rgc, growth_center_id < 600)
city.total.nomic <- city.total[city.total$city_id %in% lookup.rgc.nomic$city_id,]
city.total.table <- colSums(city.total.nomic[,2:ncol(city.total.nomic)])
rgc.with.city <- merge(merge(rgc.total.nomic, lookup.rgc.nomic[,c('growth_center_id', 'city_id', 'name')], by='growth_center_id'), city.total, by='city_id')
detail.report.files <- list()
res.region <- res.city <- NULL
for (ind in c('households', 'residential_units', 'employment')) {
	res.region <- rbind(res.region, data.frame(indicator=ind, 
												total.base=round(100*rgc.total.table[[paste(ind, base.year, sep="_")]]/all.total[[paste(ind, base.year, sep="_")]],1),
												total=round(100*rgc.total.table[[paste(ind, year, sep="_")]]/all.total[[paste(ind, year, sep="_")]],1), 
												change=round(100*rgc.total.table[[paste0(ind, "_change")]]/all.total[[paste0(ind, "_change")]],1)))
	res.city <- rbind(res.city, data.frame(indicator=ind, 
												total.base=round(100*rgc.total.table[[paste(ind, base.year, sep="_")]]/city.total.table[[paste(ind, base.year, sep="_")]],1),
												total=round(100*rgc.total.table[[paste(ind, year, sep="_")]]/city.total.table[[paste(ind, year, sep="_")]],1),
												change=round(100*rgc.total.table[[paste0(ind, "_change")]]/city.total.table[[paste0(ind, "_change")]],1)))
	ind.report <- data.frame(id=rgc.with.city[['growth_center_id']],
							 share.base=round(100*rgc.with.city[[paste0(ind, "_", base.year, ".x")]]/rgc.with.city[[paste0(ind, "_", base.year, ".y")]],1),
							 share=round(100*rgc.with.city[[paste0(ind, "_", year, ".x")]]/rgc.with.city[[paste0(ind, "_", year, ".y")]],1),
							 share.change=round(100*rgc.with.city[[paste0(ind, "_change.x")]]/rgc.with.city[[paste0(ind, "_change.y")]],1),
							 name = rgc.with.city[['name']])
	colnames(ind.report)[2:(ncol(ind.report)-1)] <-  c(paste("share", base.year), paste("share", year), paste("share change", base.year, "-", year))
	ind.report <- ind.report[order(as.character(ind.report$name)), ]
	#detail.report.files[[ind]] <- file.path(result.dir, paste0("qc_rtable_rgc_", ind, ".txt"))
	detail.report.files[[ind]] <- paste0("qc_rtable_rgc_", ind, ".txt")
	write.table(ind.report, file.path(result.dir, detail.report.files[[ind]]), row.names=FALSE, sep="\t")
}
res.region$indicator <- as.character(res.region$indicator)
res.city$indicator <- as.character(res.city$indicator)
colnames(res.region)[2:4] <- c(paste("total", base.year), paste("total", year), paste("change", base.year, "-", year))
colnames(res.city)[2:4] <- colnames(res.region)[2:4]

create.subsection(freport, title='Allocation to RGCs')
add.text(freport, "#### Share of region (%): \n")
add.table(freport, res.region)
add.text(freport, "#### Share of cities with RGCs (%):\n")
add.table(freport, res.city)
add.text(freport, "More details on share of cities for: ")
add.text(freport, paste0("[", c('households', 'residential_units', 'employment'), "](", unlist(detail.report.files), ")", collapse=', ')) 
add.text(freport, "\n")
	
# RGC vs the rest of region and city
#====================================
city.total.allrgc <- city.total[city.total$city_id %in% lookup.rgc$city_id,]
city.total.all.table <- colSums(city.total.allrgc[,2:ncol(city.total.allrgc)])
rgcall.with.city <- merge(merge(rgc.total, lookup.rgc[,c('growth_center_id', 'city_id', 'name')], by='growth_center_id'), city.total, by='city_id')
data.by.type <- list(RGC=subset(rgc.total, growth_center_id < 600), MIC=subset(rgc.total, growth_center_id >= 600),
					cities=city.total.all.table, region=all.total)
add.text(freport, "#### By type, rest of cities and region: \n")
for (ind in c('households', 'residential_units', 'employment')) {
	type.table <- NULL
	for(ctype in c("RGC", "MIC")) {
		base <- sum(data.by.type[[ctype]][[paste0(ind, "_", base.year)]])
		total <- sum(data.by.type[[ctype]][[paste0(ind, "_", year)]])
		type.table <- rbind(type.table, data.frame(area=ctype, base=base, total=total, change=total-base, percent=round(100*(total-base)/base,1),
													AAPC=round(100*((total/base)^(1/(year-base.year))-1), 2)))
	}
	for(ctype in c("cities", "region")) {
		base <- sum(data.by.type[[ctype]][[paste0(ind, "_", base.year)]]) - sum(type.table$base)
		total <- sum(data.by.type[[ctype]][[paste0(ind, "_", year)]]) - sum(type.table$total)
		type.table <- rbind(type.table, data.frame(area=paste("rest of", ctype), base=base, total=total, change=total-base, percent=round(100*(total-base)/base,1),
													AAPC=round(100*((total/base)^(1/(year-base.year))-1), 2)))
	}
	sums <- colSums(type.table[2:4])
	type.table <- rbind(type.table, data.frame(area="Total", base=sums['base'], total=sums['total'], change=sums['change'],
						percent=round(100*(sums['total']-sums['base'])/sums['base'],1),
						  AAPC=round(100*((sums['total']/sums['base'])^(1/(year-base.year))-1), 2)))
	add.text(freport, paste("#####", ind, "\n"))
	type.table$area <- as.character(type.table$area)
	colnames(type.table)[2:5] <- c(base.year, year, paste("change", base.year, "-", year), "percent change")
	add.table.highlight(freport, type.table, nrow(type.table), color="lightgrey")
}
	
# AU per unit for RGCs
#======================
rgc.au <- merge(merge(rgc.values$population, rgc.values$employment, by='growth_center_id'), lookup.rgc, by='growth_center_id')
rgc.au <- cbind(rgc.au, total.base=rgc.au[[paste('population',base.year,sep='_')]] + rgc.au[[paste('employment',base.year,sep='_')]], 
								total=rgc.au[[paste('population',year,sep='_')]] + rgc.au[[paste('employment',year,sep='_')]])
rgc.au <- subset(rgc.au, growth_center_id >= 500)
rgc.au <- cbind(rgc.au, au.base=round(rgc.au$total.base/rgc.au$acres,1), au=round(rgc.au$total/rgc.au$acres,1))

au.table.all <- rgc.au[,c('growth_center_id', 'name', 'total.base', 'au.base', 'total', 'au')]
au.table.all$name <- as.character(au.table.all$name)

au.table <- subset(au.table.all, growth_center_id < 600)
au.table.output <- au.table
colnames(au.table.output)[1] <- "id"
colnames(au.table.output)[3:ncol(au.table.output)] <- c(paste(c('AU', 'AU/acre'), base.year), paste(c('AU', 'AU/acre'), year))

create.subsection(freport, title='Activity Units per Acre - RGCs')
add.table.highlight(freport, au.table.output, which(au.table$au < 45))

# AU per unit for MICs
au.mic.table <- subset(au.table.all, growth_center_id >= 600)
au.mic.table.output <- au.mic.table
colnames(au.mic.table.output) <- colnames(au.table.output)
create.subsection(freport, title='Activity Units per Acre - MICs')
add.table.highlight(freport, au.mic.table.output, which(au.mic.table$au < 45))

# details table
outtable <- rgc.au
outtable <- cbind(outtable, popu.base=round(outtable[[paste('population',base.year,sep='_')]]/outtable$acres,1), 
				empu.base=round(outtable[[paste('employment',base.year,sep='_')]]/outtable$acres,1),
				popu=round(outtable[[paste('population',year,sep='_')]]/outtable$acres,1), 
				empu=round(outtable[[paste('employment',year,sep='_')]]/outtable$acres,1))
outtable <- outtable[,c('growth_center_id', paste('population',base.year,sep='_'), 'popu.base', paste('employment',base.year,sep='_'), 'empu.base',
					paste('population',year,sep='_'), 'popu', paste('employment',year,sep='_'), 'empu', 'name')]
colnames(outtable)[1] <- 'id'
colnames(outtable)[seq(2, by=2, length=4)] <-  c(paste0(c('Pop_', 'Emp_'), base.year), paste0(c('Pop_', 'Emp_'), year))
colnames(outtable)[seq(3, by=2, length=4)] <-  c(paste0(c('Pop/U_', 'Emp/U_'), base.year), paste0(c('Pop/U_', 'Emp/U_'), year))
#au.detail.file <- file.path(result.dir, paste0("qc_rtable_rgc_au_details.txt"))
au.detail.file <- "qc_rtable_rgc_au_details.txt"
write.table(outtable, file.path(result.dir, au.detail.file), row.names=FALSE, sep="\t")
add.text(freport, paste0("[See more details](", au.detail.file, ")\n\n"))

# Job loss in MIC
mic.jobs <- subset(rgc.values$employment, growth_center_id >= 600)
mic.jobs <- cbind(mic.jobs, difference=mic.jobs[,3] - mic.jobs[,2])
mic.jobs <- merge(lookup.rgc[,c('growth_center_id', 'name')], mic.jobs,  by='growth_center_id')
mic.jobs$name <- as.character(mic.jobs$name)
idx1 <- which(mic.jobs$difference <= -10000)
idx2 <- which(mic.jobs$difference > -10000 & mic.jobs$difference < 0)
colnames(mic.jobs)[1] <- "id"
create.subsection(freport, title='Job Loss in MICs')
add.table.highlight(freport, mic.jobs, highlight=idx1, highlight2=idx2, color='pink', color2='yellow')
