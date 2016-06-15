# Compare jurisdictional totals; must be dead match  

library(data.table)
options(StringAsFactors=FALSE)

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
ctyear <- 2040
if(!dir.exists(result.dir)) dir.create(result.dir)

indicator.names <- c('households', 'employment')
indicator_settings <- list(households=c("total_number_of_households", "household"), employment=c("total_number_of_jobs", "employment"))
indicator.path.run1 <- file.path(base.dir, run1, 'indicators')

result <- report <- NULL
for (ind in indicator.names) {
	# Read control totals
	CT <- read.table(file.path('data', paste0('annual_', indicator_settings[[ind]][2], '_control_totals.csv')), sep=',', header=TRUE)
	CT.by.jur <- data.table(CT)[,list(CT=sum(get(indicator_settings[[ind]][1]))), by=.(city_id, year)]
	CT.by.jur <- CT.by.jur[year == ctyear,]
	ind.values <- data.table(read.table(file.path(indicator.path.run1, paste0('city__table__', ind, '.csv')), 
						sep=',', header=TRUE)[,c('city_id', paste(ind,ctyear,sep='_'))])
	ct.join <- merge(CT.by.jur, ind.values, by='city_id')
	no.match <- ct.join[CT != get(paste(ind, ctyear, sep="_")),]
	this.report <- data.frame(indicator=ind, total=nrow(no.match), max.percent=NA)
	if(nrow(no.match) > 0) {
		simcol <- paste0("simulated_", ctyear)
		colnames(no.match)[colnames(no.match) == paste(ind, ctyear, sep="_")] <- simcol
		dif <- no.match[[simcol]] - no.match$CT
		dif.percent <- dif/no.match[[simcol]] * 100
		this.result <- cbind(data.frame(indicator=rep(ind, nrow(no.match)),  no.match), 
							 data.frame(difference=dif, percent=round(dif.percent,1)))
		result <- rbind(result, this.result)
		this.report[,'max.percent'] <- max(abs(this.result$percent))
	}
	report <- rbind(report, this.report)
}

# output result table
res.file <- file.path(result.dir, 'qc_rtable_CTmatch.txt')
write.table(result, res.file, sep='\t', row.names=FALSE)

# write report
source('templates/create_Rmd_blocks.R')
freport <- file.path(result.dir, 'rtables_CTmatch.Rmd')
if(file.exists(freport)) unlink(freport)
create.section(freport, title='QC Control Total Mismatch')
report$indicator <- as.character(report$indicator)
add.table(freport, report)
add.text(freport, paste0("[See more details](", paste0('file://', res.file), ")"))


