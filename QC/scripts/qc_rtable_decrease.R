# Create a table with locations that have negative change 
# between 2014 and 2040 in given indicators

# Obtain inputs from the environment
run1 <- Sys.getenv('QC_RUN1')
base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
result.dir <- Sys.getenv('QC_RESULT_PATH')
absolute.threshold <- as.integer(Sys.getenv('DECREASE_THRESHOLD'))
percent.threshold <- as.integer(Sys.getenv('DECREASE_THRESHOLD_PERCENT'))
if(is.na(absolute.threshold)) absolute.threshold <- 0
if(is.na(percent.threshold)) percent.threshold <- 0
years <- c(2014, 2040)

if(!dir.exists(result.dir)) dir.create(result.dir)

options(stringsAsFactors=FALSE)

indicator.names <- c('households', 'population', 'employment')
geographies <- c('faz', 'zone')
indicator.path.run1 <- file.path(base.dir, run1, 'indicators')

# Check decreases
result <- report <- NULL
for (ind in indicator.names) {
	for (geo in geographies) {
		ind.values <- read.table(file.path(indicator.path.run1, paste0(geo, '__table__', ind, '.csv')), sep=',', header=TRUE)
		dif <- ind.values[,paste(ind,years[2],sep='_')] - ind.values[,paste(ind,years[1],sep='_')]
		dif.percent <- -dif/ind.values[,paste(ind,years[1],sep='_')]*100
		negatives <- dif < -absolute.threshold
		if(percent.threshold > 0) negatives <- negatives & dif.percent < percent.threshold
		negatives <- which(negatives)
		lneg <- length(negatives)
		this.report <- data.frame(indicator=ind, geo=geo, total=nrow(ind.values), negat=lneg, remains=nrow(ind.values)-lneg, percent=round(lneg/nrow(ind.values)*100,2),
									max.neg=NA, max.loc=NA, median.neg=NA)
		if(lneg>0) {				
			this.result <- cbind(ind.values[negatives, paste(geo, 'id', sep="_")], dif[negatives], round(dif.percent[negatives],2))
			this.result <- data.frame(indicator=rep(ind, lneg), geography=rep(geo, lneg), this.result)
			result <- rbind(result, this.result)
			this.report[,'max.neg'] <- -min(dif[negatives])
			this.report[,'max.loc'] <- ind.values[which.min(dif[negatives]), paste(geo, 'id', sep="_")]
			this.report[,'median.neg'] <- -median(dif[negatives])
		}
		report <- rbind(report, this.report)
	}
}
# output result table
colnames(result)[3:ncol(result)] <- c('geo_id', 'difference', 'percent')
write.table(result, file.path(result.dir, paste0('qc_rtable_decrease_', absolute.threshold, '_', percent.threshold, '.txt')), sep='\t', row.names=FALSE)

# write into report.txt
freport <- file.path(result.dir, 'report.txt')
cat('\nQC DECREASES  (', years[1], ' - ', years[2], ')', file=freport, append=TRUE)
cat('\nThresholds:', absolute.threshold, ' (absolute), ', percent.threshold, ' (percent)', file=freport, append=TRUE)
cat('\n****************************************\n\n', file=freport, append=TRUE)
write.table(report, file=freport, append=TRUE, sep='\t', row.names=FALSE, quote=FALSE)

#print(report)
