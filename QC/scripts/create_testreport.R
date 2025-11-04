# Prototype for creating a QC report

# Obtain inputs from the environment
run1 <- Sys.getenv('QC_RUN1')
base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
result.dir <- Sys.getenv('QC_RESULT_PATH')
if(!dir.exists(result.dir)) dir.create(result.dir)

options(stringsAsFactors=FALSE)

indicator.names <- c('households', 'population', 'employment')
geographies <- c('faz', 'zone')
indicator.path.run1 <- file.path(base.dir, run1, 'indicators')

# Check decreases
cat("\nChecking for decreases ...")
result <- NULL
years <- c(2023, 2050)
for (ind in indicator.names) {
	for (geo in geographies) {
		filename <- file.path(indicator.path.run1, paste0(geo, '__table__', ind, '.csv'))
		if(!file.exists(filename)) next
		ind.values <- read.table(filename, sep=',', header=TRUE)
		dif <- ind.values[,paste(ind,years[2],sep='_')] - ind.values[,paste(ind,years[1],sep='_')]
		negatives <- which(dif < 0)
		if(length(negatives)>0) {
			this.result <- cbind(ind.values[negatives, paste(geo, 'id', sep="_")], dif[negatives], round(dif[negatives]/ind.values[negatives,paste(ind,years[1],sep='_')]*100,2))
			this.result <- data.frame(indicator=rep(ind, length(negatives)), geography=rep(geo, length(negatives)), this.result)
			result <- rbind(result, this.result)
		}
	}
}
cat(" done.\n")
if(!is.null(result)) {
	colnames(result)[3:ncol(result)] <- c('geo_id', 'difference', 'percent')
	subs.faz <- subset(result, geography == 'faz' & abs(percent) > 10 & abs(difference) > 50)
	subs.faz <- subs.faz[order(subs.faz$percent, decreasing=FALSE),]
	print(subs.faz)
	write.table(subs.faz, file.path(result.dir, 'testreport.txt'), sep='\t', row.names=FALSE)
} else cat("Indicator files for testing not available.\n")
