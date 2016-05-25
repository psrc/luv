# Prototype for creating a QC report

# Obtain inputs from the environment
run1 <- Sys.getenv('QC_RUN1')
run2 <- Sys.getenv('QC_RUN2')
base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
result.dir <- Sys.getenv('QC_RESULT_PATH')
if(!dir.exists(result.dir)) dir.create(result.dir)

options(stringsAsFactors=FALSE)

indicator.names <- c('households', 'population', 'employment')
geographies <- c('faz', 'zone')
indicator.path.run1 <- file.path(base.dir, run1, 'indicators')

# Check decreases
result <- NULL
years <- c(2014, 2040)
for (ind in indicator.names) {
	for (geo in geographies) {
		ind.values <- read.table(file.path(indicator.path.run1, paste0(geo, '__table__', ind, '.csv')), sep=',', header=TRUE)
		dif <- ind.values[,paste(ind,years[2],sep='_')] - ind.values[,paste(ind,years[1],sep='_')]
		negatives <- which(dif < 0)
		if(length(negatives)>0) {
			this.result <- cbind(ind.values[negatives, paste(geo, 'id', sep="_")], dif[negatives], round(dif[negatives]/ind.values[negatives,paste(ind,years[1],sep='_')]*100,2))
			this.result <- data.frame(indicator=rep(ind, length(negatives)), geography=rep(geo, length(negatives)), this.result)
			result <- rbind(result, this.result)
		}
	}
}
colnames(result)[3:ncol(result)] <- c('geo_id', 'difference', 'percent')
write.table(result, file.path(result.dir, 'qc_rtable_decrease.txt'), sep='\t', row.names=FALSE)
