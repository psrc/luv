# Script for generating LUV R report on regional geography level
# Hana Sevcikova, PSRC
# November, 2025
#

trim <- function (x)  sub("^\\s+", "", x)
curdir <- getwd()

##### BEGIN USER SETTINGS ######
if(!interactive()) { # running using Makefile
	# Obtain inputs from the environment
	if(!file.exists('cityreportLUV.R')) # probably called via Makefile from the QC directory
		setwd('../Rreports') 
	run1 <- Sys.getenv('QC_RUN1')
	base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
	result.dir <- "." # Sys.getenv('QC_RESULT_PATH')
	other.runs <- Sys.getenv('RREPORT_RUNS')
	other.runs <- trim(unlist(strsplit(other.runs, ",")))
	annual <- as.logical(Sys.getenv('RREPORT_ANNUAL', 'FALSE'))
	ci.runs <- c()
	ci.dir <- "/modelsrv6/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
	base.year <- as.integer(Sys.getenv('RUN1_BASE_YEAR'))
	end.year <- as.integer(Sys.getenv('RUN1_END_YEAR'))
} else { # running interactively
	#run1 <- "run_134.run_2018_05_12_13_11"
	run1 <- "run_8.run_2018_05_08_16_46"
	other.runs <- c('run_16.run_2018_05_14_15_03', 'run_17.run_2018_05_14_15_06', 'run_18.run_2018_05_15_12_54', 'run_19.run_2018_05_15_12_55')
	#other.runs <- c()
	base.dir <- "~/d6$/opusgit/urbansim_data/data/psrc_parcel/runs"
	run.name <- "run_134"
	run.name <- "run_8"
	result.dir <- "."
	ci.dir <- "~/d6$/opusgit/urbansim_data/data/psrc_parcel/runs"
	ci.runs <- c("8_5runs")
	annual <- FALSE
	base.year <- 2023
	end.year <- 2050
}
runs <- c(run1, other.runs)
#run.numbers <- sapply(strsplit(sapply(strsplit(runs, '[.]'), function(x) x[1]), '_'), function(x) x[2])
run.numbers <- sapply(strsplit(runs, '[.]'), function(x) x[1])

# Directory containing the runs with indicators. 
# The indicators directory must contain files of the type 'faz__tab__xxx.tab'
sim.dir <- base.dir
#sim.prefix <- 'run_' # name of the run sub-directory without the run number, e.g. 'run_' for directory run_199

show.all.years <- annual # should results from all years be shown as dotted lines
not.all.years <- c() # if show.all.years is TRUE, put here runs that are exceptions

geography <- 'fips_rgs'
output.file.name <- file.path(result.dir, paste(geography, 'reportLUV', if(show.all.years) 'annual' else '', '_', paste(run.numbers, collapse='_'), sep=''))

#years <- c(2014, seq(2015, 2050, by=5))
years <- c(2023, seq(2025, 2040, by=5),2044,2045,2050)
years <- years[years >= base.year & years <= end.year]
#years.for.table <- c(2014, 2015, seq(2020, 2050, by=10))
years.for.table <- c(2023, seq(2030, 2040, by=10),2044,2045,2050)
years.for.table <- years.for.table[years.for.table >= base.year & years.for.table <= end.year]
all.years <- if(show.all.years) base.year:end.year else c()

# Runs with CIs
ci.run.name <- list()
PIs <- 80

save.data.as.ascii <- FALSE
###### END USER SETTINGS ############


library(ggplot2)
library(grid)
library(gridExtra)

lyears <- length(years)
indicators <- c('households',  'population', 'employment')
indicators.obs <- as.list(indicators)
ci.names <- list(employment='employment', households='households', population='population',
                 employmentAn='employment', householdsAn='households', populationAn='population')
titles <- list(households='Households', employment='Employment', population='HH Population')
if(show.all.years) {
	indicators <- paste0(indicators, "An")
	names(titles) <- paste0(names(titles), "An")
}
names(indicators.obs) <- indicators

output.file.name.pdf <- paste(output.file.name,  'pdf', sep='.')
output.file.name.txt <- paste(output.file.name,  'txt', sep='.')

wrkdir <- getwd()

remove.na <- function(data)
	apply(data, c(1,2), function(x) if(trim(x)=='NA') '' else x)

sim <- ids <- CIs <- trend.data <- ids.tr <- list()

id.correspondence <- data.frame(read.table(file.path(wrkdir, 'data', 'fips_rgs.csv'), sep=',', header=TRUE))
id.correspondence <- id.correspondence[order(id.correspondence[,'fips_rgs_id']),]

for (what in indicators) {
	# Load observed data
	trend.file.name <- file.path(wrkdir, 'data',  paste(indicators.obs[[what]], '_observed_', geography, '.txt', sep=''))
	if(file.exists(trend.file.name)) {
		trend.data.raw <- data.frame(read.table(trend.file.name, sep='\t', header=TRUE))
		ids.tr[[what]] <- trend.data.raw[,1]
		trend.data[[what]] <- trend.data.raw[order(ids.tr[[what]]), 2:ncol(trend.data.raw)]
		ids.tr[[what]] <- sort(ids.tr[[what]])
		colnames(trend.data[[what]]) <- substr(colnames(trend.data[[what]]), 2,5)
		trend.data[[what]] <- trend.data[[what]][,is.element(colnames(trend.data[[what]]), as.character(years))]
	}
	sim[[what]] <- ids[[what]] <- CIs[[what]] <- list()
	for(irun in 1:length(runs)) {
		run <- runs[irun]
		# Load indicators
		data <- read.table(file.path(sim.dir, run, 
						'indicators', paste0(geography, '__table__', what, '.csv')), sep=',', header=TRUE)
		sim[[what]][[run]] <- data[,2:ncol(data)]
		ids[[what]][[run]] <- data[,1]
		sim[[what]][[run]] <- sim[[what]][[run]][order(ids[[what]][[run]]),]
		ids[[what]][[run]] <- sort(ids[[what]][[run]])
	}
	CIs[[what]] <- NULL
	for(run in ci.runs) {
	    crun.name <- if(is.null(ci.run.name[[as.character(run)]])) run else ci.run.name[[as.character(run)]]
	    for(year in years[which(years>=2020)[1]:lyears]) {
            ci.file.name <- file.path(ci.dir, paste0('bm_', run), 
	                                  paste0(year, '_', geography, '_', ci.names[[what]], ".txt"))
	        if(!file.exists(ci.file.name)) next
	        this.ci <- read.table(ci.file.name, header=TRUE)[, c("id", "median", paste0("lower_", PIs), paste0("upper_", PIs))]
	        colnames(this.ci) <- c('id', 'median', 'lower', 'upper')
	        CIs[[what]] <- rbind(CIs[[what]], 
	                                    cbind(data.frame(run=crun.name, Time=year), 
	                                          this.ci))
	    }
	}
	
	trend.data[[what]][] <- NA
}
area.names <-  id.correspondence[,c('fips_rgs_id', 'fips_rgs_name')]
colnames(area.names) <- c('id', 'name')
zones <- ids[[indicators[1]]][[runs[1]]]
years.for.df <- sort(unique(c(years, all.years)))
lyears.for.df <- length(years.for.df)
file.append <- FALSE
cat('\nProcessing ', length(zones), ' geographies:\n')
pdf(output.file.name.pdf, width=12, height=9)
for(geo in sort(zones)) {
	cat(geo, ', ')
	flush.console()
  	g <- gtab <- list()
	for (what in indicators) {
		not_found <- FALSE
		tabDF <- data.frame(Time=years.for.df)
		last.table.columns <- NA	
		run.table.columns <- c()
		for(irun in 1:length(runs)) {
			run <- runs[irun]
			runn <- run.numbers[irun]
			idx <- which(ids[[what]][[run]]==geo)
			if (length(idx) <=0) {not_found <- TRUE; break}
			coln <- colnames(sim[[what]][[run]])
			run.cols <- substr(coln, nchar(coln)-3, nchar(coln))
			col.idx <- which(run.cols %in% as.character(years.for.df))
			amount <- sim[[what]][[run]][idx,col.idx]
			yidx <- which(as.character(years.for.df) %in% run.cols[col.idx])
			matched.amount <- rep(NA, lyears.for.df)
			matched.amount[yidx] <- as.numeric(amount)
			this.data <- data.frame(run=rep(runn, lyears.for.df), Time=years.for.df, 
							amount=matched.amount)
			datafrs <- if(irun == 1) this.data else datafrs <- rbind(datafrs, this.data)
			this.tabdata <- this.data[,'amount', drop=FALSE]
			#colnames(this.tabdata) <- paste('run', runn)
			colnames(this.tabdata) <- runn
			#run.table.columns <- c(run.table.columns, paste('run', runn))
			run.table.columns <- c(run.table.columns, runn)
			last.table.columns <- c()
			tabDF <- cbind(tabDF, this.tabdata)
		}
		if(not_found) next
		
		CI.df <- NULL
		if(!is.null(CIs[[what]])) 
		    CI.df <- subset(CIs[[what]], id == geo)
		
		idxt <- which(ids.tr[[what]]==geo)
		tabDF$Actual <- rep(NA, nrow(tabDF))
		if(length(trend.data[[what]]) > 0) {
		    obs.df <- data.frame(run=rep('Actual', ncol(trend.data[[what]])), 
						Time=as.integer(colnames(trend.data[[what]])), 
						amount=as.numeric(trend.data[[what]][idxt, ]))
		    datafrs <- rbind(datafrs, obs.df)
		    datafrs <- datafrs[!is.na(datafrs$amount),]
		    tabDF$Actual[tabDF$Time %in% as.integer(colnames(trend.data[[what]]))] <- trend.data[[what]][idxt,]
		}
		# Create plot of lines
		g[[what]] <- ggplot(subset(datafrs, (Time %in% years))) + 
		                    geom_line(aes(Time, amount, colour=factor(run))) + 
							scale_y_continuous('') + scale_x_continuous('') + 
							scale_colour_discrete(name = '') +
							ggtitle(titles[[what]]) + 
							theme(legend.position=c(0,0.5), legend.justification=c(0,0), 
									legend.key=element_blank(),
									legend.key.size = unit(0.02, "npc"),
									plot.title=element_text(size=12))
		if(length(all.years) > 0)
			g[[what]] <- g[[what]] + geom_line(data=subset(datafrs, run %in% run.numbers & !(run %in% not.all.years) & Time %in% all.years), 
			                                   aes(Time, amount, colour=factor(run)), linetype=3)
		if(!is.null(CI.df)) { # add confidence intervals
		    g[[what]] <- g[[what]] + geom_ribbon(data = CI.df, aes(x=Time, ymin=lower, ymax=upper, linetype=NA), alpha=0.1) +
		                              geom_line(data = CI.df, aes(x=Time, y = median), alpha=0.4)
		}
		# Create table
		tidx <- c(which(is.element(tabDF[,1], years.for.table)), which(tabDF[,1]==9999))
		tidx.raw <- tidx
		rown <- tabDF[tidx,1]
		last.row <- c()
		for(column in colnames(tabDF)) tabDF[tidx.raw,column] <- as.integer(tabDF[tidx.raw,column])
		lastcols <- c()
		columns <- c('Actual')
		columns <- c(columns,  unlist(strsplit(paste(paste(run.table.columns, collapse=','), sep=','), ',')), lastcols)
		format.table <- format(tabDF[tidx.raw,columns],
							justify='right', big.mark = ","
							)
		#tabDF[is.na(tabDF)] <- ''
		gtab[[what]] <- list()
		
		#tabidxs <- list(1:2, 3:(length(mid.table.columns)+2), (length(mid.table.columns)+3):length(columns))
		#for(i in 1:3){
		#	gtab[[what]][[i]] <- tableGrob(format(tabDF[tidx,columns[tabidxs[[i]]]],
			gtab[[what]] <- tableGrob(format.table,
						rows=rown
    					#gpar.colfill = gpar(fill=NA,col=NA), 
#    					gpar.rowfill = gpar(fill=NA,col=NA),
    					#show.box=TRUE, 
#    					separator = "grey",
#    					h.even.alpha = 0,
#    					show.csep = TRUE, 
#    					gpar.rowtext = gpar(col="black",  equal.width = TRUE, fontface='bold', # cex=0.8,
#                        			show.vlines = TRUE, show.hlines = TRUE, separator="grey")                     
    		)
#    		gtab[[what]]$d <- remove.na(gtab[[what]]$d)
    	#}
    	tabDF <- cbind(geo, tabDF)
    	colnames(tabDF)[1] <- 'area_id'
    	if(save.data.as.ascii) {
    		write(titles[[what]], file=output.file.name.txt, append=file.append)
    		write.table(as.matrix(tabDF[tidx,c(colnames(tabDF)[1], columns)]), file=output.file.name.txt, append=TRUE, row.names=FALSE, sep='\t')
    		write('\n', file=output.file.name.txt, append=TRUE)
    		file.append <- TRUE
    	}
	}
	sub <- ''
	main.col <- 'black'
	# Assemble page		
	grid.arrange(gtab[[indicators[1]]], g[[indicators[1]]],
				 gtab[[indicators[2]]], g[[indicators[2]]],
				 gtab[[indicators[3]]], g[[indicators[3]]],
				ncol=2, 
				top=textGrob(paste(geo, '-', area.names[area.names[,'id']==geo,'name']), 
						gp=gpar(fontsize=14,fontface="bold", col=main.col, fill=main.col), 
						just=c('center', 'top')),
				heights=rep(unit(0.31, "npc"),3),
				bottom=textGrob(sub, gp=gpar(fontsize=11), just=c('center', 'bottom'))
				#just='left'
				)

}
cat('\n')
dev.off()

setwd(curdir)
