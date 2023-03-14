# Script for generating LUV R report on faz level
# Hana Sevcikova, PSRC
# March, 2015
#

trim <- function (x) gsub("^\\s+|\\s+$", "", x) # function for triming whitespace 
curdir <- getwd()

##### BEGIN USER SETTINGS ######
if(!interactive()) { # running using Makefile
	# Obtain inputs from the environment
	if(!file.exists('fazreportLUV.R')) # probably called via Makefile from the QC directory
		setwd('../Rreports') 
	run1 <- Sys.getenv('QC_RUN1')
	base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
	result.dir <- "." # Sys.getenv('QC_RESULT_PATH')
	other.runs <- Sys.getenv('RREPORT_RUNS')
	other.runs <- trim(unlist(strsplit(other.runs, ",")))
	annual <- as.logical(Sys.getenv('RREPORT_ANNUAL', 'FALSE'))
} else { # running interactively
	run1 <- "81_plus_r97.compiled"
	run1 <- "run_89.run_2020_11_25_10_17"
	run1 <- "run_98.run_2022_08_19_15_26"
	run1 <- "run_110.run_2023_02_16_12_09"
	base.dir <- "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
	base.dir <- "~/n$/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs/flatten"
	#base.dir <- "~/n$/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs/awsmodel04"
	#run.name <- "run98"
	#run.name <- "run89"
	result.dir <- "."
	#other.runs <- c('run_78.run_2016_06_23_09_47', 'run_170.run_2015_09_15_16_02')
	#other.runs <- c("run_81.run_2016_07_05_16_00", "luv_1.compiled")
	#other.runs <- c("run_47.run_2019_12_06_16_56", "run_64R.efined")
	other.runs <- c("run_99.run_2022_08_21_10_18", "run_62.run_2021_09_16_11_35", "run_89.run_2020_11_25_10_17")
	other.runs <- c("run_62.run_2021_09_16_11_35", "run_89.run_2020_11_25_10_17")
	annual <- FALSE
}
runs <- c(run1, other.runs)

#run.numbers <- sapply(strsplit(sapply(strsplit(runs, '[.]'), function(x) x[1]), '_'), function(x) x[2])
run.numbers <- sapply(strsplit(runs, '[.]'), function(x) x[1])
run.numbers <- c("LUV-it", "RTP", "RGS")

show.trend.data <- FALSE

# CIs are switched off for LUV R reports
ci.run <- c() # which run has confidence intervals 
			       # (only the last one is included in the table)
show.median <- FALSE
ci.run.name <- list("235" = 'MRr') # only needed if different from run number

# Directory containing the runs with indicators. 
# The indicators directory must contain files of the type 'faz__tab__xxx.tab'
sim.dir <- base.dir
#sim.prefix <- 'run_' # name of the run sub-directory without the run number, e.g. 'run_' for directory run_199

# Directory containing confidence intervals
# It should contain a sub-directory 'runxxx_quantiles' with faz-level CI files (xxx is ci.run)
ci.dir <- file.path(getwd(), 'quantiles')

show.all.years <- annual # should results from all years be shown as dotted lines
not.all.years <- c(other.runs) # if show.all.years is TRUE, put here runs that are exceptions
show.lut <- FALSE
show.comments <- FALSE

geography <- 'faz'
output.file.name <- file.path(result.dir, paste(geography, 'reportLUVit', if(show.all.years) 'annual' else '', '_', paste(run.numbers, collapse='_'), sep=''))

#years <- c(2014, seq(2015, 2050, by=5))
years <- c(2018, seq(2020, 2050, by=5))
years <- c(2014, 2018, seq(2020, 2040, by=5),2044,2050)
#years.for.table <- c(2014, 2015, seq(2020, 2050, by=10))
years.for.table <- c(2018, 2020, seq(2020, 2050, by=10))
years.for.table <- c(2014, 2018, seq(2020, 2040, by=10),2044,2050)
#all.years <- if(show.all.years) 2014:2050 else c()
all.years <- if(show.all.years) 2018:2050 else c()

save.data.as.ascii <- FALSE
add.data.from <- list("2014"= c("2014_faz_data_for_R_Report_No_Adj_or_Military.csv", "black"))
add.data.from <- list()
###### END USER SETTINGS ############


library(ggplot2)
library(grid)
library(gridExtra)

lyears <- length(years)
indicators <- c('households',  'population', 'employment')
indicators.obs <- as.list(indicators)
names(indicators.obs) <- indicators
ci.names <- list(employment='job', households='household', population='population')
titles <- list(households='Households', employment='Employment', population='HH Population')
indicators.ann <- NULL
if(show.all.years) {
	indicators.ann <- paste0(indicators, "An")
	#names(titles) <- paste0(names(titles), "An")
	#names(indicators.obs) <- indicators.ann
}

output.file.name.pdf <- paste(output.file.name,  'pdf', sep='.')
output.file.name.txt <- paste(output.file.name,  'txt', sep='.')

wrkdir <- getwd()
if(show.comments) source(file.path(wrkdir, 'data', paste0('commentsLUV',geography,'.R')), chdir=TRUE)



remove.na <- function(data){
    for(i in seq_along(data))
        if(!is.null(data[[i]]$label) && trim(data[[i]]$label)=='NA') data[[i]]$label <- ''
	#apply(data, c(1,2), function(x) if(trim(x)=='NA') '' else x)
    return(data)
}


sim <- fazids <- CIs <- saf <- trend.data <- lut.data <- fazids.tr <- fazids.lut <- fazids.saf <- list()

id.correspondence <- data.frame(read.table(file.path(wrkdir, 'data', 'fazes.txt'), sep='\t', header=TRUE))
id.correspondence <- id.correspondence[order(id.correspondence[,'faz_id']),]

faz.city <- read.table(file.path(wrkdir, 'data', "faz_city_luv.txt"), sep='\t', header=TRUE)
faz_names <- read.table(file.path(wrkdir, 'data', 'faz_names.txt'), header=TRUE, sep='\t')
cities <- read.table(file.path(wrkdir, 'data', "citiesLUV.csv"), sep=',', header=TRUE)

for (iwhat in seq_along(indicators)) {
    what <- indicators[iwhat]
	# Load observed data
    if(show.trend.data){
	    trend.file.name <- file.path(wrkdir, 'data',  paste(indicators.obs[[what]], '_observed_', geography, '.txt', sep=''))
	    if(file.exists(trend.file.name)) {
		    trend.data.raw <- data.frame(read.table(trend.file.name, sep='\t', header=TRUE))
		    fazids.tr[[what]] <- trend.data.raw[,1]
		    trend.data[[what]] <- trend.data.raw[order(fazids.tr[[what]]), 2:ncol(trend.data.raw)]
		    fazids.tr[[what]] <- sort(fazids.tr[[what]])
		    colnames(trend.data[[what]]) <- substr(colnames(trend.data[[what]]), 2,5)
		    trend.data[[what]] <- trend.data[[what]][,is.element(colnames(trend.data[[what]]), as.character(years))]
	    }
    }
	lut.file.name <- file.path(wrkdir, 'data',  paste(indicators.obs[[what]], '_LUT_', geography, '.txt', sep=''))
	if(show.lut && file.exists(lut.file.name)) {
		lut.data.raw <- data.frame(read.table(lut.file.name, sep='\t', header=TRUE))
		fazids.lut[[what]] <- lut.data.raw[,1]
		lut.data[[what]] <- lut.data.raw[order(fazids.lut[[what]]), 2:ncol(lut.data.raw)]
		fazids.lut[[what]] <- sort(fazids.lut[[what]])
		colnames(lut.data[[what]]) <- substr(colnames(lut.data[[what]]), 2,5)
		lut.data[[what]] <- lut.data[[what]][,is.element(colnames(lut.data[[what]]), as.character(years))]
	}

	sim[[what]] <- fazids[[what]] <- CIs[[what]] <- list()
	for(irun in 1:length(runs)) {
		run <- runs[irun]
		# Load indicators
		this.ind <- if(is.null(indicators.ann) || run %in% not.all.years) what else indicators.ann[iwhat]
		data <- read.table(file.path(sim.dir,  run,  
						'indicators', paste(geography, '__table__', this.ind, '.csv', sep='')), sep=',', header=TRUE)
		sim[[what]][[run]] <- data[,2:ncol(data)]
		fazids[[what]][[run]] <- data[,1]
		sim[[what]][[run]] <- sim[[what]][[run]][order(fazids[[what]][[run]]),]
		fazids[[what]][[run]] <- sort(fazids[[what]][[run]])
	}
}
area.names <-  faz_names[,c('faz_id', 'Name')]
colnames(area.names) <- c('id', 'name')
years.for.df <- sort(unique(c(years, all.years)))
lyears.for.df <- length(years.for.df)
zones <- sort(fazids[[indicators[1]]][[runs[1]]])
if(show.comments) {
	is.in.comments <- zones %in% as.integer(names(comments))
	zones <- c(zones[is.in.comments], zones[!is.in.comments])
}
add.data.points <- list()
if(!is.null(add.data.from)) {
	for(addyear in names(add.data.from)){
		add.data.points[[addyear]] <- read.table(file.path("data", add.data.from[[addyear]][1]), sep=",", header=TRUE)
	}
}

file.append <- FALSE
cat('\nProcessing ', length(zones), ' geographies:\n')
pdf(output.file.name.pdf, width=12, height=9)
for(faz in zones) {
	cat(faz, ', ')
	flush.console()
  	g <- gtab <- list()
	for (what in indicators) {
		faz_not_found <- FALSE
		tabDF <- data.frame(Time=years.for.df)
		last.table.columns <- NA	
		run.table.columns <- c()
		for(irun in 1:length(runs)) {
			run <- runs[irun]
			runn <- run.numbers[irun]
			idx <- which(fazids[[what]][[run]]==faz)
			if (length(idx) <=0) {faz_not_found <- TRUE; break}
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
		if(faz_not_found) next
		idxt <- which(fazids.tr[[what]]==faz)
		obs.df <- lut.df <- NULL
		if(!is.null(trend.data[[what]])) {
			obs.df <- data.frame(run=rep('Actual', ncol(trend.data[[what]])), 
						Time=as.integer(colnames(trend.data[[what]])), 
						amount=as.numeric(trend.data[[what]][idxt, ]))
		}
		idxl <- which(fazids.lut[[what]]==faz)
		if(!is.null(lut.data[[what]])) {
			lut.df <- data.frame(run=rep('LUT', ncol(lut.data[[what]])), 
						Time=as.integer(colnames(lut.data[[what]])), 
						amount=as.numeric(lut.data[[what]][idxl, ]))
		}
		datafrs <- rbind(datafrs, obs.df, lut.df)
		datafrs <- datafrs[!is.na(datafrs$amount),]
		if(show.trend.data){
		    tabDF$Actual <- rep(NA, nrow(tabDF))
		    tabDF$Actual[tabDF$Time %in% as.integer(colnames(trend.data[[what]]))] <- trend.data[[what]][idxt,]
		}
		if(show.lut) {
			tabDF$LUT <- rep(NA, nrow(tabDF))
			tabDF$LUT[tabDF$Time %in% as.integer(colnames(lut.data[[what]]))] <- lut.data[[what]][idxl,]
		}
		# Create plot of lines
		g[[what]] <- ggplot(subset(datafrs, (Time %in% years) | (Time==2014 & as.integer(run) < 170)), aes(Time, amount, colour=factor(run))) + geom_line() + 
							scale_y_continuous('') + scale_x_continuous('') + 
							scale_colour_discrete(name = '') +
							ggtitle(titles[[what]]) + 
							theme(legend.position=c(0,0.6), legend.justification=c(0,0), 
									legend.key=element_blank(),
									legend.key.size = unit(0.02, "npc"),
									plot.title=element_text(size=12),
									legend.background = element_rect(fill='transparent'))
		if(length(all.years) > 0)
			g[[what]] <- g[[what]] + geom_line(data=subset(datafrs, run %in% run.numbers & !(run %in% not.all.years) & Time %in% all.years), linetype=3)
		if(length(add.data.from)>0) { # additional data points, e.g. 2014 data
			for(addyear in names(add.data.from)){
				adddata <- subset(add.data.points[[addyear]], faz_id==faz)[,indicators.obs[[what]]]
				if(length(adddata) > 0) {
					adddata <- data.frame(Time=as.integer(addyear), amount=adddata)
					g[[what]] <- g[[what]] + geom_point(data=adddata, colour=add.data.from[[addyear]][2])
				}
			}
		}
		# Create table
		tidx <- c(which(is.element(tabDF[,1], years.for.table)), which(tabDF[,1]==9999))
		tidx.raw <- tidx
		rown <- tabDF[tidx,1]
		last.row <- c()
		for(column in colnames(tabDF)) tabDF[tidx.raw,column] <- as.integer(tabDF[tidx.raw,column])
		lastcols <- c() # this removes low and high from the table
		columns <- c()
		if(show.trend.data) columns <- c('Actual')
		if(show.lut) columns <- c(columns, 'LUT')
		columns <- c(columns,  unlist(strsplit(paste(paste(run.table.columns, collapse=','), sep=','), ',')), lastcols)
		format.table <- format(tabDF[tidx.raw,columns],
							justify='right', big.mark = ","
							)
		#tabDF[is.na(tabDF)] <- ''
		gtab[[what]] <- list()
		#mytheme <- gridExtra::ttheme_default(
    	   #	core = list(fg_params=list(#cex = 2.0)),
    		#					fill=NA, col=NA),
    		#colhead = list(fg_params=list(cex = 1.0)),
    		#rowhead = list(fg_params=list(cex = 1.0)))

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
    		gtab[[what]]$grobs <- remove.na(gtab[[what]]$grobs)
    	#}
    	tabDF <- cbind(faz, tabDF)
    	colnames(tabDF)[1] <- 'area_id'
    	if(save.data.as.ascii) {
    		write(titles[[what]], file=output.file.name.txt, append=file.append)
    		write.table(as.matrix(tabDF[tidx,c(colnames(tabDF)[1], columns)]), file=output.file.name.txt, append=TRUE, row.names=FALSE, sep='\t')
    		write('\n', file=output.file.name.txt, append=TRUE)
    		file.append <- TRUE
    	}
	}
	fc <- sort(subset(faz.city, faz_id == faz)$city_id)
	citidx <- sapply(fc, function(x) which(cities$city_id == x))
	sub <- paste(fc, cities[citidx,'city_name'], sep=": ", collapse=', ')
	if(show.comments && is.element(faz, as.integer(names(comments)))) {
			   sub <- paste(sub, '\n', comments[as.character(faz)])
			   main.col <- 'red'
			} else main.col <- 'black'
	# Assemble page		
	grid.arrange(gtab[[indicators[1]]], g[[indicators[1]]],
				 gtab[[indicators[2]]], g[[indicators[2]]],
				 gtab[[indicators[3]]], g[[indicators[3]]],
				ncol=2, 
				top=textGrob(paste(faz, '-', area.names[area.names[,'id']==faz,'name']), 
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
