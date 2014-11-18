library(Hmisc)
dir <- '/Users/hana/ForecastProducts/LUV/curve_fitting'
setwd(dir)
today <- '11182014'
years.of.observed.data <- c(2000, 2010, 2013)
# load anchor points
all.data <- read.table(paste0('TargetYr_Anchors',today,'.csv'), sep=',', header=TRUE, check.names=FALSE)
#all.data <- all.data[,-which(colnames(all.data)=="2040")]
x.all <- as.integer(substr(colnames(all.data)[3:ncol(all.data)], 1,4))
x.unique <- unique(x.all)
lxall <- length(x.unique)

# create lower and upper bounds
#cols <- which(as.integer(substr(colnames(all.data)[3:ncol(all.data)], 1, 4))>2013)
#proj.data <- all.data[,cols+2]
# proj.data.low <- proj.data - proj.data/100*5
# colnames(proj.data.low) <- paste0(colnames(proj.data.low), 'low')
# proj.data.up <- proj.data + proj.data/100*5
# colnames(proj.data.up) <- paste0(colnames(proj.data.up), 'high')
# all.data <- cbind(all.data, proj.data.low, proj.data.up)
# #write.table(all.data, file='PopTargetYrControlsLowHigh.csv', sep=',', row.names=FALSE)

x1 <- seq(min(years.of.observed.data), 2040) # values on the x-axis
n <- 100    # number of points beween the bounds
R <- 1000   # number of combinations, i.e. number of curves (must be <= n^2)
idx1 <- rep(1:n, n)
idx2 <- rep(1:n, each=n)
results <- matrix(NA, nrow=0, ncol=length(x1))

max.year.observed.data <- max(years.of.observed.data)

pdf(paste0('luv_optspline_', today, '.pdf'), width=10)
par(mfrow=c(5,5))
par(mar=c(2,2,1,0.4)+0.1, mgp=c(1,0.3,0))

set.seed(1) # makes it reproducible
for(iarea in 1:nrow(all.data)) { # iterate over each area
	data.cols <- (1:ncol(all.data))[-which(colnames(all.data) %in% c('County', 'Jurisdiction'))]
	this.row <- as.numeric(all.data[iarea,data.cols])
	names(this.row) <- colnames(all.data)[data.cols]
	idx <- which(!is.na(this.row))
	x <- sort(unique(x.all[!is.na(this.row)]))
	xcol <- c(x[x<=max.year.observed.data], rep(x[x>max.year.observed.data],2))
	xcoln <- as.character(xcol)
	range.cols <- which(xcol>max.year.observed.data)
	xcoln[range.cols] <- paste0(xcoln[range.cols], rep(c('low', 'high'), each=2))
	data <- data.frame(x=xcol, y=(this.row[idx])[xcoln])
	i30 <- which(x == min(x[x>max.year.observed.data]))
	i40 <- which(x == max(x[x>max.year.observed.data]))
	rownames2030 <- c(paste0(x[i30],'low'), paste0(x[i30], 'high'))
	rownames2040 <- c(paste0(x[i40],'low'), paste0(x[i40], 'high'))
	sampl2030 <- runif(n, data[rownames2030[1], 'y'], data[rownames2030[2],'y'])
	sampl2040 <- runif(n, data[rownames2040[1], 'y'], data[rownames2040[2], 'y'])
	samplidx <- sample(1:length(idx1), R)
	y <- data$y[1:(length(years.of.observed.data)+2)]
	ilast.two <- (length(y)-1):length(y)
	iobserved <- 1:length(years.of.observed.data)
	#minidx <- min2idx <- max1idx <- NA
	#minderiv <- min2deriv <- 9999999999
	#max1deriv <- 0
	deriv2 <- deriv1 <- rep(NA, R)
	for(i in 1:R) {
		y[ilast.two] <- c(sampl2030[idx1[i]], sampl2040[idx2[i]])
		splfun <- splinefun(x, y, method="natural")
		#f <- function(x) splfun(x, deriv=2)^2
		deriv2[i] <- mean(abs(splfun(x1, deriv=2))[x1>max.year.observed.data & x1<2040])
		deriv1[i] <- mean(splfun(x1, deriv=1)[x1>max.year.observed.data])
		#integ <- integrate(f, min(x1), max(x1))[[1]]
	}
	p2 <- deriv2/sum(deriv2)
	p1 <- deriv1/sum(deriv1)
	#minidx <- which.min(p2)
	max1idx <- which.max(1/p2*p1)
	y <- c(data$y[iobserved], mean(data[rownames2030,'y']), mean(data[rownames2040,'y']))
	#data.spl <- data.frame(x=x, y=c(data$y[1:2], sampl2030[idx1[minidx]], sampl2040[idx2[minidx]]))
	#fit.splmin <- spline(data.spl$x, data.spl$y, n=length(x1), method="natural")
	#data.spl2 <- data.frame(x=xs, y=c(data$y[1:3], sampl2030[idx1[min2idx]], sampl2040[idx2[min2idx]]))
	#fit.spl2min <- spline(data.spl2$x, data.spl2$y, n=length(x1), method="natural")
	data.spl1 <- data.frame(x=x, y=c(data$y[iobserved], sampl2030[idx1[max1idx]], sampl2040[idx2[max1idx]]))
	fit.spl1max <- spline(data.spl1$x, data.spl1$y, n=length(x1), method="natural")
	plot(x[iobserved], y[iobserved], main=all.data[iarea,'Jurisdiction'], 
		ylim=c(min(data$y, fit.spl1max$y), max(data$y, fit.spl1max$y)), xlab='', ylab='', xlim=c(min(x1),max(x1)))
	errbar(x[ilast.two], y[ilast.two], yplus=c(data[rownames2030[2], 'y'], data[rownames2040[2], 'y']), 
					yminus=c(data[rownames2030[1], 'y'], data[rownames2040[1], 'y']), add=TRUE, col='white')

	#lines(fit.spline, col='black')
	#lines(fit.splmin, col='green')
	lines(fit.spl1max, col='red')
	results <- rbind(results, fit.spl1max$y)
}
dev.off()
colnames(results) <- x1
# store results as an ASCII file
results <- as.data.frame(round(results))
results <- cbind(Jurisdiction=all.data[,'Jurisdiction'], results)
write.table(results, file=paste0('luv_optspline_numeric_', today, '.csv'), sep='\t', row.names=FALSE)

# This code creates the plot in the documentation 
# pdf('methexamplecurves.pdf', height=5, width=8)
# iarea <- 23
# max1idx <- 250
# data.spl1 <- data.frame(x=x, y=c(data$y[1:2], sampl2030[idx1[max1idx]], sampl2040[idx2[max1idx]]))
# fit.spl1max <- spline(data.spl1$x, data.spl1$y, n=length(x1), method="natural")
# plot(x[1:2], y[1:2], main='',ylim=c(min(data$y, fit.spl1max$y), max(data$y, fit.spl1max$y)), xlab='', ylab='', xlim=c(min(x1),max(x1)))
# errbar(x[3:4], y[3:4], yplus=c(data[rownames2030[2], 'y'], data[rownames2040[2], 'y']), 
 					# yminus=c(data[rownames2030[1], 'y'], data[rownames2040[1], 'y']), add=TRUE, col='white')
# lines(fit.spl1max, col='black')
# max1idx <- 574
# data.spl1 <- data.frame(x=x, y=c(data$y[1:2], sampl2030[idx1[max1idx]], sampl2040[idx2[max1idx]]))
# fit.spl1max <- spline(data.spl1$x, data.spl1$y, n=length(x1), method="natural")
# lines(fit.spl1max, col='red')
# dev.off()
