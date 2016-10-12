#This script will produce scatterplots in html comparing 2040 estimates from runs that are specified in inputs.txt

library(plotly)
library(htmlwidgets)
library(data.table)

#environment inputs
attribute.list <- list(dev_capacity="Total capacity (in thousand sqft)", dev_nonresidential_capacity="Non-residential capacity (in thousand sqft)",
					dev_residential_capacity="Residential capacity (in DU)")
attribute <- names(attribute.list)
geography <- c("city", "faz")
year1 <- (2015)
year2 <- (2015)
extension <- ".csv"

trim <- function (x) gsub("^\\s+|\\s+$", "", x) # function for triming whitespace 

make <- !interactive()
if(make) {
  base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
  run1 <- Sys.getenv('QC_RUN1')
  run2.all <- Sys.getenv('QC_RUN2')
  run2.all <- trim(unlist(strsplit(run2.all, ","))) # run2 can have multiple directories; split by comma
  result.dir <- Sys.getenv('QC_RESULT_PATH')
  faz.lookup <- read.table(file.path("data", "faz_names.txt"), header =TRUE, sep = "\t")
  city.lookup <- read.table(file.path("data", 'cities.csv'), header=TRUE, sep=',')
  source('templates/create_Rmd_blocks.R')
} else {
  base.dir <- "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  run1 <- "81_plus_r97.compiled"
  run2.all <- c("luv_1.compiled")
  run.name <- 'run81_refined'
  wrkdir <- '/Users/hana/ForecastProducts/LUV/QC'
  result.dir <- file.path(wrkdir, "results", run.name)
  faz.lookup <- read.table(file.path(wrkdir, "data/faz_names.txt"), header =TRUE, sep = "\t")
  city.lookup <- read.table(file.path(wrkdir, "data/cities.csv"), header =TRUE, sep = ",")
  source(file.path(wrkdir, 'templates/create_Rmd_blocks.R'))
}

runname1 <- unlist(strsplit(run1,"[.]"))[[1]]
runnames2 <- sapply(strsplit(run2.all,"[.]"), function(x) x[1]) # can have multiple values
if(!dir.exists(result.dir)) dir.create(result.dir)

# put a header into the index file
index.file <- file.path(result.dir, 'rplots_scatter_max_dev.Rmd')
if(file.exists(index.file)) unlink(index.file)
create.section(index.file, title="Maximum developable capacity")

for (irun in 1:length(run2.all)) {
	run2 <- run2.all[irun]
	runname2 <- runnames2[irun]
	#add.text(index.file, paste("####", runname1, "vs.", runname2, "\n"))

# build plotly table for subplot
for (a in 1:length(geography)){
  
  indicators.table <- NULL
  out.table1 <- out.table2 <- c()
  for (i in 1:length(attribute)){
  	for(all.pcl in c(FALSE)) {
    	#run1
    	filename1 <- paste0(geography[a],'__',"table",'__max_', if(all.pcl) 'all_' else '', attribute[i], extension)
    	full.filename1 <- file.path(base.dir, run1,"indicators",filename1)
    	if(!file.exists(full.filename1)) next
    	datatable1 <- read.csv(full.filename1, header = TRUE, sep = ",")
    	column_id <- colnames(datatable1)[grepl("_id",names(datatable1))]
    	column_est <-colnames(datatable1)[grepl(year1,names(datatable1))]
    	table1 <- datatable1[,c(column_id,column_est)]
    	colnames(table1)[2] <- paste0("estrun1")
    	table1$estrun1 <- round(table1$estrun1)
    	if(attribute[i] != "dev_residential_capacity") table1$estrun1 <- table1$estrun1/1000.
    	out.table1 <- if(is.null(out.table1)) table1 else merge(out.table1, table1, by=column_id)
    	colnames(out.table1)[colnames(out.table1)=="estrun1"] <- attribute.list[[attribute[i]]]
    	
    	#run2
    	filename2 <- paste0(geography[a],'__',"table",'__max_', if(all.pcl) 'all_' else '', attribute[i], extension)
    	full.filename2 <- file.path(base.dir, run2,"indicators",filename2)
    	if(!file.exists(full.filename2)) next
    	datatable2 <- read.csv(full.filename2, header = TRUE, sep = ",")
    	column_id2 <- colnames(datatable2)[grepl("_id",names(datatable2))]
    	column_est2 <-colnames(datatable2)[grepl(year2,names(datatable2))]
    	table2 <- datatable2[,c(column_id2,column_est2)]
    	colnames(table2)[2] <- paste0("estrun2")
    	table2$estrun2 <- round(table2$estrun2)
    	if(attribute[i] != "dev_residential_capacity") table2$estrun2 <- table2$estrun2/1000.
    	out.table2 <- if(is.null(out.table2)) table2 else merge(out.table2, table2, by=column_id)
    	colnames(out.table2)[colnames(out.table2)=="estrun2"] <- attribute.list[[attribute[i]]]
    	
    	#merge tables
    	merge.table <- merge(table1, table2, by = colnames(datatable2)[grepl("_id",names(datatable2))])
    	if (geography[a]=="faz"){
        		merge.table <- merge(merge.table, faz.lookup, "faz_id")
        		if(i == length(attribute)) {
        			out.table1 <- merge(out.table1, faz.lookup, "faz_id")
        			out.table2 <- merge(out.table2, faz.lookup, "faz_id")
        		}
      	} else {
      		merge.table <- merge(merge.table, city.lookup, "city_id")
      		if(i == length(attribute)) {
      			out.table1 <- merge(out.table1, city.lookup, "city_id")
      			out.table2 <- merge(out.table2, city.lookup, "city_id")
			}
		}
    	merge.table$indicator <- paste(attribute.list[[attribute[i]]], if(all.pcl) "(all parcels)" else "")    
    	indicators.table <- if(is.null(indicators.table)) merge.table else rbind(indicators.table,merge.table)
  	}
  } # end of attribute loop
  if(is.null(indicators.table)) next

  # id for anchoring traces on different plots
  indicators.table$id <- as.integer(factor(indicators.table$indicator))
  
  #plot
  p <- plot_ly(indicators.table,
               x = estrun1,
               y = estrun2,
               text = paste0("ID: ", indicators.table[,1], " Name: ", indicators.table[,grepl("name|Name",names(indicators.table))]),
               group = indicator,
               xaxis = paste0("x", id),
               type = 'scatter',
               mode = 'markers'               
               )%>%
      add_trace(x=c(0,max(estrun1)), 
                y=c(0,max(estrun1)),
                group = indicator,
                xaxis = paste0("x", id),
                marker = list(color="grey", size = 0),
                opacity = .6,
                mode = "lines",
                showlegend = F)
  
  p <- layout(subplot(p, nrows=3),
              xaxis = list(title = runname1, domain = c(0.2, .8), showgrid=TRUE),
              yaxis = list(title = runname2, domain = c(.7, 1), tickfont=list(family="Segoe UI", size = 13)),
              
              xaxis2 = list(title = runname1, domain = c(0.2, .8), showgrid=TRUE),
              yaxis2 = list(title = runname2, domain = c(.35, 0.65), tickfont=list(family="Segoe UI", size = 13)),
              
              xaxis3 = list(title = runname1, domain = c(0.2, .8), showgrid=TRUE),
              yaxis3 = list(title = runname2, domain = c(0, 0.3), tickfont=list(family="Segoe UI", size = 13)),
              

              font = list(family="Segoe UI", size = 13.5),
              title = paste0('Max developable capacity for ', runname1," and ", runname2, " by ", as.name(geography[a])),
              margin = list(l=100, b=100, t=90, r=100)
              )
  
  print (p)
  subtitle <- paste(runname1, "vs.", runname2, 'by', geography[a])
  print (paste0("Plotting ", subtitle))
  html.file <- paste0("rplots_max_dev_", runname2, "_", as.name(geography[a]), "_scatterplot.html")
  htmlwidgets::saveWidget(as.widget(p), file.path(result.dir, html.file))
  # add text into the index file
  add.text(index.file, paste0("* **", subtitle, ":**\n    + [scatterplot](", html.file, ")"))
  res.file1 <- paste0('qc_rtable_maxcap_', runname1, "_", as.name(geography[a]), '.txt')
  write.table(out.table1, file.path(result.dir, res.file1), sep='\t', row.names=FALSE)
  res.file2 <- paste0('qc_rtable_maxcap_', runname2, "_", as.name(geography[a]), '.txt')
  write.table(out.table2, file.path(result.dir, res.file2), sep='\t', row.names=FALSE)
  add.text(index.file, paste0("    + tables: [", runname1, "](", res.file1, "), [", runname2, "](", res.file2, ")"))
  
} # end of geography loop
#add.text(index.file, "\n\n")
} # end of run2 loop
 
print ("Plotting complete! Check results directory.")

