#This script will produce scatterplots in html comparing 2040 estimates from runs that are specified in inputs.txt

library(plotly)
library(htmlwidgets)

#environment inputs
attribute <- c("population", "households","employment", "residential_units")
geography <- c("zone", "faz", "city")
year1 <- (2040)
year2 <- (2040)
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
  zone.lookup <- read.table(file.path("data", "zones.txt"), header =TRUE, sep = "\t")
  city.lookup <- read.table(file.path("data", "cities.csv"), header =TRUE, sep = ",")
} else {
  base.dir <- "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  run1 <- "run_74.run_2016_06_16_15_40"
  run2.all <- "run_73.run_2016_06_13_16_56" 
  run.name <- 'run74'
  result.dir <- file.path("C:/Users/Christy/Desktop/luv/QC/results", run.name)
  faz.lookup <- read.table("C:/Users/Christy/Desktop/luv/QC/data/faz_names.txt", header =TRUE, sep = "\t")
  zone.lookup <- read.table("C:/Users/Christy/Desktop/luv/QC/data/zones.txt", header =TRUE, sep = "\t")
  city.lookup <- read.table("C:/Users/Christy/Desktop/luv/QC/data/cities.csv", header =TRUE, sep = ",")
}

runname1 <- unlist(strsplit(run1,"[.]"))[[1]]
runnames2 <- sapply(strsplit(run2.all,"[.]"), function(x) x[1]) # can have multiple values
if(!dir.exists(result.dir)) dir.create(result.dir)

# put a header into the index file
source('templates/create_Rmd_blocks.R')
index.file <- file.path(result.dir, 'rplots_scatter.Rmd')
if(file.exists(index.file)) unlink(index.file)
create.section(index.file, title="Scatterplots")

for (irun in 1:length(run2.all)) {
	run2 <- run2.all[irun]
	runname2 <- runnames2[irun]
	add.text(index.file, paste("####", runname1, "vs.", runname2, "\n"))

# build plotly table for subplot
for (a in 1:length(geography)){
  
  indicators.table <- NULL
  
  for (i in 1:length(attribute)){
    #run1
    filename1 <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
    datatable1 <- read.csv(file.path(base.dir, run1,"indicators",filename1), header = TRUE, sep = ",")
    column_id <- colnames(datatable1)[grepl("_id",names(datatable1))]
    column_est <-colnames(datatable1)[grepl(year1,names(datatable1))]
    table1 <- datatable1[,c(column_id,column_est)]
    colnames(table1)[2] <- paste0("estrun1")
    
    #run2
    filename2 <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
    datatable2 <- read.csv(file.path(base.dir, run2,"indicators",filename2), header = TRUE, sep = ",")
    column_id2 <- colnames(datatable2)[grepl("_id",names(datatable2))]
    column_est2 <-colnames(datatable2)[grepl(year2,names(datatable2))]
    table2 <- datatable2[,c(column_id2,column_est2)]
    colnames(table2)[2] <- paste0("estrun2")
    
    #merge tables
    merge.table <- merge(table1, table2, by = colnames(datatable2)[grepl("_id",names(datatable2))])
    if (geography[a]=="zone"){
      merge.table <- merge(merge.table, zone.lookup, "zone_id")
      merge.table <- merge(merge.table, faz.lookup, "faz_id")
      merge.table <- merge.table[,c(2, 1, 3:(ncol(merge.table)))] 
    }else{
      if (geography[a]=="faz")
        merge.table <- merge(merge.table, faz.lookup, "faz_id")
      else merge.table <- merge(merge.table, city.lookup, "city_id")
    }  
    
    merge.table$indicator <- switch(attribute[i],"population"="Total Population", "households"="Households", "employment"="Employment", "residential_units"="Residential Units")
    ifelse (is.null(indicators.table),indicators.table <- merge.table, indicators.table <- rbind(indicators.table,merge.table))
  
  } # end of attribute loop
  
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
  
  p <- layout(subplot(p, nrows=2),
              xaxis = list(title = runname1, domain = c(0, .45), showgrid=TRUE),
              yaxis = list(title = runname2, domain = c(.55, 1), tickfont=list(family="Segoe UI", size = 13)),
              
              xaxis2 = list(title = runname1, domain = c(.55, 1), showgrid=TRUE),
              yaxis2 = list(title = runname2, domain = c(.55, 1), tickfont=list(family="Segoe UI", size = 13)),
              
              xaxis3 = list(title = runname1, domain = c(0, .45), showgrid=TRUE),
              yaxis3 = list(title = runname2, domain = c(0, .45), tickfont=list(family="Segoe UI", size = 13)),
              
              xaxis4 = list(title = runname1, domain = c(.55, 1), showgrid=TRUE),
              yaxis4 = list(title = runname2, domain = c(0, .45), tickfont=list(family="Segoe UI", size = 13)),
              
              font = list(family="Segoe UI", size = 13.5),
              title = paste0(runname1," and ", runname2, " 2040 estimates by ", as.name(geography[a])),
              margin = list(l=100, b=100, t=90, r=100)
              )
  
  print (p)
  subtitle <- paste0("All indicators by ", as.name(geography[a]))
  print (paste0("Plotting ", subtitle))
  html.file <- file.path(result.dir, paste0("rplots_", as.name(geography[a]), "_scatterplot.html"))
  htmlwidgets::saveWidget(as.widget(p), html.file)
  # add text into the index file
  add.text(index.file, paste0("* [", subtitle, "](", paste0('file://', html.file), ")"))
  
} # end of geography loop
add.text(index.file, "\n\n")
} # end of run2 loop

print ("Plotting complete! Check results directory.")

