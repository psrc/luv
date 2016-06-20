#This script will produce scatterplots in html comparing 2040 estimates from runs that are specified in inputs.txt

library(plotly)
library(htmlwidgets)

#environment inputs
attribute <- c("population", "households","employment", "residential_units")
geography <- c("zone", "faz", "city")
year1 <- (2040)
year2 <- (2040)
extension <- ".csv"

make <- !interactive()
if(make) {
  base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
  run1 <- Sys.getenv('QC_RUN1')
  run2 <- Sys.getenv('QC_RUN2')
  result.dir <- Sys.getenv('QC_RESULT_PATH')

} else {
    base.dir <- "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
    run1 <- "run_71.run_2016_05_26_12_41"
    run2 <- "run_170.run_2015_09_15_16_02" 
    run.name <- 'run71'
    result.dir <- file.path("C:/Users/Christy/Desktop/luv/QC/results", run.name)
    #faz.lookup <- read.table("C:/Users/Christy/Desktop/luv/QC/data/faz_names.txt", header =TRUE, sep = "\t")
    #zone.lookup <- read.table("C:/Users/Christy/Desktop/luv/QC/data/zones.txt", header =TRUE, sep = "\t")
}
faz.lookup <- read.table(file.path("data", "faz_names.txt"), header =TRUE, sep = "\t")
zone.lookup <- read.table(file.path("data", "zones.txt"), header =TRUE, sep = "\t")
city.lookup <- read.table(file.path("data", "cities.csv"), header =TRUE, sep = ",")

runname1 <- unlist(strsplit(run1,"[.]"))[[1]]
runname2 <- unlist(strsplit(run2,"[.]"))[[1]]
if(!dir.exists(result.dir)) dir.create(result.dir)

# put a header into the index file
source('templates/create_Rmd_blocks.R')
index.file <- file.path(result.dir, paste0('rplots_scatter', runname2, '.Rmd'))
if(file.exists(index.file)) unlink(index.file)
create.section(index.file, title=paste("Scatterplots for ", runname1, "and", runname2))

for (a in 1:length(geography)){
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
  
    #plot
    p <- plot_ly(merge.table,
                 x = estrun1,
                 y = estrun2,
                 text = paste0("ID: ", merge.table[,1], " FAZ Name: ", merge.table[,ncol(merge.table)]),
                 name = as.character(as.name(attribute[i])),
                 type = 'scatter',
                 mode = 'markers'
                 )%>%
        add_trace(x=c(0,max(estrun1)), 
                  y=c(0,max(estrun1)),
                  name = "1:1 line", 
                  marker = list(size = 0),
                  opacity = .6,
                  mode = "lines")%>%
        layout(font = list(family="Segoe UI", size = 13.5),
               title = paste0(as.name(attribute[i]), " by ", as.name(geography[a])),
               xaxis = list(title = runname1),
               yaxis = list(title = runname2),
               margin = list(l=100, b=100, t=90, r=100)
                )
    
    #print (p)
    subtitle <- paste0(as.name(attribute[i]), " by ", as.name(geography[a]))
    print (paste0("Plotting ", subtitle))
    html.file <- file.path(result.dir, paste0('rplots_', as.name(attribute[i]), "_", as.name(geography[a]), "_scatterplot.html"))
    htmlwidgets::saveWidget(as.widget(p), html.file)
    # add text into the index file
    add.text(index.file, paste0("* [", subtitle, "](", paste0('file://', html.file), ")"))
  }
}
# convert index.Rmd into index.html
print ("Plotting complete! Check results directory.")




