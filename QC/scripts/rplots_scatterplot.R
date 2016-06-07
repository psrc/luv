#This script will produce scatterplots in html comparing 2040 estimates from runs that are specified in input.txt

library(plotly)
library(htmlwidgets)

#environment inputs
attribute <- c("households","population", "employment", "residential_units")
geography <- "zone"
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
}

runname1 <- unlist(strsplit(run1,"[.]"))[[1]]
runname2 <- unlist(strsplit(run2,"[.]"))[[1]]
if(!dir.exists(result.dir)) dir.create(result.dir)

for (i in 1:length(attribute)){
  #run1
  filename1 <- paste0(geography,'__',"table",'__',attribute[i], extension)
  datatable1 <- read.csv(file.path(base.dir, run1,"indicators",filename1), header = TRUE, sep = ",")
  column_id <- colnames(datatable1)[grepl("_id",names(datatable1))]
  column_est <-colnames(datatable1)[grepl(year1,names(datatable1))]
  table1 <- datatable1[,c(column_id,column_est)]
  colnames(table1)[2] <- paste0("estrun1")
  
  #run2
  filename2 <- paste0(geography,'__',"table",'__',attribute[i], extension)
  datatable2 <- read.csv(file.path(base.dir, run2,"indicators",filename2), header = TRUE, sep = ",")
  column_id2 <- colnames(datatable2)[grepl("_id",names(datatable2))]
  column_est2 <-colnames(datatable2)[grepl(year2,names(datatable2))]
  table2 <- datatable2[,c(column_id2,column_est2)]
  colnames(table2)[2] <- paste0("estrun2")

  
  #merge tables
  merge.table <- merge(table1, table2, by = colnames(datatable2)[grepl("_id",names(datatable2))])

  #plot
  p <- plot_ly(merge.table,
               x = estrun1,
               y = estrun2,
               text = paste0("Zone ", merge.table[,1]),
               type = 'scatter',
               mode = 'markers'
               )%>%
      layout(font = list(family="Segoe UI", size = 13.5),
             title = paste0(as.name(attribute[i]), " by ", geography),
             xaxis = list(title = runname1),
             yaxis = list(title = runname2),
             margin = list(l=100, b=100, t=90, r=100)
              )
  
  #print (p)
  print (paste0("Plotting ", as.name(attribute[i])))
  htmlwidgets::saveWidget(as.widget(p),file.path(result.dir, paste0('rplots_', as.name(attribute[i]),"_scatterplot.html")))
}

print ("Plotting complete! Check results directory.")



