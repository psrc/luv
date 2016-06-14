library(plotly)

attribute <- c("population", "households","employment", "residential_units")
geography <- c("zone")#, "faz")
year1 <- (2040)
year2 <- (2040)
extension <- ".csv"

make <- !interactive()
if(make) {
  base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
  run1 <- Sys.getenv('QC_RUN1')
  run2 <- Sys.getenv('QC_RUN2')
  result.dir <- Sys.getenv('QC_RESULT_PATH')
  faz.lookup <- read.table(file.path("data", "faz_names.txt"), header =TRUE, sep = "\t")
  zone.lookup <- read.table(file.path("data", "zones.txt"), header =TRUE, sep = "\t")
  fazxy.lookup <- read.table(file.path("data","fazxy.txt"), header =TRUE, sep = "\t")
  zonexy.lookup <- read.table(file.path("data", "zonesxy.txt"), header =TRUE, sep = "\t")
} else {
  base.dir <- "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  run1 <- "run_71.run_2016_05_26_12_41"
  run2 <- "run_170.run_2015_09_15_16_02" 
  run.name <- 'run71_map'
  result.dir <- file.path("C:/Users/Christy/Desktop/luv/QC/results", run.name)
  faz.lookup <- read.table("C:/Users/Christy/Desktop/luv/QC/data/faz_names.txt", header =TRUE, sep = "\t")
  zone.lookup <- read.table("C:/Users/Christy/Desktop/luv/QC/data/zones.txt", header =TRUE, sep = "\t")
  fazxy.lookup <- read.table("C:/Users/Christy/Desktop/luv/QC/data/fazxy.txt", header =TRUE, sep = "\t")
  zonexy.lookup <- read.table("C:/Users/Christy/Desktop/luv/QC/data/zonesxy.txt", header =TRUE, sep = "\t")
}

runname1 <- unlist(strsplit(run1,"[.]"))[[1]]
runname2 <- unlist(strsplit(run2,"[.]"))[[1]]
if(!dir.exists(result.dir)) dir.create(result.dir)

diff.table <- NULL

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
    merge.table <- cbind(merge.table, diff=merge.table$estrun1-merge.table$estrun2, indicator=as.character(attribute[i]))
    
    #select largest differences
    merge.table <- merge.table[order(merge.table$diff),]
    top <- head(merge.table, n=15)
    bottom <- tail(merge.table, n=15)
    ifelse (is.null(diff.table),diff.table <- rbind(top, bottom), diff.table <- rbind(diff.table, top, bottom))
  } #end of attribute loop
  
  #merge with name lookups and xy
  if (geography[a]=="zone"){
    drops <- c("area_type_id", "district_id")
    combine.lookup <- merge(zone.lookup, faz.lookup, by = "faz_id")
    combine.lookup <- combine.lookup[,!(names(combine.lookup) %in% drops)]
    new.zonexy.lookup <- zonexy.lookup[,!(names(zonexy.lookup) %in% drops)]
    combine.lookup <- merge(combine.lookup, new.zonexy.lookup, by = c("zone_id","faz_id"))
    diff.table <- merge(diff.table, combine.lookup, by="zone_id")
  }

 # common map properties
 g <- list(scope = 'usa', 
           projection = list(type="mercator"),
           lonaxis = list(range = c(-123,-120)), 
           lataxis = list(range = c(47,48)),
           resolution = "50", 
           showland = T, 
           landcolor = toRGB("gray90"), 
           showcountries = F, 
           subunitcolor = toRGB("white"))
 
 # id for anchoring traces on different plots
 diff.table$id <- as.integer(factor(diff.table$indicator))
 
 #plot 
 p <- plot_ly(diff.table, 
             type = 'scattergeo', 
             lon = long, 
             lat = lat, 
             geo = paste0("geo", id),
             group = indicator,
             name = as.character(attribute[i]),
             showlegend = T,
             marker = list(size=(abs(diff))/100, opacity = 0.5)) %>%
      layout(font = list(family="Segoe UI", size = 13.5),
            title = paste0('Greatest differences in 2040 estimates between ', runname1, " and ", runname2, " by ", geography[a]),
            geo = g,
            autosize = F,
            width = 1900,
            height = 1000,
            margin = list(l=50, b=50, t=90, r=100),
            hovermode = F)
  
 q <- subplot(p, nrows = 2)
 
 print (q)

} #end of geography loop



