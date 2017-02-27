# This script will produce choropleth maps in html comparing 2040 estimates from runs that are specified in inputs.txt
# script crashes with zone geography -- pandoc out of memory error

library(leaflet)
library(rgdal)
library(sp)
library(htmlwidgets)
library(RColorBrewer)

options(pandoc.stack.size="1000m")


## function---------------------------------
# Sets Leaflet color scheme and numeric bins.
map.colorBins <- function(diffcolumn){
  rng <- range(diffcolumn) 
  if (rng[1] < 0 & rng[2] > 0){
    diff.range <- "both"
    bins.from.positive <- abs(rng[2]) > abs(rng[1])
  } else if (rng[1] >=0 & rng[2] > 0){
    diff.range <- "pos"
  } else if (rng[1] < 0 & rng[2] < 0){
    diff.range <- "neg"
  } else {
    diff.range <- "none"
  }
  
  max.bin <- max(abs(rng))
  round.to <- 10^floor(log10(max.bin)) 
  # round maximum to the nearest 100 or 1000 or whatever is appropriate (determined by the log10)
  max.bin <- ceiling(max.bin/round.to)*round.to 
  absbreaks <- (sqrt(max.bin)*c(0.1, 0.2,0.4, 0.6, 0.8, 1))^2 # breaks on sqrt scale
  
  if (diff.range == "both"){
    color <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#ffffff", "#f7f7f7", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
    bin <- c(-rev(absbreaks), absbreaks)
  } else if (diff.range == "pos"){
    color <- "Reds"
    bin <- c(0, absbreaks)
  } else if (diff.range == "neg"){
    color <- "Blues"
    bin <- c(-rev(absbreaks), 0)
  } else if (diff.range == "none"){
    color <- "transparent"
    bin <- c(0, 1)
  }
  return(list(color=color, bin=bin))
}	


## environment inputs-------------------------------
attribute <- c("max_dev_nonresidential_capacity", "max_dev_residential_capacity", "max_dev_capacity")
geography <- c("faz")
year1 <- rep(2015, 3)
year2 <- year1
extension <- ".csv"

trim <- function (x) gsub("^\\s+|\\s+$", "", x) # function for triming whitespace 

make <- !interactive()
if(make) {
  base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
  run1 <- Sys.getenv('QC_RUN1')
  run2.all <- Sys.getenv('QC_RUN2')
  run2.all <- trim(unlist(strsplit(run2.all, ","))) # run2 can have multiple directories; split by comma
  result.dir <- Sys.getenv('QC_RESULT_PATH')
  dsn <- file.path("data")
  source('templates/create_Rmd_blocks.R')
} else {
  #base.dir <- "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  base.dir <- "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
  #base.dir <- "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  run1 <- "run_31.run_2017_01_17_13_57"
  run2.all <- c("luv_1.compiled")
  run.name <- 'run31'
  #wrkdir <- "/Users/hana/ForecastProducts/LUV/QC"
  wrkdir <- "C:/Users/Christy/Desktop/luv/QC"
  result.dir <- file.path(wrkdir, "results", run.name)
  dsn <- file.path(wrkdir, "data")
  source(file.path(wrkdir, 'templates/create_Rmd_blocks.R'))
}

faz.lookup <- read.table(file.path(dsn, "faz_names.txt"), header =TRUE, sep = "\t")
zone.lookup <- read.table(file.path(dsn, "zones.txt"), header =TRUE, sep = "\t")
layer_faz <- "FAZ_2010_WGS84"
layer_taz <- "TAZ_2010_WGS84"
layer_centers <- "centers_WGS84"
centers <- readOGR(dsn=dsn, layer=layer_centers)

runname1 <- unlist(strsplit(run1,"[.]"))[[1]]
runnames2 <- sapply(strsplit(run2.all,"[.]"), function(x) x[1]) # can have multiple values
if(!dir.exists(result.dir)) dir.create(result.dir)

# put a header into the index file
index.file <- file.path(result.dir, 'rplots_choroplethmap.Rmd')
if(file.exists(index.file)) unlink(index.file)
create.section(index.file, title="Choropleth maps")

for (irun in 1:length(run2.all)) {
	run2 <- run2.all[irun]
	runname2 <- runnames2[irun]
	add.text(index.file, paste("####", runname1, "vs.", runname2, "\n"))

# create tables, maps
for (a in 1:length(geography)){
  
  for (i in 1:length(attribute)){
    # run1
    filename1 <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
    full.filename1 <- file.path(base.dir, run1,"indicators",filename1)
    if(!file.exists(full.filename1)) next
    datatable1 <- read.csv(full.filename1, header = TRUE, sep = ",")
    column_id <- colnames(datatable1)[grepl("_id",names(datatable1))]
    column_est <-colnames(datatable1)[grepl(year1[i],names(datatable1))]
    table1 <- datatable1[,c(column_id,column_est)]
    colnames(table1)[2] <- paste0("estrun1")
    
    # run2
    filename2 <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
    full.filename2 <- file.path(base.dir, run2,"indicators",filename2)
    if(!file.exists(full.filename2)) next
    datatable2 <- read.csv(full.filename2, header = TRUE, sep = ",")
    column_id2 <- colnames(datatable2)[grepl("_id",names(datatable2))]
    column_est2 <-colnames(datatable2)[grepl(year2[i],names(datatable2))]
    table2 <- datatable2[,c(column_id2,column_est2)]
    colnames(table2)[2] <- paste0("estrun2")
    
    # merge tables
    merge.table <- merge(table1, table2, by = colnames(datatable2)[grepl("_id",names(datatable2))])
    dif <- merge.table$estrun1-merge.table$estrun2
    if(all(dif==0)) next # do not show maps where there is no difference between run1 and run2, because the mapping fails
    merge.table <- cbind(merge.table, diff=dif)
    
    # join shape to tables
    faz <- readOGR(dsn=dsn,layer=layer_faz)
    map.table <- merge(merge.table, faz.lookup, by = "faz_id")
    colnames(map.table)[1] <- paste0("id")
    shp <- merge(faz, map.table, by.x=c("FAZ10"), by.y=c("id"))
    shp$name_id <- shp$FAZ10
    
    colorBinResult <- map.colorBins(shp$diff)
    pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=shp$diff, pretty = FALSE)
    
    geo.popup1 <- paste0("<strong>ID: </strong>", shp$name_id,
                         "<br><strong>Name: </strong>", shp$Name,
                         "<br><strong>", runname1," estimate: </strong>", prettyNum(round(shp$estrun1, 0), big.mark = ","),
                         "<br><strong>", runname2," estimate: </strong>", prettyNum(round(shp$estrun2, 0), big.mark = ","),
                         "<br><strong>Difference: </strong>", prettyNum(round(shp$diff, 0), big.mark = ",")
    )

    geo.popup2 <- paste0("<strong>Center: </strong>", centers$name_id)
    geo <- toupper(geography[a])
    
    #map
    m <- leaflet(data=shp)%>% 
      addProviderTiles("CartoDB.Positron", group = "Street Map")%>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery")%>%
      addPolygons(fillColor = ~pal(shp$diff),
                  fillOpacity = 0.7,
                  stroke = TRUE,
                  color = "#8a8a95",
                  weight = 1,
                  group = geo,
                  popup = geo.popup1
      )%>%
      addPolygons(data=centers,
                  stroke = TRUE,
                  color = "#a9a9b1",
                  dashArray = "5",
                  weight = 2,
                  group = "Centers",
                  popup = geo.popup2
      )%>%
      addLegend("bottomright",
                pal = pal,
                values = pal(shp$diff),
                title = paste0(runname1, " and ", runname2, "<br>Difference in ", year1[i], " ", attribute[i], " by ", geography[a]),
                opacity =1,
                labFormat = labelFormat(digits = 0, big.mark = ","))%>%
      setView(lng = -122.008546, lat = 47.549390, zoom = 9)%>%
      addLayersControl(baseGroups = c("Street Map", "Imagery"),
                       overlayGroups = c("Centers", geo),
                       options = layersControlOptions(collapsed = FALSE))
    
    print(m)
    
    
    # create html files
    subtitle <- paste0(attribute[i], " by ", as.name(geography[a]))
    html.file <- paste0('rplots_', runname2, "_",as.name(attribute[i]),"_by_", as.name(geography[a]), "_choroplethmap.html")
    saveWidget(m, file=file.path(result.dir, html.file))

    # add text into the index file
    add.text(index.file, paste0("* [", subtitle, "](", html.file, ")"))
    
  } # end of attribute loop
  
  print (paste0("Mapping ", geography[a],"s Complete!"))
  
} # end of geography loop
add.text(index.file, "\n\n")
} # end of run2 loop




