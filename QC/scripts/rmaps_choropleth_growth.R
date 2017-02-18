# This script will produce choropleth maps by FAZ in html comparing 2014 and 2040 estimates from runs that are specified in inputs.txt
# script crashes with zone geography -- pandoc out of memory error

library(leaflet)
library(rgdal)
library(sp)
library(htmlwidgets)
library(RColorBrewer)


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
attribute <- c("population", "employment", "households", "residential_units")
geography <- c("faz")
year1 <- (2014)
year2 <- (2040)
extension <- ".csv"

make <- !interactive()
if(make) {
  base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
  run1 <- Sys.getenv('QC_RUN1')
  result.dir <- Sys.getenv('QC_RESULT_PATH')
  wrkdir <- getwd()
} else {
  #base.dir <- "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  base.dir <- "//MODELSRV8/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
  #base.dir <- "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  run1 <- "run_31.run_2017_01_17_13_57"
  run.name <- 'run31'
  #wrkdir <- file.path(getwd(), '..')
  wrkdir <- "C:/Users/Christy/Desktop/luv/QC"
  result.dir <- file.path(wrkdir, "results", run.name)
}
dsn <- file.path(wrkdir, "data")

faz.lookup <- read.table(file.path(dsn, "faz_names.txt"), header =TRUE, sep = "\t")
layer_faz <- "FAZ_2010_WGS84"
layer_centers <- "centers_WGS84"
source(file.path(wrkdir, 'templates', 'create_Rmd_blocks.R'))

centers <- readOGR(dsn=dsn, layer=layer_centers)
if(!dir.exists(result.dir)) dir.create(result.dir)

# put a header into the index file
index.file <- file.path(result.dir, 'rplots_chorogeo_growth.Rmd')
if(file.exists(index.file)) unlink(index.file)
create.section(index.file, title=paste("Choropleth maps for 2014-40 Growth"))

runnames <- run1
for (r in 1:length(runnames)){
  runname <- unlist(strsplit(runnames[r],"[.]"))[[1]]  
  
  # create tables, maps
  for (a in 1:length(geography)){
    table <- NULL
    for (i in 1:length(attribute)){
      # table setup
      filename <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
      datatable <- read.csv(file.path(base.dir, runnames[r],"indicators",filename), header = TRUE, sep = ",")
      column_id <- colnames(datatable)[grepl("_id",names(datatable))]
      column_est <-colnames(datatable)[grepl(paste0(year1,"|",year2),names(datatable))]
      table <- datatable[,c(column_id,column_est)]
      colnames(table)[2:3] <- c("yr1","yr2")
      table <- cbind(table, diff=table$yr2-table$yr1)
      
      # join shape to tables
      faz <- readOGR(dsn=dsn,layer=layer_faz)
      map.table <- merge(table, faz.lookup, by = "faz_id")
      colnames(map.table)[1] <- paste0("id")
      shp <- merge(faz, map.table, by.x=c("FAZ10"), by.y=c("id"))
      shp$name_id <- shp$FAZ10
      
      colorBinResult <- map.colorBins(shp$diff)
      pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=shp$diff, pretty = FALSE)
      
      geo.popup1 <- paste0("<strong>ID: </strong>", shp$name_id,
                           "<br><strong>Name: </strong>", shp$Name,
                           "<br><strong>", year2," estimate: </strong>", shp$yr2,
                           "<br><strong>", year1," estimate: </strong>", shp$yr1,
                           "<br><strong>Difference: </strong>", shp$diff)
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
                  title = paste0(runname, "<br>2014-40 growth in ", attribute[i], " by ", geography[a]),
                  opacity =1,
                  labFormat = labelFormat(digits = 0, big.mark = ","))%>%
        setView(lng = -122.008546, lat = 47.549390, zoom = 9)%>%
        addLayersControl(baseGroups = c("Street Map", "Imagery"),
                         overlayGroups = c("Centers", geo),
                         options = layersControlOptions(collapsed = FALSE))
      
      print(m)
      
      # create html files
      subtitle <- paste0(attribute[i], " by ", as.name(geography[a]), ", ", as.name(runname))
      html.file <- paste0('rplots', "_",as.name(attribute[i]),"_by_", as.name(geography[a]),"_", as.name(runname), "_growth_choroplethmap.html")
      saveWidget(m, file=file.path(result.dir, html.file))

      # add text into the index file
      add.text(index.file, paste0("* [", subtitle, "](", html.file, ")"))

    } # end of attribute loop
  } # end of geography loop
  print (paste0("Mapping ", runname," Complete!"))
} # end of runnames loop