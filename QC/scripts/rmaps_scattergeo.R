# This script will produce choropleth maps in html comparing 2040 estimates from runs that are specified in inputs.txt
# script crashes with zone geography -- pandoc out of memory error

library(leaflet)
library(rgdal)
library(sp)
library(htmlwidgets)
library(RColorBrewer)

options(pandoc.stack.size="1000m")

# environment inputs
attribute <- c("max_dev_nonresidential_capacity", "max_dev_residential_capacity", "max_dev_capacity")
geography <- c("faz", "zone")
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
  base.dir <- "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  base.dir <- "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  run1 <- "run_124.run_2016_09_14_20_47"
  run2.all <- c("run_105.run_2016_08_25_14_39", "run_81.run_2016_07_05_16_00", "81_plus_r97.compiled", "luv_1.compiled") 
  run.name <- 'run124'
  wrkdir <- "/Users/hana/ForecastProducts/LUV/QC"
  #wrkdir <- "C:/Users/Christy/Desktop/luv/QC"
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
    
    value.type <- NULL
    
    # check for positive and negative values
    pos <- merge.table[merge.table$diff >= 0,]
    maxdif <- max(merge.table$diff)
    if ((nrow(pos) != 0) && (nrow(pos) < nrow(merge.table)) && maxdif > 0){ 
      value.type <- c("positive", "negative")
    } else if (nrow(pos) == nrow(merge.table)){
      value.type <- c("positive")
    } else {
      value.type <- c("negative")
    }
    
    print(value.type)
    
    for (v in 1:length(value.type)){
      table1 <- switch(value.type[v], 
      				"positive"=subset(merge.table,merge.table$diff>=0), "negative"= subset(merge.table, if(maxdif > 0) merge.table$diff<0  else merge.table$diff<=0))
      
      # merge with lookup tables
      if (geography[a]=="zone"){
        taz <- readOGR(dsn=dsn,layer=layer_taz)
        drops <- c("area_type_id", "district_id")
        combine.lookup <- merge(zone.lookup, faz.lookup, by = "faz_id")
        combine.lookup <- combine.lookup[,!(names(combine.lookup) %in% drops)]
        map.table <- merge(table1, combine.lookup, by = "zone_id")
        map.table <- map.table[,!(names(map.table) == "faz_id")]
        colnames(map.table)[1] <- paste0("id")
        shp.merge <- merge(taz, map.table, by.x=c("TAZ"), by.y=c("id"))
        shp.merge$name_id <- shp.merge$TAZ
      } else {
        faz <- readOGR(dsn=dsn,layer=layer_faz)
        map.table <- merge(table1, faz.lookup, by = "faz_id")
        colnames(map.table)[1] <- paste0("id")
        shp.merge <- merge(faz, map.table, by.x=c("FAZ10"), by.y=c("id"))
        shp.merge$name_id <- shp.merge$FAZ10
      }
        
        assign(paste("shp.",value.type[v], sep = ""),shp.merge)
      
    } # end of value.type loop
    
    # scenarios for mapping
    if (exists("shp.positive") && exists("shp.negative")){
      shp.positive <- shp.positive[!is.na(shp.positive@data$diff),]
      shp.negative <- shp.negative[!is.na(shp.negative@data$diff),]
      
      # map 
      print (paste0("Mapping ", attribute[i], " by ", geography[a]))
      
      geo.popup1 <- paste0("<strong>ID: </strong>", shp.positive$name_id, 
                           "<br><strong>Name: </strong>", shp.positive$Name,
                           "<br><strong>", runname1," estimate: </strong>", shp.positive$estrun1,
                           "<br><strong>", runname2," estimate: </strong>", shp.positive$estrun2,
                           "<br><strong>Difference: </strong>", shp.positive$diff
                          )
      geo.popup2 <- paste0("<strong>ID: </strong>", shp.negative$name_id, 
                           "<br><strong>Name: </strong>", shp.negative$Name,
                           "<br><strong>", runname1," estimate: </strong>", shp.negative$estrun1,
                           "<br><strong>", runname2," estimate: </strong>", shp.negative$estrun2,
                           "<br><strong>Difference: </strong>", shp.negative$diff
                          )
      geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id) 
      
      colors <- brewer.pal(n=5, name="Reds")
      pal <- colorBin(colors, bins = 5, domain=shp.positive$diff, pretty = FALSE)
      
      colors2 <- rev(brewer.pal(n=5, name="Blues"))
      pal2 <- colorBin(colors2, bins = 5, domain=shp.negative$diff, pretty = FALSE)
      
      m <- leaflet(data=shp.positive)%>% 
        addProviderTiles("CartoDB.Positron", group = "Street Map")%>%
        addProviderTiles("Esri.WorldImagery", group = "Imagery")%>%
        addPolygons(fillColor = ~pal(diff), 
                    fillOpacity = 0.7,
                    stroke = TRUE,
                    color = "#8a8a95",
                    weight = 1,
                    group = "Data",
                    popup = geo.popup1)%>%
        addPolygons(data=shp.negative,
                    fillColor = ~pal2(diff), 
                    fillOpacity = 0.7,
                    stroke = TRUE,
                    color = "#8a8a95",
                    weight = 1,
                    group = "Data",
                    popup = geo.popup2)%>%
        addPolygons(data=centers,
                    stroke = TRUE,
                    color = "#a9a9b1",
                    dashArray = "5",
                    weight = 2,
                    group = "Centers",
                    popup = geo.popup3)%>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = ~diff,
                  title = "",
                  opacity =1,
                  labFormat = labelFormat(digits = 0, big.mark = ","))%>%
        addLegend("bottomright",
                  pal = pal2,
                  values = ~diff,
                  title = paste0(runname1, " and ", runname2, "<br>Difference in ", year1[i], " ", attribute[i], " by ", geography[a]),
                  opacity =1,
                  labFormat = labelFormat(digits = 0, big.mark = ","))%>%
        addLayersControl(baseGroups = c("Street Map", "Imagery"),
                         overlayGroups = c("Data", "Centers"),
                         options = layersControlOptions(collapsed = FALSE)
        )
      
      print(m)
    } else if (value.type == "positive"){
      shp.positive <- shp.positive[!is.na(shp.positive@data$diff),]
      # map 
      print (paste0("Mapping ", attribute[i], " by ", geography[a]))
      
      geo.popup1 <- paste0("<strong>ID: </strong>", shp.positive$name_id, 
                           "<br><strong>Name: </strong>", shp.positive$Name,
                           "<br><strong>", runname1," estimate: </strong>", shp.positive$estrun1,
                           "<br><strong>", runname2," estimate: </strong>", shp.positive$estrun2,
                           "<br><strong>Difference: </strong>", shp.positive$diff
      )
      
      geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id) 
      
      colors <- brewer.pal(n=5, name="Reds")
      pal <- colorBin(colors, bins = 5, domain=shp.positive$diff, pretty = FALSE)
      
      m <- leaflet(data=shp.positive)%>% 
        addProviderTiles("CartoDB.Positron", group = "Street Map")%>%
        addProviderTiles("Esri.WorldImagery", group = "Imagery")%>%
        addPolygons(fillColor = ~pal(diff), 
                    fillOpacity = 0.7,
                    stroke = TRUE,
                    color = "#8a8a95",
                    weight = 1,
                    group = "Data",
                    popup = geo.popup1)%>%
        addPolygons(data=centers,
                    stroke = TRUE,
                    color = "#a9a9b1",
                    dashArray = "5",
                    weight = 2,
                    group = "Centers",
                    popup = geo.popup3)%>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = ~diff,
                  title = paste0(runname1, " and ", runname2, "<br>Difference in 2040 ", attribute[i], " by ", geography[a]),
                  opacity =1,
                  labFormat = labelFormat(digits = 0, big.mark = ","))%>%
        addLayersControl(baseGroups = c("Street Map", "Imagery"),
                         overlayGroups = c("Data", "Centers"),
                         options = layersControlOptions(collapsed = FALSE)
        )
      
      print(m)
    } else {
      shp.negative <- shp.negative[!is.na(shp.negative@data$diff),]
      # map 
      print (paste0("Mapping ", attribute[i], " by ", geography[a]))
      
      geo.popup2 <- paste0("<strong>ID: </strong>", shp.negative$name_id, 
                           "<br><strong>Name: </strong>", shp.negative$Name,
                           "<br><strong>", runname1," estimate: </strong>", shp.negative$estrun1,
                           "<br><strong>", runname2," estimate: </strong>", shp.negative$estrun2,
                           "<br><strong>Difference: </strong>", shp.negative$diff
      )
      geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id) 
      
      colors2 <- rev(brewer.pal(n=5, name="Blues"))
      pal2 <- colorBin(colors2, bins = 5, domain=shp.negative$diff, pretty = FALSE)
      
      m <- leaflet(data=shp.negative)%>% 
        addProviderTiles("CartoDB.Positron", group = "Street Map")%>%
        addProviderTiles("Esri.WorldImagery", group = "Imagery")%>%
        addPolygons(data=shp.negative,
                    fillColor = ~pal2(diff), 
                    fillOpacity = 0.7,
                    stroke = TRUE,
                    color = "#8a8a95",
                    weight = 1,
                    group = "Data",
                    popup = geo.popup2)%>%
        addPolygons(data=centers,
                    stroke = TRUE,
                    color = "#a9a9b1",
                    dashArray = "5",
                    weight = 2,
                    group = "Centers",
                    popup = geo.popup3)%>%
        addLegend("bottomright",
                  pal = pal2,
                  values = ~diff,
                  title = paste0(runname1, " and ", runname2, "<br>Difference in 2040 ", attribute[i], " by ", geography[a]),
                  opacity =1,
                  labFormat = labelFormat(digits = 0, big.mark = ","))%>%
        addLayersControl(baseGroups = c("Street Map", "Imagery"),
                         overlayGroups = c("Data", "Centers"),
                         options = layersControlOptions(collapsed = FALSE)
        )
      
      print(m)
    }
    
    # create html files
    subtitle <- paste0(attribute[i], " by ", as.name(geography[a]))
    html.file <- paste0('rplots_', runname2, "_",as.name(attribute[i]),"_by_", as.name(geography[a]), "_choroplethmap.html")
    saveWidget(m, file=file.path(result.dir, html.file))
 
    # add text into the index file
    add.text(index.file, paste0("* [", subtitle, "](", html.file, ")"))
    
    # clear shapes
    if(exists("shp.positive")) remove(shp.positive)
    if(exists("shp.negative")) remove(shp.negative)   
  } # end of attribute loop
  
  print (paste0("Mapping ", geography[a],"s Complete!"))
  
} # end of geography loop
add.text(index.file, "\n\n")
} # end of run2 loop




