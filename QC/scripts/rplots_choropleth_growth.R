# This script will produce choropleth maps in html comparing 2014 and 2040 estimates from runs that are specified in inputs.txt
# script crashes with zone geography -- pandoc out of memory error

library(leaflet)
library(rgdal)
library(sp)
library(htmlwidgets)
library(RColorBrewer)

# environment inputs
attribute <- c("population","employment","households","residential_units")
geography <- c("faz")#,"zone")
year1 <- (2014)
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
  dsn <- file.path("data")
  layer_faz <- "FAZ_2010_WGS84"
  layer_taz <- "TAZ_2010_WGS84"
  layer_centers <- "centers_WGS84"
  source('templates/create_Rmd_blocks.R')
} else {
  base.dir <- "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  run1 <- "run_75.run_2016_06_20_17_26"
  run2 <-"run_78.run_2016_06_23_09_47"
  run.name <- 'run75v78_test'
  result.dir <- file.path("C:/Users/Christy/Desktop/luv/QC/results", run.name)
  faz.lookup <- read.table("C:/Users/Christy/Desktop/luv/QC/data/faz_names.txt", header =TRUE, sep = "\t")
  zone.lookup <- read.table("C:/Users/Christy/Desktop/luv/QC/data/zones.txt", header =TRUE, sep = "\t")
  dsn <- "C:/Users/Christy/Desktop/luv/QC/data"
  layer_faz <- "FAZ_2010_WGS84"
  layer_taz <- "TAZ_2010_WGS84"
  layer_centers <- "centers_WGS84"
  source('C:/Users/Christy/Desktop/luv/QC/templates/create_Rmd_blocks.R')
}

centers <- readOGR(dsn=dsn, layer=layer_centers)
if(!dir.exists(result.dir)) dir.create(result.dir)

# put a header into the index file
index.file <- file.path(result.dir, 'rplots_chorogeo_growth.Rmd')
if(file.exists(index.file)) unlink(index.file)
create.section(index.file, title=paste("Choropleth maps for 2014-40 Growth"))

runnames <- c(run1, run2)
for (r in 1:length(runnames)){
  runname <- unlist(strsplit(runnames[r],"[.]"))[[1]]  
  
  # create tables, maps
  for (a in 1:length(geography)){
    
    table <- NULL
    
    for (i in 1:length(attribute)){
      # table setup
      filename1 <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
      datatable1 <- read.csv(file.path(base.dir, runnames[r],"indicators",filename1), header = TRUE, sep = ",")
      column_id <- colnames(datatable1)[grepl("_id",names(datatable1))]
      column_est <-colnames(datatable1)[grepl(paste0(year1,"|",year2),names(datatable1))]
      table <- datatable1[,c(column_id,column_est)]
      colnames(table)[2:3] <- c("yr1","yr2")
      table <- cbind(table, diff=table$yr2-table$yr1)
      
      value.type <- NULL
      
      # check for positive and negative values
      pos <- table[table$diff >=0,]
      if ((nrow(pos) != 0) & (nrow(pos) < nrow(table))){ 
        value.type <- c("positive", "negative")
      } else if (nrow(pos) == nrow(table)){
        value.type <- c("positive")
      } else {
        value.type <- c("negative")
      }
      
      print(value.type)
      
      for (v in 1:length(value.type)){
        table1 <- switch(value.type[v], "positive"=subset(table,table$diff>=0), "negative"=subset(table,table$diff<0))
        
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
      if (exists("shp.positive") & exists("shp.negative")){
        shp.positive <- shp.positive[!is.na(shp.positive@data$diff),]
        shp.negative <- shp.negative[!is.na(shp.negative@data$diff),]
        
        # map 
        print (paste0("Mapping ", runname, " ", attribute[i], " by ", geography[a]))
        
        geo.popup1 <- paste0("<strong>ID: </strong>", shp.positive$name_id,
                             "<br><strong>Name: </strong>", shp.positive$Name,
                             "<br><strong>", year2," estimate: </strong>", shp.positive$yr2,
                             "<br><strong>", year1," estimate: </strong>", shp.positive$yr1,
                             "<br><strong>Difference: </strong>", shp.positive$diff
                            )
        geo.popup2 <- paste0("<strong>ID: </strong>", shp.negative$name_id,
                             "<br><strong>Name: </strong>", shp.negative$Name,
                             "<br><strong>", year2," estimate: </strong>", shp.negative$yr2,
                             "<br><strong>", year1," estimate: </strong>", shp.negative$yr1,
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
                    title = paste0(runname, "<br>2014-40 growth in ", attribute[i], " by ", geography[a]),
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
        print (paste0("Mapping ", runname, " ", attribute[i], " by ", geography[a]))
        
        geo.popup1 <- paste0("<strong>ID: </strong>", shp.positive$name_id,
                             "<br><strong>Name: </strong>", shp.positive$Name,
                             "<br><strong>", year2," estimate: </strong>", shp.positive$yr2,
                             "<br><strong>", year1," estimate: </strong>", shp.positive$yr1,
                             "<br><strong>Difference: </strong>", shp.positive$diff
                            )
        
        geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id) 
        
        colors <- brewer.pal(n=7, name="Reds")
        pal <- colorBin(colors, bins = 7, domain=shp.positive$diff, pretty = FALSE)
        
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
                    title = paste0(runname, "<br>2014-40 growth in ", attribute[i], " by ", geography[a]),
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
        print (paste0("Mapping ", runname, " ", attribute[i], " by ", geography[a]))
        
        geo.popup2 <- paste0("<strong>ID: </strong>", shp.negative$name_id,
                             "<br><strong>Name: </strong>", shp.negative$Name,
                             "<br><strong>", year2," estimate: </strong>", shp.negative$yr2,
                             "<br><strong>", year1," estimate: </strong>", shp.negative$yr1,
                             "<br><strong>Difference: </strong>", shp.negative$diff
                            )
        geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id) 
        
        colors2 <- rev(brewer.pal(n=7, name="Blues"))
        pal2 <- colorBin(colors2, bins = 7, domain=shp.negative$diff, pretty = FALSE)
        
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
                    title = paste0(runname, "<br>2014-40 growth in ", attribute[i], " by ", geography[a]),
                    opacity =1,
                    labFormat = labelFormat(digits = 0, big.mark = ","))%>%
          addLayersControl(baseGroups = c("Street Map", "Imagery"),
                           overlayGroups = c("Data", "Centers"),
                           options = layersControlOptions(collapsed = FALSE)
                          )
        print(m)
      }
      
      # create html files
      subtitle <- paste0(attribute[i], " by ", as.name(geography[a]), ", ", as.name(runname))
      html.file <- file.path(result.dir, paste0('rplots', "_",as.name(attribute[i]),"_by_", as.name(geography[a]),"_", as.name(runname), "_growth_choroplethmap.html"))
      saveWidget(m, file=html.file)
      
      # add text into the index file
      add.text(index.file, paste0("* [", subtitle, "](", paste0('file://', html.file), ")"))
      
      # clear shapes
      if(exists("shp.positive")) remove(shp.positive)
      if(exists("shp.negative")) remove(shp.negative)   
    } # end of attribute loop
    
  } # end of geography loop
  
  print (paste0("Mapping ", runname," Complete!"))
  
} # end of runnames loop

