# This script will produce choropleth maps in html comparing 2040 estimates from runs that are specified in inputs.txt
# script crashes with zone geography -- pandoc out of memory error

library(leaflet)
library(rgdal)
library(sp)
library(htmlwidgets)
library(RColorBrewer)

# environment inputs
attribute <- c("population", "households","employment", "residential_units")
geography <- c("faz")#,"zone",)
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
  dsn <- file.path("data")
  layer_faz <- "FAZ_2010_WGS84"
  layer_taz <- "TAZ_2010_WGS84"
  layer_centers <- "centers_WGS84"
  source('templates/create_Rmd_blocks.R')
} else {
  base.dir <- "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  run1 <- "run_75.run_2016_06_20_17_26"
  run2 <- "run_78.run_2016_06_23_09_47" 
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

runname1 <- unlist(strsplit(run1,"[.]"))[[1]]
runname2 <- unlist(strsplit(run2,"[.]"))[[1]]
if(!dir.exists(result.dir)) dir.create(result.dir)

# put a header into the index file
index.file <- file.path(result.dir, 'rplots_choroplethmap.Rmd')
if(file.exists(index.file)) unlink(index.file)
create.section(index.file, title=paste("Choropleth maps for ", runname1, "and", runname2))

# create tables, maps
for (a in 1:length(geography)){
  
  for (i in 1:length(attribute)){
    # run1
    filename1 <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
    datatable1 <- read.csv(file.path(base.dir, run1,"indicators",filename1), header = TRUE, sep = ",")
    column_id <- colnames(datatable1)[grepl("_id",names(datatable1))]
    column_est <-colnames(datatable1)[grepl(year1,names(datatable1))]
    table1 <- datatable1[,c(column_id,column_est)]
    colnames(table1)[2] <- paste0("estrun1")
    
    # run2
    filename2 <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
    datatable2 <- read.csv(file.path(base.dir, run2,"indicators",filename2), header = TRUE, sep = ",")
    column_id2 <- colnames(datatable2)[grepl("_id",names(datatable2))]
    column_est2 <-colnames(datatable2)[grepl(year2,names(datatable2))]
    table2 <- datatable2[,c(column_id2,column_est2)]
    colnames(table2)[2] <- paste0("estrun2")
    
    # merge tables
    merge.table <- merge(table1, table2, by = colnames(datatable2)[grepl("_id",names(datatable2))])
    merge.table <- cbind(merge.table, diff=merge.table$estrun1-merge.table$estrun2)#, indicator=switch(attribute[i],"population"="Total Population", "households"="Households", "employment"="Employment", "residential_units"="Residential Units"))
    
    # merge with lookup tables
    if (geography[a]=="zone"){
      taz <- readOGR(dsn=dsn,layer=layer_taz)
      drops <- c("area_type_id", "district_id")
      combine.lookup <- merge(zone.lookup, faz.lookup, by = "faz_id")
      combine.lookup <- combine.lookup[,!(names(combine.lookup) %in% drops)]
      map.table <- merge(merge.table, combine.lookup, by = "zone_id")
      map.table <- map.table[,!(names(map.table) == "faz_id")]
      colnames(map.table)[1] <- paste0("id")
      shp.merge <- merge(taz, map.table, by.x=c("TAZ"), by.y=c("id"))
      shp.merge$name_id <- shp.merge$TAZ
    } else {
      faz <- readOGR(dsn=dsn,layer=layer_faz)
      map.table <- merge(merge.table, faz.lookup, by = "faz_id")
      colnames(map.table)[1] <- paste0("id")
      shp.merge <- merge(faz, map.table, by.x=c("FAZ10"), by.y=c("id"))
      shp.merge$name_id <- shp.merge$FAZ10
    }

    # map 
    print (paste0("Mapping ", attribute[i], " by ", geography[a]))
    
    pal <- colorBin(palette = "PuOr", bins = 9, domain=shp.merge$diff, pretty = FALSE)
      
    geo.popup1 <- paste0("<strong>ID: </strong>", shp.merge$name_id, 
                        "<br><strong>Name: </strong>", shp.merge$Name,
                        "<br><strong>Difference: </strong>", shp.merge$diff
                        )
    geo.popup2 <- paste0("<strong>Center: </strong>", centers$name_id) 
      
    m <- leaflet(data=shp.merge)%>% 
      addProviderTiles("CartoDB.Positron", group = "Basic Street Map")%>%
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
                  popup = geo.popup2)%>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~diff,
                title = paste0(runname1, " and ", runname2, "<br>Difference in 2040 ", attribute[i], " by ", geography[a]),
                opacity =1,
                labFormat = labelFormat(digits = 0, big.mark = ","))%>%
      addLayersControl(baseGroups = c("Basic Street Map", "Imagery"),
                       overlayGroups = c("Data", "Centers"),
                       options = layersControlOptions(collapsed = FALSE)
                      )
      
    print(m)

    # create html files
    subtitle <- paste0(attribute[i], " by ", as.name(geography[a]))
    html.file <- file.path(result.dir, paste0('rplots', "_",as.name(attribute[i]),"_by_", as.name(geography[a]), "_choroplethmap.html"))
    saveWidget(m, file=html.file)
 
    # add text into the index file
    add.text(index.file, paste0("* [", subtitle, "](", paste0('file://', html.file), ")"))
    
    
  } # end of attribute loop
  
  print (paste0("Mapping ", geography[a],"s Complete!"))
  
} # end of geography loop



