# This ShinyApp will produce choropleth maps of growth from runs that are specified in inputs.txt
library(shiny)
library(leaflet)
library(rgdal)
library(sp)
library(magrittr)
#library(RColorBrewer)

# Preapre data and shapes ==========================
# environment inputs

attribute <- c("population","households","employment","residential_units")
geography <- c("zone", "faz", "city")
years <- c(2014, 2015, 2020, 2025, 2030, 2035, 2040)
extension <- ".csv"

trim <- function (x) gsub("^\\s+|\\s+$", "", x) # function for triming whitespace 

make <- nchar(Sys.getenv('QC_NAME')) > 0 

if(make) {
  base.dir <- Sys.getenv('QC_BASE_DIRECTORY')
  run1 <- Sys.getenv('QC_RUN1')
  run2.all <- Sys.getenv('QC_RUN2') 
  run2.all <- trim(unlist(strsplit(run2.all, ","))) # run2 can have multiple directories; split by comma
  result.dir <- Sys.getenv('QC_RESULT_PATH')
  result.name <- Sys.getenv('QC_NAME') 
  wrkdir <- file.path(Sys.getenv('QC_SCRIPT_PATH'), "..") 
  dsn <- file.path(wrkdir, "data")
} else {
  base.dir <- "//modelsrv3/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  #base.dir <- "/Volumes/e$/opusgit/urbansim_data/data/psrc_parcel/runs"
  run1 <- "run_99.run_2016_08_15_17_11"
  run2.all <- c("run_81.run_2016_07_05_16_00","luv_1.compiled")
  run.name <- 'run99'
  result.dir <- file.path("C:/Users/Christy/Desktop/luv/QC/results", run.name)
  #dsn <- "/Users/hana/ForecastProducts/LUV/QC/data"
  dsn <- "C:/Users/Christy/Desktop/luv/QC/data"
}

faz.lookup <- read.table(file.path(dsn, "faz_names.txt"), header =TRUE, sep = "\t") 
zone.lookup <- read.table(file.path(dsn, "zones.txt"), header =TRUE, sep = "\t")    
city.lookup <- read.table(file.path(dsn, "cities.csv"), header =TRUE, sep = ",")

layer_faz <- "FAZ_2010_WGS84"
layer_zone <- "TAZ_2010_WGS84"
layer_city <- "JURIS_2014_WGS84"
layer_centers <- "centers_WGS84"

centers <- readOGR(dsn=dsn, layer=layer_centers)

runname1 <- unlist(strsplit(run1,"[.]"))[[1]]
runnames2 <- sapply(strsplit(run2.all,"[.]"), function(x) x[1]) # can have multiple values
runs <- c(runname1, unlist(runnames2))
runnames <- c(run1, run2.all)

for (r in 1:length(runnames)){
  runname <- unlist(strsplit(runnames[r],"[.]"))[[1]]
  
  # create tables, maps
  for (a in 1:length(geography)){
    
    indicators.table <- NULL
    
    for (i in 1:length(attribute)){
      # table setup
      filename1 <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
      datatable1 <- read.csv(file.path(base.dir, runnames[r],"indicators",filename1), header = TRUE, sep = ",")
      column_id <- colnames(datatable1)[grepl("_id",names(datatable1))]
      column_est <- NULL
      for (y in 1: length(years)){
        column_est1 <- colnames(datatable1)[grepl((years[y]),names(datatable1))]
        ifelse (is.null(column_est1), 
                column_est <- column_est1,
                column_est <- cbind(column_est, column_est1))
      }
      table <- datatable1[,c(column_id,column_est)]
      colnames(table)[2:ncol(table)] <- paste0("yr", sapply(years, function(x) x[1]))
      table$indicator <- switch(attribute[i],
                                "population"="Total Population", 
                                "households"="Households", 
                                "employment"="Employment", 
                                "residential_units"="Residential Units")
      
      ifelse (is.null(indicators.table),
              indicators.table <- table, 
              indicators.table <- rbind(indicators.table, table))
    
    } # end of attribute loop
    
    # assign tables and shapefiles to geography-based list
    if (geography[a]=="faz"){
      faz.shape <- readOGR(dsn=dsn,layer=layer_faz) %>% sp::merge(faz.lookup, by.x = "FAZ10", by.y = "faz_id")
      faz.shape$name_id <- faz.shape$FAZ10
      faz <- list(tbl = indicators.table, shp = faz.shape)
    } else if (geography[a]=="zone"){
      zone.shape <- readOGR(dsn=dsn,layer=layer_zone) %>% sp::merge(zone.lookup, by.x = "TAZ", by.y = "zone_id")
      zone.shape$name_id <- zone.shape$TAZ
      zone <- list(tbl = indicators.table, shp = zone.shape)
    } else {
      city.shape <- readOGR(dsn=dsn,layer=layer_city) %>% sp::merge(city.lookup, by = "city_id")
      city.shape$name_id <- city.shape$city_id
      city.shape$Name <- city.shape$city_name
      city <- list(tbl = indicators.table, shp = city.shape)
    }
    
  } # end of geography loop
  
  # assign geography, tables, and shapefiles to run2 lists
  assign(paste(runname, ".data", sep=""), list(zone = zone, faz = faz, city = city))

} # end of runnames loop      

# UI ===============================================

ui <- fluidPage(
  
  sidebarLayout(sidebarPanel(h4(class="header", checked=NA,
                                      tags$b("Select the following to see growth since 2014")
                                      ),
                            br(),
                            selectInput(inputId = "select_run",
                                         label = "Run",
                                         choices = runs),
                            selectInput(inputId = "select_geography",
                                        label = "Geography",
                                        choices = c("TAZ"=1,
                                                    "FAZ"=2,
                                                    "City"=3),
                                        selected = 1),
                            selectInput(inputId = "select_indicator",
                                        label = "Indicator",
                                        choices = c("Total Population"=1,
                                                    "Households"=2,
                                                    "Employment"=3,
                                                    "Residential Units"=4),
                                        selected = 1),
                            sliderInput(inputId = "select_year",
                                        label = "End Year",
                                        min = years[2],
                                        max = years[length(years)],
                                        value = years[length(years)],
                                        step = 5,
                                        sep = ""
                                        ),
                 width = 2
                 ), # end sidebarPanel
    mainPanel(leafletOutput("map", width = "100%", height = "840px"),
              width = 10
              ), # end mainPanel
    position = c("left", "right"),
    fluid = TRUE
  ) # end sidebarLayout
) # end fluidPage


  
# Server ===========================================  
  
server <- function(input, output, session) {
  
  # reactions
  run <- reactive({
    paste0(input$select_run, ".data")
  })
  
  table <- reactive({ 
    switch(as.integer(input$select_indicator),
           "Total Population",
           "Households",
           "Employment",
           "Residential Units") 
  })
  
  year <- reactive({
    paste0("yr", input$select_year)
  })
  
  year.label <- reactive({
    unlist(strsplit(year(),"r"))[[2]]
  })
  
  geog <- reactive({
    geo <- as.integer(input$select_geography)
    get(run())[[geo]][[1]]
  })
  
  shape_select <- reactive({ 
    geo <- as.integer(input$select_geography)
    get(run())[[geo]][[2]]
  })
  
  shape <- reactive({
    data1 <- subset(geog(), geog()$indicator==table())
    year.start <- data1[,2]
    year.col <- which(names(data1) == year())
    year.end <- data1[, year.col]
    data1 <- cbind(data1, diff = year.end - year.start)
    
    if (input$select_geography == 1){
      shape.join <- sp::merge(shape_select(), data1, by.x = c("TAZ"), by.y = c("zone_id")) %>% 
        merge(faz.lookup, by = "faz_id") 
      return(shape.join)
    } else if (input$select_geography == 2){
      shape.join <- sp::merge(shape_select(), data1, by.x = c("FAZ10"), by.y = c("faz_id"))
      return(shape.join)
    } else {
      shape.join <- sp::merge(shape_select(), data1, by = "city_id")
      return(shape.join)
    }
  })
  
  # leaflet layer control
  geo <- reactive({
    if (input$select_geography == 1){
      geo <- "TAZ"
      return(geo)
    } else if (input$select_geography == 2){
      geo <- "FAZ"
      return(geo)
    } else {
      geo <- "City"
      return(geo)
    }
  })
   
  output$map <- renderLeaflet({
    
    runname2.trim <- sapply(strsplit(run(),"[.]"), function(x) x[1])
    
    data <- shape()
    
    # Set up symbology and categorization
    rng <- range(shape()$diff)
    if (rng[1] < 0 & rng[2] > 0){
      diff.range <- "both"
    } else if (rng[1] >=0 & rng[2] > 0){
      diff.range <- "pos"
    } else if (rng[1] < 0 & rng[2] < 0){
      diff.range <- "neg"
    } else {
      diff.range <- "none"
    }
    
    if (diff.range == "both"){
      color <- c("#053061",
                 "#2166ac", 
                 "#4393c3", 
                 "#92c5de", 
                 "#d1e5f0", 
                 "#ffffff", 
                 "#f7f7f7", 
                 "#fddbc7", 
                 "#f4a582", 
                 "#d6604d", 
                 "#b2182b", 
                 "#67001f")
      if (input$select_geography == 1){ # separate bins for TAZ
        bin <- c(-70000, -3000, -1000, -500,- 250, 0, 1, 250, 500, 1000, 3000, 70000)
      } else { # for other geographies
        bin <- c(-170000, -30000, -10000, -5000,- 2500, 0, 1, 2500, 5000, 10000, 30000, 170000)
      }
    } else if (diff.range == "pos"){
      color <- "Reds"
      bin <- c(0, 500, 1000, 2500, 5000, 7000, 10000, 25000, 30000, 70000)
    } else if (diff.range == "neg"){
      color <- "Blues"
      bin <- c(-70000, -30000, -10000, -25000, -5000,- 2500, -1000, -500, 0)
    } else if (diff.range == "none"){
      color <- "transparent"
      bin <- c(0, 1)
    }
    
    pal <- colorBin(palette = color, bins = bin, domain=shape()$diff, pretty = FALSE) 
    
    # popup setup
    year.start <- match(paste0("yr", years[[1]]),names(shape()))
    year.end <- match(year(), names(shape()))
    geo.popup1 <- paste0("<strong>ID: </strong>", shape()$name_id,
                         "<br><strong>", geo(), " Name: </strong>", shape()$Name,
                         "<br><strong>", years[[1]]," estimate: </strong>", shape()@data[,year.start],
                         "<br><strong>", year.label()," estimate: </strong>", shape()@data[,year.end],
                         "<br><strong>Difference: </strong>", shape()$diff
    )
    
    geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)
    
    m <- leaflet(data=shape())%>% 
      addProviderTiles("CartoDB.Positron", group = "Street Map")%>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery")%>%
      addPolygons(fillColor = ~pal(shape()$diff),
                  fillOpacity = 0.7,
                  stroke = TRUE,
                  color = "#8a8a95",
                  weight = 1,
                  group = geo(),
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
                values = pal(shape()$diff),
                title = paste0("2014-", year.label(), " growth by ", geo()),
                opacity =1,
                labFormat = labelFormat(digits = 0, big.mark = ","))%>%
      setView(lng = -122.008546, lat = 47.549390, zoom = 9)%>%
      addLayersControl(baseGroups = c("Street Map", "Imagery"),
                       overlayGroups = c(geo(), "Centers"),
                       options = layersControlOptions(collapsed = FALSE))


    }) # end renderLeaflet
  
} # end server function

shinyApp(ui = ui, server = server)


