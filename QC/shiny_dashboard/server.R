function(input, output) {
  #functions------------------------------------------------------------------------------
  
  # remove all whitespaces
  remove.spaces <- function(x) gsub(" ", "", x, fixed = TRUE) 
  
  # Creates a Plotly scatterplot. Requires reactive table, string source name and string x&y axis titles.
  scatterplot <- function(table, sourcename, xcolumn, ycolumn, xtitle, ytitle) {
    data <- table 
    key <- data$name_id # uniquely identify geo for Plotly
    p <- plot_ly(data,
                 type = 'scatter',
                 x = ~xcolumn,
                 y = ~ycolumn, 
                 name = "",
                 source = sourcename,
                 text = ~paste0("ID: ", name_id,
                               "<br>Name: ", Name), 
                 key = key, # will appear in 'eventdata'
                 mode = 'markers',
                 showlegend = F)%>%
      add_trace(x=c(0,~max(xcolumn)),
                y=c(0,~max(xcolumn)),
                color= I("grey"),
                opacity = .6,
                mode = "lines",
                showlegend = F)%>%
      layout(font = list(family="Segoe UI", size = 13.5),
             title = " ",
             xaxis = list(title = xtitle),
             yaxis = list(title = ytitle),
             margin = list(l=100, b=100, t=90, r=100)
      )
    p
  }
  
  # Joins reactive tables to respective shapefiles.
  joinShp2Tbl <- function(inputGeog, table){
    if (inputGeog == 1){
      shape.join <- sp::merge(zone.shape, table, by.x = "TAZ", by.y = "name_id")
      return(shape.join)
    } else if (inputGeog == 2){
      shape.join <- sp::merge(faz.shape, table, by.x = "FAZ10", by.y = "name_id")
      return(shape.join)
    } else if (inputGeog == 3){
      shape.join <- sp::merge(city.shape, table, by.x = "city_id", by.y = "name_id")
      return(shape.join)
    } else {
      centers.shape <- centers[centers$name_id != 0,]
      shape.join <- sp::merge(centers.shape, table, by.x = "name_id", by.y = "name_id")
      return(shape.join)
    }
  }
  
  # Sets Leaflet color scheme and numeric bins.
  map.colorBins <- function(diffcolumn, inputGeog){
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
      color <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#ffffff", "#f7f7f7", 
                 "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
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
  
  # Writes Leaflet popup text for non-centers shapefiles. Requires reactive shapefile, string x&y axis titles.
  map.shp.popup <- function(shapefile, xcolumn, ycolumn, layerctrl, xtitle, ytitle){
    paste0("<strong>ID: </strong>", shapefile$name_id,
           "<br><strong>", layerctrl, " Name: </strong>", shapefile$Name,
           "<br><strong>", xtitle," estimate: </strong>", prettyNum(round(shapefile@data[,xcolumn], 0), big.mark = ","),
           "<br><strong>", ytitle," estimate: </strong>", prettyNum(round(shapefile@data[,ycolumn], 0), big.mark = ","),
           "<br><strong>Difference: </strong>", prettyNum(round(shapefile$diff, 0), big.mark = ","))		
  }		
  
  # Creates Leaflet baselayers. Requires reactive shapefile, string legend title.
  map.layers <- function(shapefile, layerctrl, legendtitle, popupgeo, popupctr, mappalette){
    map <- leaflet(data=shapefile)%>% 
      addProviderTiles("CartoDB.Positron", group = "Street Map")%>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery")%>%
      addPolygons(fillColor = ~mappalette(shapefile$diff),
                  fillOpacity = 0.7,
                  stroke = TRUE,
                  color = "#8a8a95",
                  weight = 1,
                  group = layerctrl,
                  popup = popupgeo)%>%
      addPolygons(data=centers,
                  stroke = TRUE,
                  color = "#a9a9b1",
                  dashArray = "5",
                  weight = 2,
                  group = "Centers",
                  popup = popupctr)%>%
      addLegend("bottomright",
                pal = mappalette,
                values = mappalette(shapefile$diff),
                title = legendtitle,
                opacity =1,
                labFormat = labelFormat(digits = 0, big.mark = ","))%>%
      setView(lng = -122.008546, lat = 47.549390, zoom = 9)%>%
      addLayersControl(baseGroups = c("Street Map", "Imagery"),
                       overlayGroups = c("Centers",layerctrl),
                       options = layersControlOptions(collapsed = FALSE))
    return(map)
  }	
  
  map.layers.basic <- function(shapefile, layerctrl, legendtitle, popupgeo, mappalette){
    map <- leaflet(data=shapefile)%>% 
      addProviderTiles("CartoDB.Positron", group = "Street Map")%>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery")%>%
      addPolygons(fillColor = ~mappalette(shapefile$diff),
                  fillOpacity = 0.7,
                  stroke = TRUE,
                  color = "#8a8a95",
                  weight = 1,
                  group = layerctrl,
                  popup = popupgeo)%>%
      addLegend("bottomright",
                pal = mappalette,
                values = mappalette(shapefile$diff),
                title = legendtitle,
                opacity =1,
                labFormat = labelFormat(digits = 0, big.mark = ","))%>%
      setView(lng = -122.008546, lat = 47.549390, zoom = 9)%>%
      addLayersControl(baseGroups = c("Street Map", "Imagery"),
                       overlayGroups = c(layerctrl),
                       options = layersControlOptions(collapsed = FALSE))
    return(map)
  }
  
  # Selects IDs of scatterplot points and finds match in respective shapefile. Requires string source name
  # that matches its respective scatterplot source name. Requires reactive shapefile.
  select.items <- function(sourcename, inputGeog, shapefile){
    eventdata <- event_data(event = "plotly_selected", source = sourcename)
    if(is.null(eventdata)){
      return(NULL) # do nothing
    } else {
      geoid <- eventdata[['key']]
      if (inputGeog == 1){
        sub <- shapefile[shapefile$name_id %in% geoid, ] # Give back a sp data frame of the selected ids
        return(sub)
      } else if (inputGeog == 2){
        sub <- shapefile[shapefile$name_id %in% geoid, ]
        return(sub)
      } else {
        sub <- shapefile[shapefile$name_id %in% geoid, ]
        return(sub)
      }
    }
  }	
  
  # Creates new map layer of selected geographies. Requires 2 arguments: reactive drag event (c or g selected_geo()) and
  # reactive Leaflet layer control
  addSelectedGeo <- function(map, dragevent, layerctrl){
    addPolygons(map, 
                data = dragevent,
                fill = FALSE,
                color = '#FFFF00',
                opacity = 1,
                group = paste0("Selected ", layerctrl))			   
  }
  
  # Creates new map view and layer control settings when there are selected geographies. 
  # Requires only 1 argument: reactive Leaflet layer control
  map.settings <-function(map, layerctrl){
    map <- setView(map, lng = -122.008546, lat = 47.549390, zoom = 9)%>%
      addLayersControl(baseGroups = c("Street Map", "Imagery"),
                       overlayGroups = c("Centers",layerctrl, paste0("Selected ", layerctrl)),
                       options = layersControlOptions(collapsed = FALSE))
    return(map)
  }

  #Growth reactions-------------------------------------------------------------------------------
  
  
  gRun <- reactive({
    input$growth_select_run
  })
  
  gGeog <- reactive({
    switch(as.integer(input$growth_select_geography),
           "zone",
           "faz",
           "city") 
  })
  
  gIndicator <- reactive({
    switch(as.integer(input$growth_select_indicator),
           "Total Population",
           "Households",
           "Employment",
           "Residential Units") 
  })
    
  gYear <- reactive({
    paste0("yr", input$growth_select_year)
  })
  
  gYear.label <- reactive({
    unlist(strsplit(gYear(),"r"))[[2]]
  })
  
  gTable <- reactive({
    dt <- alldt[run == gRun() & geography == gGeog() & indicator == gIndicator(), 
                .(name_id, geography, run, indicator, yr2014, get(gYear()))]
    setnames(dt, c(dt[,ncol(dt)-1], dt[,ncol(dt)]), c('yr1', 'yr2'))
    dt[,"diff" := (yr2-yr1)]
    
    switch(as.integer(input$growth_select_geography),
      dt <- merge(dt, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
      dt <- merge(dt, faz.lookup, by.x = "name_id", by.y = "faz_id"),
      dt <- merge(dt, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name")
      )
  })
  
  # shapefile ready for visualization
  gShape <- reactive({
    joinShp2Tbl(input$growth_select_geography, gTable())
  })
  
  # leaflet layer control
  geo <- reactive({
    switch(as.integer(input$growth_select_geography),
           geo <- "TAZ",
           geo <- "FAZ",
           geo <- "City"
    )
  })

    
  #Run Comparison reactions----------------------------------------------------------------------------
 
   
  cRun <- reactive({
    input$compare_select_run2
  })
  
  cGeog <- reactive({
    switch(as.integer(input$compare_select_geography),
           "zone",
           "faz",
           "city") 
  })
  
  cIndicator <- reactive({
    switch(as.integer(input$compare_select_indicator),
           "Total Population",
           "Households",
           "Employment",
           "Residential Units") 
  })
  
  cYear <- reactive({
    paste0("yr", input$compare_select_year)
  })
  
  cTable <- reactive({
    dt1 <- alldt[run == runname1 & geography == cGeog() & indicator == cIndicator(), 
                 .(name_id, geography, indicator, get(cYear()))]
    setnames(dt1, dt1[,ncol(dt1)], 'estrun1')
    
    dt2 <- alldt[run == cRun() & geography == cGeog() & indicator == cIndicator(), 
                 .(name_id, get(cYear()))]
    setnames(dt2, dt2[,ncol(dt2)], 'estrun2')
    
    dt <- merge(dt1, dt2, by = 'name_id')
    dt[,"diff" := (estrun1-estrun2)]
    
    switch(as.integer(input$compare_select_geography),
           dt <- merge(dt, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
           dt <- merge(dt, faz.lookup, by.x = "name_id", by.y = "faz_id"),
           dt <- merge(dt, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name")
    )
  })

  # shapefile ready for visualization
  cShape <- reactive({
    joinShp2Tbl(input$compare_select_geography, cTable())
  })
  
  # leaflet layer control
  cGeo <- reactive({
    switch(as.integer(input$compare_select_geography),
           geo <- "TAZ",
           geo <- "FAZ",
           geo <- "City"
    )
  })
  
  
  #Growth rendering------------------------------------------------------------------------------------  

  # Plotly
  output$growth_plot <- renderPlotly({
    scatterplot(gTable(), "growth", gTable()$yr1, gTable()$yr2, as.character(years[[1]]), gYear.label())
  })
  
  # Leaflet
  output$growth_map <- renderLeaflet({
    # Set up symbology and categorization
    colorBinResult <- map.colorBins(gShape()$diff, input$growth_select_geography)	
    pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=gShape()$diff, pretty = FALSE) 
    
    # popup setup
    geo.popup1 <- map.shp.popup(gShape(),'yr1','yr2',geo(),years[[1]],gYear.label())	
    geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)
    
    # Draw the map without selected geographies
    map <- map.layers(gShape(), geo(), paste0("2014-", gYear.label(), " growth by ", geo()), geo.popup1, geo.popup3, pal)
    
    # Re-draw the map with selected geographies
    subdata <- gSelected_geo()
    if(length(subdata) > 0) 
      map <- map %>% addSelectedGeo(gSelected_geo(), geo()) %>% map.settings(geo())

    map
 }) 

  # Drag event for the scatterplot: will grab ids of selected points
  gSelected_geo <- reactive({
    sub <- select.items("growth", input$growth_select_geography, gShape())
    return(sub)
  })

  
  #Run Comparison rendering-----------------------------------------------------------------------------
 
   
  # Plotly
  output$compare_plot <- renderPlotly({
    runname2.trim <- sapply(strsplit(cRun(),"[.]"), function(x) x[1])
    scatterplot(cTable(), "compare", cTable()$estrun1, cTable()$estrun2, runname1, runname2.trim)
  })

  # Leaflet
  output$compare_map <- renderLeaflet({
    runname2.trim <- sapply(strsplit(cRun(),"[.]"), function(x) x[1])

    # Set up symbology and categorization
    colorBinResult <- map.colorBins(cShape()$diff, input$compare_select_geography)
    pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=cShape()$diff, pretty = FALSE)
    
    # popup setup
    geo.popup1 <- map.shp.popup(cShape(),'estrun1','estrun2', cGeo(), runname1, runname2.trim)
    geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)
    
    # Draw the map without selected geographies
    map <- map.layers(cShape(), cGeo(), paste0("Run difference by ", cGeo()), geo.popup1, geo.popup3, pal)

    # Re-draw the map with selected geographies
    subdata <- cSelected_geo()
    if(length(subdata) > 0)
      map <- map %>% addSelectedGeo(cSelected_geo(), cGeo()) %>% map.settings(cGeo())

    map
  })

  # Drag event for the scatterplot: will grab ids of selected points
  cSelected_geo <- reactive({
    sub <- select.items("compare", input$compare_select_geography, cShape())
    return(sub)
  })
  
  #Time Series reactions and rendering--------------------------------------------------------
  
  lgarea <- list("EastsideKing_1","EastsideKing_2","GreenRiver","SeattleandShoreline","SEKingandKingOther",
                 "SWKing","Central,North,andSouthKitsap","PeninsulaandTacoma","PierceOther_1","PierceOther_2",
                 "SWPierce","Everett","NWSnohomish","SnohomishOther","SWSnohomish_1","SWSnohomish_2")
  
  tsSelected_plot <- reactive({
    plot <- lgarea[[as.integer(input$select_tsplots)]]
    file <- remove.spaces(paste0('qc_ts_city_', plot,'.html'))
    return(file)
  })
  
  output$tsplots <- renderText({
    t <- paste0('<iframe height=5000 width=2000 frameBorder=0 seamless="seamless" scrolling="yes" src="', tsSelected_plot(),'">')
    return(t)
    })
  
  
  #Demographic Indicator reactions and rendering---------------------------------------------
  
  # Display graphs or text depending if demographic indicators exist
  output$condDemog_Plot <- renderUI({
    if (!is.null(demog.table)){
      plotlyOutput("demog_plot", height = "850px")
    } else if (is.null(demog.table)) {
      verbatimTextOutput("demog_plot_test")
    }
  })
  
  output$demog_plot_test <- renderText({
    "Demographic indicators have not yet been generated for any of your runs"
  }) 
  
  # Returning a list of runs with demographic indicators or hide if not
  output$demog_Runs <- renderUI({
    if (!is.null(demog.table)){
      select.runs <- list(unique(demogdt[demographic == dDemographic(), run]))
      selectInput(inputId = "demog_select_run",
                  label = "Run",
                  choices = select.runs)
    } else if (is.null(demog.table)) {
      return() 
    }
  })
  
  dRun <- reactive({
    input$demog_select_run
  })
  
  dDemographic <- reactive({
    switch(as.integer(input$demog_select_demographic),
           "agegroup",
           "agegroup_intr",
           "dollargroup",
           "incomegroup",
           "persontype") 
  })
  
  # Build table for Plotly
  dTable <- reactive({
    if (input$demog_select_format == 1){
      demogdt[run == dRun() & demographic == dDemographic(),]
    } else if (input$demog_select_format == 2){
      main <- demogdt[run == dRun() & demographic == dDemographic(),]
      region.totals <- demogdt[run == dRun() & demographic == dDemographic(), .(total = sum(estimate)), by = year] 
      setkey(main, year)[region.totals, percent := round((estimate/total)*100, 2)] 
      dt <- main[,.(percent, year, run, groups, demographic)]
      setnames(dt, "percent", "estimate") 
      return(dt)
    }
  })
  
  # Build bar charts
  output$demog_plot <- renderPlotly({
    dat2 <- subset(as.data.frame(dTable()), year == "2014")
    
    one_plot <- function(dat){
      plot_ly(dat,
              x = ~groups,
              y = ~estimate,
              split = ~year,
              type = 'bar')%>%
        add_trace(x = dat2$groups,
                  y = dat2$estimate,
                  hoverinfo = "text",
                  text = paste("2014 Baseline:", dat2$estimate),
                  type = 'scatter',
                  mode = 'lines',
                  line = list(color = '#ff6500'),
                  showlegend = FALSE
        )%>%
        layout(xaxis = list(type = "category",
                            categoryorder = "array",
                            categoryarray = unique(dat$groups))
        )
    }
    
    data <- as.data.frame(dTable())
    
    p <- data %>%
      group_by(year) %>%
      do(p = one_plot(.)) %>%
      subplot(shareY = TRUE, nrows = 1) %>%
      layout(yaxis = list(title = " "),
             font = list(family="Segoe UI", size = 13),
             margin = list(l=100, b=195, t=50, r=100))
    
    p
  })
  
  #Development Capacity reactions and rendering-----------------------------------------------------
  
  # Returning a list of runs with DevCap indicators
  output$dcap_select_run <- renderUI({
    select.runs <- unique(devdt[, run]) #currently run is based on Dev source table, not a list?
    selectInput(inputId = "dcap_select_run",
                label = "Run",
                choices = select.runs)
  })
  
  dcapRun <- reactive({
    input$dcap_select_run
  })
  
  dcapGeog <- reactive({
    switch(as.integer(input$dcap_select_geography),
           "zone",
           "faz",
           "city",
           "growth_center") 
  })
  
  dcapYear <- reactive({
    input$dcap_select_year
  })
  
  dcapTable_total <- reactive({
    t1 <- capdt[run == dcapRun() & geography == dcapGeog() & captype == "Total",][,.(name_id, capacity, captype)]
    t2 <- devdt[run == dcapRun() & geography == dcapGeog() & year == dcapYear() & devtype == "Building Sqft",]
    t <- merge(t1, t2, by = c("name_id"))
    t0 <- t[, diff := capacity-estimate]
    
    switch(as.integer(input$dcap_select_geography),
           dt <- merge(t0, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
           dt <- merge(t0, faz.lookup, by.x = "name_id", by.y = "faz_id"),
           dt <- merge(t0, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name"),
           dt <- t0
    )
    return(dt)
    
  })
  
  dcapTable_res <- reactive({
    t1 <- capdt[run == dcapRun() & geography == dcapGeog() & captype == "Residential",][,.(name_id, capacity, captype)]
    t2 <- devdt[run == dcapRun() & geography == dcapGeog() & year == dcapYear() & devtype == "Residential Units",]
    t <- merge(t1, t2, by = "name_id")
    t0 <- t[, diff := capacity-estimate]
    
    switch(as.integer(input$dcap_select_geography),
           dt <- merge(t0, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
           dt <- merge(t0, faz.lookup, by.x = "name_id", by.y = "faz_id"),
           dt <- merge(t0, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name"),
           dt <- t0
    )
    
    return(dt)
  })
  
  dcapTable_nonres <- reactive({
    t1 <- capdt[run == dcapRun() & geography == dcapGeog() & captype == "Non-Residential",][,.(name_id, capacity, captype)]
    t2 <- devdt[run == dcapRun() & geography == dcapGeog() & year == dcapYear() & devtype == "Non-Residential Sqft",]
    t <- merge(t1, t2, by = "name_id")
    t0 <- t[, diff := capacity-estimate]
    
    switch(as.integer(input$dcap_select_geography),
           dt <- merge(t0, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
           dt <- merge(t0, faz.lookup, by.x = "name_id", by.y = "faz_id"),
           dt <- merge(t0, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name"),
           dt <- t0
    )
    
    return(dt)
  })
  
  # Total shapefile ready for visualization
  dcapShape_total <- reactive({
    joinShp2Tbl(input$dcap_select_geography, dcapTable_total()) 
  })
  
  # Residential shapefile ready for visualization
  dcapShape_res <- reactive({
    joinShp2Tbl(input$dcap_select_geography, dcapTable_res()) 
  })
  
  # Non-Residential shapefile ready for visualization
  dcapShape_nonres <- reactive({
    joinShp2Tbl(input$dcap_select_geography, dcapTable_nonres()) 
  })
  
  # leaflet layer control
  dcapGeo <- reactive({
    switch(as.integer(input$dcap_select_geography),
           geo <- "TAZ",
           geo <- "FAZ",
           geo <- "City",
           geo <- "Growth Center"
    )
  })
  
  # Total Dev Capacity map
  output$dcap_total_map <- renderLeaflet({
    if (is.na(dcapShape_total()$diff)){
      map <- leaflet()%>% 
        addProviderTiles("CartoDB.Positron", group = "Street Map")%>%
        setView(lng = -122.008546, lat = 47.549390, zoom = 9)
      
      map
    } else {
      # Set up symbology and categorization
      colorBinResult <- map.colorBins(dcapShape_total()$diff, input$dcap_select_geography)
      pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=dcapShape_total()$diff, pretty = FALSE)
      
      # popup setup
      geo.popup1 <- map.shp.popup(dcapShape_total(),'capacity','estimate',dcapGeo(), 'Total Max Development Capacity', 'Building Sqft')
      geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)
      
      if (as.integer(input$dcap_select_geography) == 4){
        map <- map.layers.basic(dcapShape_total(), dcapGeo(), "Total Development Capacity", geo.popup1, pal)
      } else {
        map <- map.layers(dcapShape_total(), dcapGeo(), "Total Development Capacity", geo.popup1, geo.popup3, pal)
      }
      
      map
    } # end else
  })
  
  # Residential Dev Capacity map
  output$dcap_res_map <- renderLeaflet({
    if (is.na(dcapShape_res()$diff)){
      map <- leaflet()%>% 
        addProviderTiles("CartoDB.Positron", group = "Street Map")%>%
        setView(lng = -122.008546, lat = 47.549390, zoom = 9)
      
      map
    } else {
    # Set up symbology and categorization
      colorBinResult <- map.colorBins(dcapShape_res()$diff, input$dcap_select_geography)
      pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=dcapShape_res()$diff, pretty = FALSE)
      
      # popup setup
      geo.popup1 <- map.shp.popup(dcapShape_res(),'capacity','estimate',dcapGeo(), 'Residential Max Development Capacity', 'Residential Units')
      geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)
      
      if (as.integer(input$dcap_select_geography) == 4){
        map <- map.layers.basic(dcapShape_res(), dcapGeo(), "Residential Development Capacity", geo.popup1, pal)
      } else {
        map <- map.layers(dcapShape_res(), dcapGeo(), "Residential Development Capacity", geo.popup1, geo.popup3, pal)
      }
      
      map
    } # end of else
  }) 
  
  # Non-Residential Dev Capacity map
  output$dcap_nonres_map <- renderLeaflet({
    if (is.na(dcapShape_nonres()$diff)){
      map <- leaflet()%>% 
        addProviderTiles("CartoDB.Positron", group = "Street Map")%>%
        setView(lng = -122.008546, lat = 47.549390, zoom = 9)
      
      map
    } else {
    # Set up symbology and categorization
      colorBinResult <- map.colorBins(dcapShape_nonres()$diff, input$dcap_select_geography)
      pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=dcapShape_nonres()$diff, pretty = FALSE)
      
      # popup setup
      geo.popup1 <- map.shp.popup(dcapShape_nonres(),'capacity','estimate',dcapGeo(), 'Non-Residential Max Development Capacity', 'Non-Residential Sqft')
      geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)
      
      if (as.integer(input$dcap_select_geography) == 4){
        map <- map.layers.basic(dcapShape_nonres(), dcapGeo(), "Non-Residential Development Capacity", geo.popup1, pal)
      } else {
        map <- map.layers(dcapShape_nonres(), dcapGeo(), "Non-Residential Development Capacity", geo.popup1, geo.popup3, pal)
      }
      
      map
    } # end of else
  }) 

}# end server function