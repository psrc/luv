function(input, output) {
  #functions------------------------------------------------------------------------------
  
  scatterplot <- function(xcolumn, ycolumn, xlayout, ylayout) {
    data <- gTable() 
    key <- data$name_id # uniquely identify geo for Plotly
    p <- plot_ly(data,
                 type = 'scatter',
                 x = xcolumn,
                 y = ycolumn, 
                 name = "",
                 source = "source",
                 text = paste0("ID: ", name_id,
                               "<br>Name: ", Name), 
                 key = key, # will appear in 'eventdata'
                 mode = 'markers',
                 showlegend = F)%>%
      add_trace(x=c(0,max(xcolumn)),
                y=c(0,max(xcolumn)),
                marker = list(color="grey", size = 0),
                opacity = .6,
                mode = "lines",
                showlegend = F)%>%
      layout(font = list(family="Segoe UI", size = 13.5),
             title = " ",
             xaxis = list(title = xlayout),
             yaxis = list(title = ylayout),
             margin = list(l=100, b=100, t=90, r=100)
      )
    p
  }

  #Growth reactions-----------------------------------------------------------------------
  
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
  
  #create function with input$growth_select_geography & gTable() as functions?!
  gShape <- reactive({
    if (input$growth_select_geography == 1){
      shape.join <- sp::merge(zone.shape, gTable(), by.x = "TAZ", by.y = "name_id")
      return(shape.join)
    } else if (input$growth_select_geography == 2){
      shape.join <- sp::merge(faz.shape, gTable(), by.x = "FAZ10", by.y = "name_id")
      return(shape.join)
    } else {
      shape.join <- sp::merge(city.shape, gTable(), by.x = "city_id", by.y = "name_id")
      return(shape.join)
    }
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
  
  cTable <- reactive({ #need to edit table to select from two runs for only one year
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

  cShape <- reactive({
    if (input$compare_select_geography == 1){
      shape.join <- sp::merge(zone.shape, cTable(), by.x = "TAZ", by.y = "name_id")
      return(shape.join)
    } else if (input$compare_select_geography == 2){
      shape.join <- sp::merge(faz.shape, cTable(), by.x = "FAZ10", by.y = "name_id")
      return(shape.join)
    } else {
      shape.join <- sp::merge(city.shape, cTable(), by.x = "city_id", by.y = "name_id")
      return(shape.join)
    }
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

  output$growth_plot <- renderPlotly({
    scatterplot(gTable()$yr1, gTable()$yr2, as.character(years[[1]]), gYear.label())
  })
  
  # leaflet layer control
  geo <- reactive({
    switch(as.integer(input$growth_select_geography),
      geo <- "TAZ",
      geo <- "FAZ",
      geo <- "City"
    )
  })

  output$growth_map <- renderLeaflet({
    #runname2.trim <- sapply(strsplit(run(),"[.]"), function(x) x[1])
    
    # Set up symbology and categorization
    rng <- range(gShape()$diff)
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
      if (input$growth_select_geography == 1){ # separate bins for TAZ
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
    
    pal <- colorBin(palette = color, bins = bin, domain=gShape()$diff, pretty = FALSE) 
    
    # popup setup
    geo.popup1 <- paste0("<strong>ID: </strong>", gShape()$name_id,
                         "<br><strong>", geo(), " Name: </strong>", gShape()$Name,
                         "<br><strong>", years[[1]]," estimate: </strong>", gShape()@data[,'yr1'],
                         "<br><strong>", gYear.label()," estimate: </strong>", gShape()@data[,'yr2'],
                         "<br><strong>Difference: </strong>", gShape()$diff
    )
    
    geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)
    
    map <- leaflet(data=gShape())%>% 
      addProviderTiles("CartoDB.Positron", group = "Street Map")%>%
      addProviderTiles("Esri.WorldImagery", group = "Imagery")%>%
      addPolygons(fillColor = ~pal(gShape()$diff),
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
                values = pal(gShape()$diff),
                title = paste0("2014-", gYear.label(), " growth by ", geo()),
                opacity =1,
                labFormat = labelFormat(digits = 0, big.mark = ","))%>%
      setView(lng = -122.008546, lat = 47.549390, zoom = 9)%>%
      addLayersControl(baseGroups = c("Street Map", "Imagery"),
                       overlayGroups = c(geo(), "Centers"),
                       options = layersControlOptions(collapsed = FALSE))

      # Re-draw the map with selected geographies
      subdata <- gSelected_geo()
      if(length(subdata) > 0)
        map <- map %>%
        addPolygons(data = gSelected_geo(),
                    fill = FALSE,
                    color = '#FFFF00',
                    opacity = 1,
                    group = paste0("Selected ", geo()))

      map <- map %>%
        setView(lng = -122.008546, lat = 47.549390, zoom = 9)%>%
        addLayersControl(baseGroups = c("Street Map", "Imagery"),
                         overlayGroups = c("Centers",geo(), paste0("Selected ", geo())),
                         options = layersControlOptions(collapsed = FALSE))
      map

 }) # end renderLeaflet

  # Drag event for the scatterplot: will grab ids of selected points
  gSelected_geo <- reactive({
    eventdata <- event_data(event = "plotly_selected", source = "source")
    if(is.null(eventdata)){
      return(NULL) # do nothing
    } else {
      geoid <- eventdata[['key']]
      if (input$growth_select_geography == 1){
        sub <- gShape()[gShape()$name_id %in% geoid, ] # Give back a sp data frame of the selected ids
        return(sub)
      } else if (input$growth_select_geography == 2){
        sub <- gShape()[gShape()$name_id %in% geoid, ]
        return(sub)
      } else {
        sub <- gShape()[gShape()$name_id %in% geoid, ]
        return(sub)
      }
    }
  })

  
  #Run Comparison--------------------------------------------------------------------------------------
  
  # Plotly
  output$compare_plot <- renderPlotly({
    runname2.trim <- sapply(strsplit(cRun(),"[.]"), function(x) x[1])
    scatterplot(cTable()$estrun1, cTable()$estrun2, runname1, runname2.trim)
  })

  # # Draw the map without selected geographies
  # output$compare_map <- renderLeaflet({
  #   
  #   runname2.trim <- sapply(strsplit(run(),"[.]"), function(x) x[1])
  #   
  #   # Set up symbology and categorization
  #   rng <- range(shape()$diff)
  #   if (rng[1] < 0 & rng[2] > 0){
  #     diff.range <- "both"
  #   } else if (rng[1] >=0 & rng[2] > 0){
  #     diff.range <- "pos"
  #   } else if (rng[1] < 0 & rng[2] < 0){
  #     diff.range <- "neg"
  #   } else {
  #     diff.range <- "none"
  #   }
  #   
  #   if (diff.range == "both"){
  #     color <- c("#053061",
  #                "#2166ac", 
  #                "#4393c3", 
  #                "#92c5de", 
  #                "#d1e5f0", 
  #                "#ffffff", 
  #                "#f7f7f7", 
  #                "#fddbc7", 
  #                "#f4a582", 
  #                "#d6604d", 
  #                "#b2182b", 
  #                "#67001f")
  #     if (input$compare_select_geography == 1){ # separate bins for TAZ
  #       bin <- c(-70000, -3000, -1000, -500,- 250, 0, 1, 250, 500, 1000, 3000, 70000)
  #     } else { # for other geographies
  #       bin <- c(-70000, -30000, -10000, -5000,- 2500, 0, 1, 2500, 5000, 10000, 30000, 70000)
  #     }
  #   } else if (diff.range == "pos"){
  #     color <- "Reds"
  #     bin <- c(0, 500, 1000, 2500, 5000, 7000, 10000, 25000, 30000, 70000)
  #   } else if (diff.range == "neg"){
  #     color <- "Blues"
  #     bin <- c(-70000, -30000, -10000, -25000, -5000,- 2500, -1000, -500, 0)
  #   } else if (diff.range == "none"){
  #     color <- "transparent"
  #     bin <- c(0, 1)
  #   }
  #   
  #   pal <- colorBin(palette = color, bins = bin, domain=shape()$diff, pretty = FALSE)    
  #   
  #   geo.popup1 <- paste0("<strong>ID: </strong>", shape()$name_id,
  #                        "<br><strong>Large Area Name: </strong>", shape()$Name,
  #                        "<br><strong>", runname1," estimate: </strong>", shape()$estrun1,
  #                        "<br><strong>", runname2.trim," estimate: </strong>", shape()$estrun2,
  #                        "<br><strong>Difference: </strong>", shape()$diff
  #   )
  #   
  #   map <- leaflet()%>%
  #     addProviderTiles("CartoDB.Positron", group = "Street Map")%>%
  #     addProviderTiles("Esri.WorldImagery", group = "Imagery")%>%
  #     clearShapes()%>%
  #     addPolygons(data = shape(),
  #                 fillColor = ~pal(shape()$diff),
  #                 fillOpacity = 0.7,
  #                 stroke = TRUE,
  #                 color = "#8a8a95",
  #                 weight = 1,
  #                 group = geo(),
  #                 popup = geo.popup1
  #     )%>%
  #     addLegend(position = "bottomright",
  #               pal = pal,
  #               values = pal(shape()$diff),
  #               title = paste0("Run difference by ", geo()),
  #               opacity =1,
  #               labFormat = labelFormat(digits = 0, big.mark = ","))
  #   
  #   # Re-draw the map with selected geographies    
  #   subdata <- selected_geo()
  #   if(length(subdata) > 0)
  #     map <- map %>% 
  #     addPolygons(data = selected_geo(),
  #                 fill = FALSE,
  #                 color = '#FFFF00',
  #                 opacity = 1,
  #                 group = paste0("Selected ", geo()))
  #   
  #   map <- map %>%
  #     setView(lng = -122.008546, lat = 47.549390, zoom = 9)%>%
  #     addLayersControl(baseGroups = c("Street Map", "Imagery"),
  #                      overlayGroups = c(geo(), paste0("Selected ", geo())),
  #                      options = layersControlOptions(collapsed = FALSE))
  #   map  
  #   
  # })
  # 
  # # Drag event for the scatterplot: will grab ids of selected points
  # selected_geo <- reactive({
  #   eventdata <- event_data(event = "plotly_selected", source = "source")
  #   if(is.null(eventdata)){
  #     return(NULL) # do nothing
  #   } else {
  #     geoid <- eventdata[['key']]
  #     if (input$compare_select_geography == 1){
  #       sub <- shape()[shape()$TAZ %in% geoid, ] # Give back a sp data frame of the selected ids
  #       return(sub)
  #     } else if (input$compare_select_geography == 2){
  #       sub <- shape()[shape()$nFAZ10 %in% geoid, ] 
  #       return(sub)
  #     } else {
  #       sub <- shape()[shape()$city_id %in% geoid, ] 
  #       return(sub)
  #     } 
  #   }
  # }) 
  
}# end server function