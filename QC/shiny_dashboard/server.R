function(input, output) {

  #Growth-----------------------------
  
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
  
  # table contains only select years
  gTable <- reactive({
    dt <- alldt[run == gRun() & geography == gGeog() & indicator == gIndicator(), 
          .(name_id, geography, run, indicator, yr2014, get(gYear()))]
    setnames(dt, dt[,ncol(dt)], gYear())
    dt[,"diff" := (get(gYear())-yr2014)]
  })
  
  # gShape <- reactive({
  # 
  # })
  
  output$growth_table <- renderPrint({
    print(gTable())
  })
  
  # output$growth_plot <- renderPlotly({
  #   #need table
  # })
  # 
  # output$growth_map <- renderLeaflet({
  #   #need shapefile
  # })
  
  
  #Run Comparison---------------------Need to change from accessing list to table or environments
  # run <- reactive({
  # paste0(input$compare_select_run2, ".data")
  # })
  # 
  # table <- reactive({ 
  #   switch(as.integer(input$compare_select_indicator),
  #          "Total Population",
  #          "Households",
  #          "Employment",
  #          "Residential Units") 
  # })
  # 
  # geog <- reactive({
  #   geo <- as.integer(input$compare_select_geography)
  #   get(run())[[geo]][[1]]
  # })
  # 
  # # Plotly
  # output$compare_plot <- renderPlotly({
  #   data <- subset(geog(), geog()$indicator==table()) 
  #   runname2.trim <- sapply(strsplit(run(),"[.]"), function(x) x[1])
  #   
  #   # uniquely identify geo for Plotly
  #   switch(as.integer(input$compare_select_geography),
  #          key <- data$zone_id,
  #          key <- data$faz_id,
  #          key <- data$city_id)
  #   
  #   p <- plot_ly(data,
  #                type = 'scatter',
  #                x = estrun1,
  #                y = estrun2,
  #                name = "",
  #                source = "source",
  #                text = paste0("ID: ", data[,1], 
  #                              " Name: ", data[,grepl("name|Name|city_name",names(data))]),
  #                key = key, # will appear in 'eventdata'
  #                mode = 'markers',
  #                showlegend = F)%>%
  #     add_trace(x=c(0,max(estrun1)), 
  #               y=c(0,max(estrun1)),
  #               marker = list(color="grey", size = 0),
  #               opacity = .6,
  #               mode = "lines",
  #               showlegend = F)%>%
  #     layout(font = list(family="Segoe UI", size = 13.5),
  #            title = " ",
  #            xaxis = list(title = runname1),
  #            yaxis = list(title = runname2.trim),
  #            margin = list(l=100, b=100, t=90, r=100)
  #     )
  #   p
  # })
  # 
  # 
  # shape_select <- reactive({ 
  #   geo <- as.integer(input$compare_select_geography)
  #   get(run())[[geo]][[2]]
  # })
  # 
  # shape <- reactive({
  #   data1 <- subset(geog(), geog()$indicator==table())
  #   if (input$compare_select_geography == 1){
  #     shape.join <- sp::merge(shape_select(), data1, by.x = c("TAZ"), by.y = c("zone_id"))
  #     return(shape.join)
  #   } else if (input$compare_select_geography == 2){
  #     shape.join <- sp::merge(shape_select(), data1, by.x = c("FAZ10", "Name"), by.y = c("faz_id", "Name"))
  #     return(shape.join)
  #   } else {
  #     shape.join <- sp::merge(shape_select(), data1, by = "city_id")
  #     return(shape.join)
  #   }
  # })
  # 
  # # leaflet layer control
  # geo <- reactive({
  #   if (input$compare_select_geography == 1){
  #     geo <- "TAZ"
  #     return(geo)
  #   } else if (input$compare_select_geography == 2){
  #     geo <- "FAZ"
  #     return(geo)
  #   } else {
  #     geo <- "City"
  #     return(geo)
  #   }
  # })
  # 
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