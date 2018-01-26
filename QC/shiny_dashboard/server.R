server <- function(input, output, session) {
  
# functions ---------------------------------------------------------------

  
  # remove all whitespaces
  remove.spaces <- function(x) gsub(" ", "", x, fixed = TRUE)
  
  # Creates a Plotly line graph for Vacancy tab
  linegraph <- function(table, xcolumn, ycolumn, split) {
    table %>%
      plot_ly(x = ~xcolumn,
              y = ~ycolumn,
              color = ~split,
              colors = "Dark2",
              type = 'scatter',
              mode = 'lines') %>%
      layout(font = list(family = "Segoe UI", size = 12),
             yaxis = list(title = ""),
             xaxis = list(title = "Year")
      )
  }
  
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
    switch(as.integer(inputGeog), 
      sp::merge(zone.shape, table, by.x = "TAZ", by.y = "name_id"),
      sp::merge(faz.shape, table, by.x = "FAZ10", by.y = "name_id"),
      sp::merge(city.shape, table, by.x = "city_id", by.y = "name_id"),
      {centers.shape <- centers[centers$name_id != 0,];
      sp::merge(centers.shape, table, by.x = "name_id", by.y = "name_id")}
    )
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
  c.map.shp.popup <- function(shapefile, baseyear, xcolumn, ycolumn, layerctrl, xtitle, ytitle){
    paste0("<strong>ID: </strong>", shapefile$name_id,
           "<br><strong>", layerctrl, " Name: </strong>", shapefile$Name,
           "<br><strong>", years[1]," estimate: </strong>", prettyNum(round(shapefile@data[,baseyear], 0), big.mark = ","),
           "<br><strong>", xtitle," estimate: </strong>", prettyNum(round(shapefile@data[,xcolumn], 0), big.mark = ","),
           "<br><strong>", ytitle," estimate: </strong>", prettyNum(round(shapefile@data[,ycolumn], 0), big.mark = ","),
           "<br><strong>Difference: </strong>", prettyNum(round(shapefile$diff, 0), big.mark = ","))
  }
 
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
      addEasyButton(
        easyButton(
          icon="fa-globe", 
          title="Zoom to Region",
          onClick=JS("function(btn, map){ 
                     map.setView([47.549390, -122.008546],9);}"))
          )%>%
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
  select.items <- function(sourcename, shapefile){
    eventdata <- event_data(event = "plotly_selected", source = sourcename)
    if(is.null(eventdata)) return(NULL) # do nothing
    else {
      geoid <- eventdata[['key']]
      return(shapefile[shapefile$name_id %in% geoid, ])
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
  
  # Prepares generic series of calculation on Modellers topsheet
  calc.cols.tsTable <- function(table, select.years){
    table[, Change := (table[[ncol(table)-1]]-table[[ncol(table)]])
       ][, Per.Change := round((Change/table[[4]])*100, 2)
         # ][, Per.Growth := round(Change/(table[[4]]-table[[2]])*100, 2)
           ][, r1.baseyr := (table[[3]]-table[[2]])
             ][, r1.baseyr.per := round((r1.baseyr/table[[2]])*100, 2)
               ][, r1.avgann := round(((table[[3]]/table[[2]])^(1/(as.numeric(select.years[2])-as.numeric(select.years[1])))-1)*100, 2)
                 ][, r2.baseyr := (table[[4]]-table[[2]])
                   ][, r2.baseyr.per := round((r2.baseyr/table[[2]])*100, 2)
                     ][, r2.avgann := round(((table[[4]]/table[[2]])^(1/(as.numeric(select.years[2])-as.numeric(select.years[1])))-1)*100, 2)]
    return(table)
  }
  
  # Prepares generic topsheet table for main indicators (households, population, employment)
  create.tsTable <- function(table, idname){ #idname aka 'County' or 'Name'
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    sel.yrs.col <- paste0("yr", sel.yr.fl)

    t1 <- dcast.data.table(table, paste(idname, "~ run"), value.var = sel.yrs.col)
    setcolorder(t1, c(idname, paste0(sel.yrs.col[1],"_",runs[1]), paste0(sel.yrs.col[2],"_",runs[1]), paste0(sel.yrs.col[2],"_",runs[2]), paste0(sel.yrs.col[1],"_",runs[2])))
    t1[, ncol(t1) := NULL]
    t1[, Change := (t1[[ncol(t1)-1]]-t1[[ncol(t1)]])
       ][, Per.Change := round((Change/t1[[4]])*100, 2)
         ][, Per.Growth := round(Change/(t1[[4]]-t1[[2]])*100, 2)
           ][, r1.baseyr := (t1[[3]]-t1[[2]])
             ][, r2.baseyr := (t1[[4]]-t1[[2]])
               ][, r1.baseyr.per := round((r1.baseyr/t1[[2]])*100, 2)
                 ][, r2.baseyr.per := round((r2.baseyr/t1[[2]])*100, 2)
                   ][, r1.avgann := round(((t1[[3]]/t1[[2]])^(1/(as.numeric(sel.yr.fl[2])-as.numeric(sel.yr.fl[1])))-1)*100, 2)
                     ][, r2.avgann := round(((t1[[4]]/t1[[2]])^(1/(as.numeric(sel.yr.fl[2])-as.numeric(sel.yr.fl[1])))-1)*100, 2)]
    setnames(t1, colnames(t1), c(idname,
                                 paste0(sel.yr.fl[1], "_", runs[1]),
                                 paste0(sel.yr.fl[2], "_", runs[1]),
                                 paste0(sel.yr.fl[2], "_", runs[2]),
                                 "Change",
                                 "Per.Change",
                                 "Per.Growth",
                                 "r1.baseyr",
                                 "r2.baseyr",
                                 "r1.baseyr.per",
                                 "r2.baseyr.per",
                                 "r1.avgann",
                                 "r2.avgann"))
    t1[, `:=` (r1dist = round(r1.baseyr/(unlist(t1[like(get(eval(idname)), "Sub-Total"), .(r1.baseyr)])[[1]])*100, 2), r2dist = round(r2.baseyr/(unlist(t1[like(get(eval(idname)), "Sub-Total"), .(r2.baseyr)])[[1]])*100, 2))]
    setcolorder(t1, c(idname,
                      paste0(sel.yr.fl[1], "_", runs[1]),
                      paste0(sel.yr.fl[2], "_", runs[1]),
                      paste0(sel.yr.fl[2], "_", runs[2]),
                      "Change",
                      "Per.Change",
                      "r1dist",
                      "r2dist",
                      "Per.Growth",
                      "r1.baseyr",
                      "r1.baseyr.per",
                      "r1.avgann",
                      "r2.baseyr",
                      "r2.baseyr.per",
                      "r2.avgann"
                      ))

    t1[, c(2:4, 10, 13) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = c(2:4, 10, 13)]
  }

  # Prepares expanded topsheet table for RGCs & Key Locations
  create.exp.tsTable <- function(table){
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    sel.yrs.col <- paste0("yr", c(years[1], tsYear))
    
    setcolorder(table, 
                c("Name",
                  paste0(sel.yrs.col[1], "_", "Population","_", runs[1]),
                  paste0(sel.yrs.col[2], "_", "Population","_", runs[1]),
                  paste0(sel.yrs.col[2], "_", "Population","_", runs[2]),
                  paste0(sel.yrs.col[1], "_", "Employment","_", runs[1]),
                  paste0(sel.yrs.col[2], "_", "Employment","_", runs[1]),
                  paste0(sel.yrs.col[2], "_", "Employment","_", runs[2]),
                  paste0(sel.yrs.col[1], "_", "Population","_", runs[2]),
                  paste0(sel.yrs.col[1], "_", "Employment","_", runs[2])))
    table[, (ncol(table)-1):ncol(table) := NULL] 
    
    t1<- table[, Pop.Change := (table[[3]]-table[[4]])
               ][, Pop.Per.Change := round((Pop.Change/table[[4]])*100, 2)
                 ][, Emp.Change := (table[[6]]-table[[7]])
                   ][, Emp.Per.Change := round((Emp.Change/table[[7]])*100, 2)]
    setcolorder(t1,
                c("Name",
                  paste0(sel.yrs.col[1], "_", "Population","_", runs[1]),
                  paste0(sel.yrs.col[2], "_", "Population","_", runs[1]),
                  paste0(sel.yrs.col[2], "_", "Population","_", runs[2]),
                  "Pop.Change",
                  "Pop.Per.Change",
                  paste0(sel.yrs.col[1], "_", "Employment","_", runs[1]),
                  paste0(sel.yrs.col[2], "_", "Employment","_", runs[1]),
                  paste0(sel.yrs.col[2], "_", "Employment","_", runs[2]),
                  "Emp.Change",
                  "Emp.Per.Change"))
    setnames(t1, colnames(t1), c("Name",
                                 paste0(sel.yr.fl[1], "_", "Pop","_", runs[1]),
                                 paste0(sel.yr.fl[2], "_", "Pop","_", runs[1]),
                                 paste0(sel.yr.fl[2], "_", "Pop","_", runs[2]),
                                 "Pop.Change",
                                 "Pop.Per.Change",
                                 paste0(sel.yr.fl[1], "_", "Emp","_", runs[1]),
                                 paste0(sel.yr.fl[2], "_", "Emp","_", runs[1]),
                                 paste0(sel.yr.fl[2], "_", "Emp","_", runs[2]),
                                 "Emp.Change",
                                 "Emp.Per.Change"))
    t1[, c(2:4,7:9) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = c(2:4,7:9)]
  }
  
  # Create basic table container
  sketch.basic <- function(grpcol, year1, year2, run1, run2){
    htmltools::withTags(table(
      class = 'display', 
      thead(
        tr(
          th(rowspan = 3, grpcol),
          th(bgcolor='AliceBlue', colspan = 1, year1),
          th(colspan = 10, year2)
        ),
        tr(
          th(style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='AliceBlue', 'A'),
          lapply(list('B'), function(x) th(style="font-size:12px; font-style:italic; font-weight:normal;", x)),
          lapply(list('C = B-A', 'D = C/A', ''), function(x) th(style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='LightGoldenRodYellow', x)),
          lapply(list('E'), function(x) th(style="font-size:12px; font-style:italic; font-weight:normal;", x)),
          lapply(list('F = E-A', 'G = F/A', ''), function(x) th(style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='LightGoldenRodYellow', x)),
          lapply(list('H = B-E', 'I = H/E'), function(x) th(style="font-size:12px; font-style:italic; font-weight:normal;", x))
        ),
        tr(
          th(style="font-size:14px;", bgcolor='AliceBlue', run1),
          th(style="font-size:14px;",run1),
          lapply(c('Growth', '% Growth', '% AvgAnn'), function(x) th(style="font-size:14px;",bgcolor='LightGoldenRodYellow', x)),
          th(style="font-size:14px;",run2),
          lapply(c('Growth', '% Growth', '% AvgAnn'), function(x) th(style="font-size:14px;",bgcolor='LightGoldenRodYellow', x)),
          lapply(c('Change', '% Change'), function(x) th(style="font-size:14px;",x))
        )
      ) # end thead
    )) # end withTags/table
  }
  
  sketch.basic.growth <- function(grpcol, year1, year2, run1, run2){
    htmltools::withTags(table(
      class = 'display', 
      thead(
        tr(
          th(rowspan = 3, grpcol),
          th(bgcolor='AliceBlue', colspan = 1, year1),
          th(colspan = 7, year2)
        ),
        tr(
          th(style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='AliceBlue', 'A'),
          lapply(c('B', '(B-A)/(subtotal[B-A])'), function(x) th(style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='MintCream', x)), 
          lapply(c('C', '(C-A)/(subtotal[C-A])'), function(x) th(style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='Ivory', x)),
          lapply(c('D = B-C', 'D/C', 'D/(C-A)'), function(x) th(style="font-size:12px; font-style:italic; font-weight:normal;", x))
        ),
        tr(
          th(style="font-size:13px;", bgcolor='AliceBlue', run1),
          lapply(c(run1, paste('%', run1, "Growth Distribution")), function(x) th(style="font-size:13px;", bgcolor='MintCream', x)),
          lapply(c(run2 , paste('%', run2, "Growth Distribution")), function(x) th(style="font-size:13px;", bgcolor='Ivory', x)),
          lapply(c('Change', '% Change', '% Growth from Base Year'), function(x) th(style="font-size:13px;", x))
        )
      ) # end thead
    )) # end withTags/table
  }
  
  # Create expanded table container
  sketch.expanded <- function(grpcol, year1, year2, run1, run2){
    htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th(rowspan = 4, grpcol),
          th(colspan = 5, 'Population'),
          th(colspan = 5, 'Employment')
        ),
        tr(
          rep(list(th(bgcolor='AliceBlue', colspan = 1, year1), th(colspan = 4, year2)), 2)
        ),
        tr(
          rep(
            list(th(style="font-size:12px; font-style:italic; font-weight:normal;", bgcolor='AliceBlue', 'A'),
                 lapply(list('B', 'C', 'D = B-C', 'D/C'), function(x) th(style="font-size:12px; font-style:italic; font-weight:normal;", x))),
            2)
        ),
        tr(
          rep(list(th(bgcolor='AliceBlue', run1), lapply(c(run1, run2, 'Change', '% Change'), th)), 2)
        )
      ) # end thead
    )) # end withTags/table
  }
  
  # Create a basic DT
  create.DT.basic <- function(table, acontainer){
    DT::datatable(table,
                  extensions = 'Buttons',
                  class = 'cell-border stripe',
                  options = list(columnDefs = list(list(className = 'dt-center', targets = 1:11), 
                                                   list(width = '20%', targets = 0)),
                                 dom = 'Bfrtip',
                                 buttons = list('copy',
                                                list(extend = 'excel',
                                                     buttons = 'excel',
                                                     filename = 'LUVQCDashboard')),
                                 #autoWidth = TRUE,
                                 paging = FALSE, 
                                 searching = FALSE 
                  ),
                  container = acontainer, 
                  rownames = FALSE
    ) %>% 
      formatStyle(colnames(table)[(ncol(table)-1):(ncol(table))],
                  color = styleInterval(c(0), c('red', 'black'))) %>%
      formatStyle(colnames(table)[2],
                  backgroundColor = 'AliceBlue') %>%
      formatStyle(colnames(table)[4:6],
                  backgroundColor = 'LightGoldenRodYellow') %>%
      formatStyle(colnames(table)[8:10],
                  backgroundColor = 'LightGoldenRodYellow')
  }
  
  # Create a basic DT for Growth Topsheet
  create.DT.basic.growth <- function(table, acontainer){
    DT::datatable(table,
                  extensions = 'Buttons',
                  class = 'cell-border stripe',
                  options = list(columnDefs = list(list(className = 'dt-center', targets = 1:8), 
                                                   list(width = '15%', targets = 0)),
                                 dom = 'Bfrtip',
                                 buttons = list('copy',
                                                list(extend = 'excel',
                                                     buttons = 'excel',
                                                     filename = 'LUVQCDashboard')),
                                 #autoWidth = TRUE,
                                 paging = FALSE, 
                                 searching = TRUE
                  ),
                  container = acontainer, 
                  rownames = FALSE
    ) %>% 
      formatStyle(colnames(table)[c(4, (ncol(table)-3):(ncol(table)))], color = styleInterval(c(0), c('red', 'black'))
                  ) %>%
      formatStyle(colnames(table)[2], backgroundColor = 'AliceBlue'
                  ) %>%
      formatStyle(colnames(table)[3:4], backgroundColor = 'MintCream'
                  ) %>%
      formatStyle(colnames(table)[5:6], backgroundColor = 'Ivory'
                  )

  }
  
  # Create an expanded DT
  create.DT.expanded <- function(table, acontainer){
    DT::datatable(table,
                  extensions = 'Buttons',
                  class = 'cell-border stripe',
                  options = list(columnDefs = list(list(className = 'dt-center', targets = 1:10),
                                                   list(width = '20%', targets = 0)),
                                 dom = 'Bfrtip',
                                 buttons = list('copy',
                                                list(extend = 'excel',
                                                     buttons = 'excel',
                                                     filename = 'LUVQCDashboard')),
                                 paging = FALSE, 
                                 searching = FALSE 
                  ),
                  container = acontainer, 
                  rownames = FALSE
    ) %>% 
      formatStyle(colnames(table)[c(5:6, (ncol(table)-1):(ncol(table)))],
                  color = styleInterval(c(0), c('red', 'black'))) %>%
      formatStyle(colnames(table)[c(2,7)],
                  backgroundColor = 'AliceBlue')
  }
  
  create.DT.generic <- function(table) {
    datatable(table,
              extensions = 'Buttons',
              class = 'cell-border stripe',
              options = list(dom = 'Bfrtip',
                             buttons = list('copy',
                                            list(extend = 'excel',
                                                 buttons = 'excel',
                                                 filename = 'LUVQCDashboard')),
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'font-size': '15px'});",
                               "}"),
                             paging = TRUE,
                             pageLength = 150,
                             searching = TRUE,
                             scrollY = '450px'
              )
    ) %>%
      formatStyle(colnames(table)[1:ncol(table)],
                  `font-size` = '13px')
  }


# Bookmarking State -------------------------------------------------------

  setBookmarkExclude(c("bookmark1"))
  
  # Trigger bookmarking with either button
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  
  onBookmark(function(state) {
    state$values$submitted <- vars$submitted
    state$values$dir <- vars$result.dir
    state$values$select_run1 <- vars$select_run1
    state$values$select_run2all <- vars$select_run2all
    state$values$runnames <- vars$runnames
    state$values$runname1 <- vars$runname1
    state$values$runnames2 <- vars$runnames2
    state$values$runs <- vars$runs
  })
  
  onRestore(function(state) {
    vars$submitted <- state$values$submitted
    vars$result.dir <- state$values$dir
    vars$select_run1 <-state$values$select_run1 
    vars$select_run2all<-state$values$select_run2all
    vars$runnames<-state$values$runnames
    vars$runname1<-state$values$runname1
    vars$runnames2<-state$values$runnames2 
    vars$runs<- state$values$runs 
  })

      
# Initialize Dashboard ----------------------------------------------------

  
  trim.subdir <- tempfile(pattern="sessiondir", tmpdir=".")
  # trim.subdir <- tempfile(pattern="sessiondir", tmpdir="")
  subdir <- file.path("www", trim.subdir)
  vars <- reactiveValues(submitted = FALSE, 
                         result.dir = NULL,
                         select_run1 = NULL,
                         select_run2all = NULL,
                         runnames = NULL,
                         runname1 = NULL,
                         runnames2 = NULL,
                         runs = NULL
                         )
  
  base.dir <- reactive({input$init_select_allruns})

  output$init_select_resultsdir <- renderUI({
    select.resultsdir <- list.files(file.path(wrkdir, "results"))
    selectInput(inputId = "select_resultsdir",
                label = "Makefile Results Folder*",
                choices = select.resultsdir,
                width = "100%")
  })
  

  resultsDir <- reactive({
    vars$result.dir
  })
  
  selectRun1 <- reactive({
    vars$select_run1
  })

  selectRun2 <- reactive({
    vars$select_run2all
  })

  runnames <- reactive({
    vars$runnames
  })

  runname1 <- reactive({
    vars$runname1
  })

  runnames2 <- reactive({
    vars$runnames2
  })

  runs <- reactive({
    vars$runs
  })
  
  # create sub-directory in 'www'
  # find text files from results dir and copy to www dir
  observeEvent(input$goButton, {
    if(length(input$select_resultsdir) == 0) return()
    if (!(file.exists(subdir))) dir.create(subdir)
    vars$result.dir <- file.path(wrkdir, "results", input$select_resultsdir)
    vars$select_run1 <- str_extract(input$init_select_allruns[1], "runs/(.*)") %>% str_split("/") %>% unlist %>% .[2]
    vars$select_run2all <- lapply(input$init_select_allruns[2:length(input$init_select_allruns)], function(x) str_extract(x, "runs/(.*)") %>% str_split("/") %>% unlist %>% .[2]) %>% unlist
    vars$runnames <-  c(vars$select_run1, vars$select_run2all)
    vars$runname1 <- unlist(strsplit(vars$select_run1,"[.]"))[[1]]
    vars$runnames2 <- sapply(strsplit(vars$select_run2all,"[.]"), function(x) x[1])
    vars$runs <- c(vars$runname1, unlist(vars$runnames2))
    
    result.dir <- resultsDir()
    
    flist <- list.files(subdir, glob2rx('*.txt|*.html'), full.names = TRUE, include.dirs=TRUE, ignore.case=TRUE)
    if (length(flist) > 0) file.remove(flist)
    unlink(file.path(subdir, 'index_files'), recursive = TRUE)
    flist <- list.files(result.dir, glob2rx('*.txt|*.html'), full.names = TRUE, include.dirs=TRUE, ignore.case=TRUE)
    if (length(flist) > 0) file.copy(flist, subdir)

    # remove index.html from www dir
    # fn <- list.files('www', glob2rx('index.html'), full.names = TRUE, include.dirs=TRUE, ignore.case=TRUE)
    # if (length(fn) > 0 && file.exists(fn)) file.remove(fn)
    indexf.dir <- file.path(result.dir,"index_files")
     if (length(indexf.dir) == 0) browser()
    if(file.exists(indexf.dir)) {
    	file.copy(indexf.dir, subdir, recursive = TRUE)
    	indexdirs <- c()#'bootstrap-3.3.5', 'jquery-1.11.3'
    	for (dir in indexdirs)
      		unlink(file.path(subdir, 'index_files', dir), recursive = TRUE)
    }
    vars$submitted <- TRUE
  })
  
  output$link <- renderUI({HTML(paste0("<a href=", "'", file.path(trim.subdir, 'index.html'), "'", "target='blank'>View Index file</a>"))})

  # Compile Source Tables ---------------------------------------------------  
    
  # build general attributes source table
  alldt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
    base.dir <- base.dir()
    
    # ititialize alldata.table
    alldata.table <- data.frame(matrix(ncol = length(years) + 4, nrow = 0)) 
    new.alldata.table.colnames <- c("name_id", paste0("yr", years), "indicator", "geography", "run") 
    colnames(alldata.table) <- new.alldata.table.colnames
    alldata.table <- alldata.table %>% as.data.table()
    
    for (r in 1:length(runnames)) {
      for (a in 1:length(geography)){
        for (i in 1:length(attribute)){
          filename <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
          datatable <- read.csv(file.path(base.dir[r], "indicators",filename), header = TRUE, sep = ",")
          colnames(datatable)[2: ncol(datatable)] <- str_replace(colnames(datatable)[2: ncol(datatable)], '\\w+_', 'yr') # rename columns
          colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
          datatable$indicator <- switch(attribute[i],
                                        "population"="Total Population",
                                        "households"="Households",
                                        "employment"="Employment",
                                        "residential_units"="Residential Units")
          
          datatable$geography <- geography[a]
          datatable$run <- runs[r]
          datatable <- datatable %>% as.data.table()
          alldata.table <- rbindlist(list(alldata.table, datatable), use.names = TRUE, fill = TRUE)
        } # end of attribute loop
      } # end of geography loop
    } # end of runnames loop
    
    # convert class of selected columns
    logical.cols <- setdiff(years, luv.years) %>% paste0("yr", .) 
    for (col in logical.cols) set(alldata.table, j = col, value = as.numeric(alldata.table[[col]]))
    
    # where na in alldata.table fill with 0
    for(j in seq_along(alldata.table)){
      set(alldata.table, which(is.na(alldata.table[[j]])), j, 0)
    }
    
    return(alldata.table)
  })
  
  # build structure type (sf/mf) indicators source table
  strdt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
    base.dir <- base.dir()
   
    stypedt <- NULL
    for (r in 1:length(runnames)){
      structure.files <- as.list(list.files(file.path(base.dir[r], "indicators"), 
                                            pattern = 'dataset_table__DU_and_HH_by_bld_type_by(_)*(\\w+)*(_)*(\\d+)*\\.tab'))
      if (length(structure.files) > 0){
        for (f in 1:length(structure.files)){
          geo <- str_extract(structure.files[f], "^(\\w+)__dataset") %>% strsplit("__") %>% unlist()
          yr <- str_extract(structure.files[f], "(\\d+)")  
          dt0 <- read.table(file.path(base.dir[r], 'indicators', structure.files[f]), header = T, sep = "\t", fill = TRUE) %>% as.data.table()
          setnames(dt0, colnames(dt0), str_match(colnames(dt0), "(\\w+_\\w+)[^_^\\d+]")[,1])
          setnames(dt0, colnames(dt0)[1], "name_id")
          dt <- melt.data.table(dt0, id.vars = colnames(dt0)[1], measure.vars = colnames(dt0)[2:ncol(dt0)], variable.name = "description", value.name = "estimate")
          dt[, `:=` (run = runs[r], geography = geo[1], year = yr, indicator = str_extract(description, "^\\w{2}"), type = str_match(description, "\\w+_(\\w+$)")[,2])]
          t0 <- dcast.data.table(dt, name_id + run + geography + year + indicator ~ type, value.var = 'estimate')
          ifelse(is.null(stypedt), stypedt <- t0, stypedt <- rbind(stypedt, t0))
        } # end structure.files loop
      } else if (length(structure.files) == 0) {
        next
      } # end conditional
    } # end runnames loop
    
    dt1 <- stypedt[, multifamily := MF + CO][, singlefamily := Total - multifamily]
    dt2 <- melt.data.table(dt1, id.vars = colnames(dt1)[1:5], measure.vars = colnames(dt1)[(ncol(dt1)-1):ncol(dt1)], variable.name = 'strtype', value.name = "estimate")
    ind.name <- c("HH" = "Households", "DU" = "Residential Units")
    dt2$indicator <- ind.name[dt2$indicator]
    
    return(dt2)
  })
  
  # build demographics indicators source table
  demogdt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
    base.dir <- base.dir()
  
    demog.indicators <- list(agegroup = "5year_age_groups__\\d+",
                             agegroup_intr = "age_groups_of_interest__\\d+",
                             dollargroup = "30_60_90_in_14dollars_groups__\\d+",
                             incomegroup = "new_14incomegroups__\\d+",
                             persontype = "pptyp__\\d+",
                             workertype = "pwtyp__\\d+")
    demog.table <- NULL
    table <- NULL

    for (r in 1:length(runnames)){
      for (d in 1:length(demog.indicators)){
        demog.files <- as.list(list.files(file.path(base.dir[r], "indicators"), pattern = paste0(demog.indicators[[d]], extension)))
        if (length(demog.files) > 0){
          for (f in 1:length(demog.files)){
            year <- str_match(demog.files[f] , "(\\d+){4}")[,1]
            datafile <- read.csv(file.path(base.dir[r], "indicators", demog.files[f]), header = TRUE, sep = ",")
            table <- transpose(datafile)
            names(table) <- "estimate"
            table$year <- year
            table$run <- runs[r]
            table$groups <- names(datafile)
            table$demographic <- names(demog.indicators[d])
            table <- table[2:nrow(table),]
            ifelse(is.null(table), demog.table <- table, demog.table <- rbind(demog.table, table))
          } # end of demog.files loop
        } else if (length(demog.files) == 0) {
          next
        } # end conditional
      } # end of demog.indicators loop
    } # end of runnames loop
    
    demog.table <- demog.table %>% as.data.table()
    
    my.dt <-NULL
    # loop through runs
    for (rn in runs){
      missing.yrs <- setdiff(years, demog.table[run == rn, year]) %>% as.vector
      if (length(missing.yrs) == 0) {
        next
      } else {
        for (y in 1:length(missing.yrs)){
          missyr.df <- demog.lookup
          missyr.df$year <- missing.yrs[y]
          missyr.df$run <- rn
          ifelse(is.null(my.dt), my.dt <- missyr.df, my.dt <- rbind(my.dt, missyr.df))
        }
      }
    }
    
    my.dt <- my.dt %>% as.data.table()
    
    #rbind tables
    demog.table <- rbindlist(list(demog.table, my.dt), use.names = TRUE, fill = TRUE)
    
    for(j in seq_along(demog.table)){
      set(demog.table, i = which(is.na(demog.table[[j]]) & is.numeric(demog.table[[j]])), j = j, value = 0)
    }
    
    ifelse(!is.null(demog.table), return(as.data.table(demog.table)), return(NULL))
    
  })
  
  # build max capacity source tables
  capdt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
    base.dir <- base.dir()
    
    cap.geography <- c(geography, "growth_center")
    cap.type <- c("max_dev", "max_dev_nonresidential", "max_dev_residential")
    
    cap.table <- NULL

    for (r in 1:length(runnames)){
      cap.files <- as.list(list.files(file.path(base.dir[r], "indicators"), pattern = paste0("max_dev(_\\w+)*", extension)))
      if (length(cap.files) >= 1){
        for (g in 1:length(cap.geography)){
          for (c in 1:length(cap.type)){
            cap.tbl <- NULL
            cap.file <- paste0(cap.geography[g], '__table__', cap.type[c], "_capacity", extension)
            cap.tbl <- read.csv(file.path(base.dir[r],"indicators", cap.file), header = TRUE, sep = ",")
            cap.tbl$captype <- switch(cap.type[c],
                                      "max_dev"="Total",
                                      "max_dev_nonresidential"="Non-Residential",
                                      "max_dev_residential"="Residential")
            cap.tbl$geography <- cap.geography[g]
            cap.tbl$year <- str_sub(names(cap.tbl)[2], -4)
            cap.tbl$run <- runs[r]
            colnames(cap.tbl)[1] <- "name_id"
            colnames(cap.tbl)[2] <- "capacity"
            ifelse(is.null(cap.table),
                   cap.table <- cap.tbl,
                   cap.table <- rbind(cap.table, cap.tbl))
          } # end of cap.type loop
        } # end of cap.geography loop
      } else if (length(cap.files) < 1){
        next
      } # end conditional
    } # end of runnames loop

    return(as.data.table(cap.table))
  })

  devdt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
    base.dir <- base.dir()
    
    cap.geography <- c(geography, "growth_center")
    dev.type <- c("residential_units", "building_sqft", "nonres_sqft")
    
    dev.dt <- NULL
    
    for (r in 1:length(runnames)){
      dev.files <- as.list(list.files(file.path(base.dir[r], "indicators"), pattern = paste0("sqft", extension)))
      if (length(dev.files) >= 1){
        for (g in 1:length(cap.geography)){
          for (d in 1:length(dev.type)){
            dev.tbl <- NULL
            dev.file <- paste0(cap.geography[g], '__table__', dev.type[d], extension)
            dev.tbl <- fread(file.path(base.dir[r],"indicators", dev.file), header = TRUE)
            dev.tbl.m <- melt(dev.tbl, id.vars = c(paste0(cap.geography[g], "_id")), measure.vars = names(dev.tbl)[2:ncol(dev.tbl)])
            dev.tbl.m[, `:=` (devtype = switch(dev.type[d],
                                               "residential_units" = "Residential Units",
                                               "building_sqft" = "Building Sqft",
                                               "nonres_sqft" = "Non-Residential Sqft"),
                              year = str_sub(variable, -4),
                              geography = cap.geography[g],
                              run = runs[r])]
            setnames(dev.tbl.m, paste0(cap.geography[g], "_id"), "name_id")
            setnames(dev.tbl.m, "value", "estimate")
            ifelse(is.null(dev.dt),
                   dev.dt <- dev.tbl.m,
                   dev.dt <- rbind(dev.dt, dev.tbl.m))
          } # end of dev.type loop
        } # end cap.geography loop
      } else if (length(dev.files) < 1){
        next
      } # end conditional
    } # end of runnames loop
    return(dev.dt)
  })
  
  # Build Jobs by Sector table
  jobsectdt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
    base.dir <- base.dir()
    
    sectorJobs.pat <- "city__dataset_table__employment_by_aggr_sector__\\d+"
    
    sectorJobs.table <- NULL
    for (r in 1:length(runnames)) {
      sectorJobs.file <- list.files(file.path(base.dir[r], "indicators"), pattern = paste0(sectorJobs.pat, ".tab"))
      for (f in 1:length(sectorJobs.file)){
        t <- read.csv(file.path(base.dir[r], "indicators", sectorJobs.file[f]), header = TRUE, sep = "\t") %>% as.data.table
        t1 <- t[, 1 := NULL][, lapply(.SD, sum)][, `:=` (run = runs[r], year = str_extract(sectorJobs.file[f], "\\d+"))]
        ifelse(is.null(sectorJobs.table), sectorJobs.table <- t1, sectorJobs.table  <- rbind(sectorJobs.table, t1))
      } # end sectorJobs.file loop
    } # end runnames loop

    my.dt <-NULL
    # create separate table for non-luv years
    for (rn in runs){
      missing.yrs <- setdiff(years, sectorJobs.table[run == rn, year]) %>% as.vector
      if (length(missing.yrs) == 0) {
        next
      } else {
        missyr.df <- data.frame(matrix(ncol = 2, nrow=length(missing.yrs)))
        colnames(missyr.df) <- c("run", "year")
        missyr.df$year <- missing.yrs
        missyr.df$run <- rn
        ifelse(is.null(my.dt), my.dt <- missyr.df, my.dt <- rbind(my.dt, missyr.df))
      }
    }

    if (!is.null(my.dt)) {
      my.dt[colnames(sectorJobs.table)[1:(ncol(sectorJobs.table)-2)]] <- 0
      my.dt <- my.dt %>% as.data.table()
      sectorJobs.table <- rbindlist(list(sectorJobs.table, my.dt), use.names = TRUE, fill = TRUE)
    }

    sj <- melt.data.table(sectorJobs.table, id.vars = c("run", "year"), measure.vars = colnames(sectorJobs.table)[1:(ncol(sectorJobs.table)-2)])
    setnames(sj, colnames(sj), c("run", "year", "sector", "estimate"))
    return(sj)
  })
  
  # Build Growth Centers table
  growctrdt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
    base.dir <- base.dir()
    
    # initialize growctr.table
    growctr.table <- data.frame(matrix(ncol = length(years) + 3, nrow = 0)) #30
    new.growctr.table.colnames <- c("name_id", paste0("yr", years), "indicator", "run") #seq(2014, 2040)
    colnames(growctr.table) <- new.growctr.table.colnames
    growctr.table <- growctr.table %>% as.data.table()
    
    for (r in 1:length(runnames)) {
      for (i in 1:length(attribute)){
        filename <- paste0('growth_center__table','__',attribute[i], extension)
        datatable <- read.csv(file.path(base.dir[r],"indicators",filename), header = TRUE, sep = ",")
        colnames(datatable)[2:ncol(datatable)] <- str_replace(colnames(datatable)[2: ncol(datatable)], '\\w+_', 'yr') # rename columns
        colnames(datatable)[1] <- str_replace(colnames(datatable)[1], '\\w+_', 'name_')
        datatable$indicator <- switch(attribute[i],
                                      "population"="Total Population",
                                      "households"="Households",
                                      "employment"="Employment",
                                      "residential_units"="Residential Units")
        
        
        datatable$run <- runs[r]
        datatable <- datatable %>% as.data.table()
        growctr.table <- rbindlist(list(growctr.table, datatable), use.names = TRUE, fill = TRUE)
      } # end of attribute loop
    } # end of runnames loop
    
    rgc.lookup1 <- rgc.lookup[,c("growth_center_id", "name")]
    gc.table <- merge(growctr.table, rgc.lookup1, by.x = "name_id", by.y = "growth_center_id")
    
    for(j in seq_along(gc.table)){
      set(gc.table, i = which(is.na(gc.table[[j]]) & is.numeric(gc.table[[j]])), j = j, value = 0)
    }
    
    return(gc.table)

  })
  
  # Build vacancy estimates/rates table
  vacdt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
    base.dir <- base.dir()
    
    vac.table <- NULL
    dtype <- c("rate","estimate")
    
    for (d in 1:length(dtype)) {
      for (r in 1:length(runnames)){
        p <- switch(dtype[d], "rate" = "(\\w+_)*eoy_vacancy(_\\w+)*", "estimate" = "(\\w+_)*units_and_nonres_sqft(.)*")
        vac.files <- list.files(file.path(base.dir[r], "indicators"), pattern = paste0(p, extension))
        if (length(vac.files) > 0) {
          for (v in 1:length(vac.files)) {
            vac.tbl <- NULL
            vac.file <- NULL
            vac.file <- vac.files[v]
            vac.tbl <- read.csv(file.path(base.dir[r],"indicators", vac.file), header = TRUE, sep = ",") %>% as.data.table()
            if (dtype[d] == "estimate") {
              vac.tbl.col <- colnames(vac.tbl)[grep(".*units$|.*sqft$", colnames(vac.tbl))]
              vac.tbl <- vac.tbl[, c("county_id", eval(vac.tbl.col)), with = FALSE]
            } 
            vac.tbl1 <- melt.data.table(vac.tbl, id.vars = 1, measure.vars = c(2:ncol(vac.tbl)), variable.name = "building_type", value.name = "value")
            vac.tbl1 <- vac.tbl1[, `:=`(year = str_extract(vac.file, "\\d+"), type = dtype[d], run = runs[r])]
            ifelse(is.null(vac.table), vac.table <- vac.tbl1, vac.table <- rbind(vac.table, vac.tbl1))
          } # end vac.files loop
        } else {
          next
        } # end if/else
      } # end dtype
    } # end runnames
    
    colstr <- str_extract(vac.table$building_type, "(\\w+_\\d+)")
    vac.table$building_type <- colstr
    vac.table$county_id <- as.factor(vac.table$county_id)
    return(vac.table)
  })
  
# Data Ready Message ------------------------------------------------------

  
  output$submit_msg <- renderText({
    if (vars$submitted == TRUE) {
      "Data has been loaded, click on Index link or dashboard tabs"
    } else {
      return(NULL)
    }
  })
  
  # Delete temporary sub-directory in 'www' when session ends 
  session$onSessionEnded(function() {
    unlink(subdir, recursive=TRUE)
  })
  

# Topsheet Reactions and Rendering ----------------------------------------


  output$ts_currRun <- renderText({
    paste("<b>Current Run:</b>", selectRun1())
  })
  
  output$ts_desc <- renderText({
    base.dir <- base.dir()
    filename <- "Description.txt"
    desc.file <- readLines(file.path(base.dir[1],"indicators", filename), warn = FALSE)
    paste("<b>Description</b>:", desc.file)
  })
  
  output$ts_rest <- renderText({
    base.dir <- base.dir()
    filename <- "Restrictions.txt"
    desc.file <- readLines(file.path(base.dir[1],"indicators", filename), warn = FALSE)
    paste("<b>Restrictions</b>:", desc.file)
  })
  
  tsYear <- reactive({
    input$ts_select_year
  })
  
  # Filter table and calculate regional totals for general all-data-table
  tsTable <- reactive({
    alldt <- alldt()
    runs <- runs()
    tsYear <- tsYear()
    sel.yrs.col <- paste0("yr", c(years[1], tsYear))
    
    t <- merge(alldt[geography == 'zone' & (run == runs[1] | run == runs[2])], zonecnty.lookup, by.x = "name_id", by.y = "TAZ")
    t1 <- t[, lapply(.SD, sum), by = list(County = COUNTY_NM, indicator, run), .SDcols = sel.yrs.col]
    t.sum <- t1[, lapply(.SD, sum), by = list(indicator, run), .SDcols = sel.yrs.col][, County := "Sub-Total: Region"]
    rbindlist(list(t1, t.sum), use.names = TRUE)
  })
  
  # Display households summary table
  output$tpsht_hh <- DT::renderDataTable({
    tsTable <- tsTable()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    
    t <- tsTable[indicator == 'Households']
    t1 <- create.tsTable(t, "County") %>% select(1:3, 10:12, 4, 13:15, 5:6)
    sketch <- sketch.basic(colnames(t1)[1],  sel.yr.fl[1],  sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic(t1, sketch)
  })
  
  # Display population summary table
  output$tpsht_pop <- DT::renderDataTable({
    tsTable <- tsTable()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    
    t <- tsTable[indicator == 'Total Population']
    t1 <- create.tsTable(t, "County") %>% select(1:3, 10:12, 4, 13:15, 5:6)
    sketch <- sketch.basic(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic(t1, sketch)
  })
  
  # Display employment summary table
  output$tpsht_emp <- DT::renderDataTable({
    tsTable <- tsTable()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    
    t <- tsTable[indicator == 'Employment']
    t1 <- create.tsTable(t, "County") %>% select(1:3, 10:12, 4, 13:15, 5:6)
    sketch <- sketch.basic(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic(t1, sketch)
  })
  
  # Filter table and calculate totals for Jobs by Sector table
  tsSectorJobs <- reactive({
    jobsectdt <- jobsectdt()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)

    t <- jobsectdt[(run == runs[1] | run == runs[2]) & (year %in% sel.yr.fl)]
    t.sum <- t[, .(estimate = sum(estimate)), by = list(run, year)][, sector := "Sub-Total: Jobs"]
    rbindlist(list(t, t.sum), use.names = TRUE)
  })

  # Display Jobs by sector summary table
  output$tpsht_jobs <- DT::renderDataTable({
    tsSectorJobs <- tsSectorJobs()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)

    t <- dcast.data.table(tsSectorJobs, sector ~ year + run, value.var = "estimate")
    setnames(t, "sector", "Sector")
    setcolorder(t, c("Sector", paste0(sel.yr.fl[1],"_",runs[1]), paste0(sel.yr.fl[2],"_",runs[1]), paste0(sel.yr.fl[2],"_",runs[2]), paste0(sel.yr.fl[1],"_",runs[2])))
    t[, ncol(t) := NULL]
    t <- calc.cols.tsTable(t, sel.yr.fl)
    setcolorder(t, c("Sector",
                     paste0(sel.yr.fl[1], "_", runs[1]),
                     paste0(sel.yr.fl[2], "_", runs[1]),
                     "r1.baseyr",
                     "r1.baseyr.per",
                     "r1.avgann",
                     paste0(sel.yr.fl[2], "_", runs[2]),
                     "r2.baseyr",
                     "r2.baseyr.per",
                     "r2.avgann",
                     "Change",
                     "Per.Change"))
    t1 <- t[, c(2:4, 7:8) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = c(2:4, 7:8)]
    sketch <- sketch.basic(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic(t1, sketch)
  })
  
  # Filter table and calculate totals for PWTYPE
  tsPwtypeTable <- reactive({
    demogdt <- demogdt()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    
    t <- demogdt[demographic == 'workertype' & (run == runs[1] | run == runs[2]) & (year %in% sel.yr.fl)
                 ][, lapply(.SD, sum), by = list(Group = groups, run, year), .SDcols = "estimate"]
    t.sum <- t[, .(estimate = sum(estimate)), by = list(run, year)][, Group := "Sub-Total: Persons"]
    rbindlist(list(t, t.sum), use.names = TRUE)
  })
  
  # Display PWTYPE summary table
  output$tpsht_pwtype <- DT::renderDataTable({
    tsPwtypeTable <- tsPwtypeTable()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    newOrder <- c("full_time", "part_time", "non_workers_no_job", "workers_no_job", "Sub-Total: Persons")
    
    if (length(unique(tsPwtypeTable$run)) == 2) {
      pt <- dcast.data.table(tsPwtypeTable, Group ~ year + run, value.var = "estimate")
      setcolorder(pt, c("Group", paste0(sel.yr.fl[1],"_",runs[1]), paste0(sel.yr.fl[2],"_",runs[1]), paste0(sel.yr.fl[2],"_",runs[2]), paste0(sel.yr.fl[1],"_",runs[2])))
      pt[, ncol(pt) := NULL]
      pt <- calc.cols.tsTable(pt, sel.yr.fl)
      pt <- pt[ , name := factor(Group, levels = newOrder)]
      t0 <- pt[with(pt, order(name)),]
      t <- t0[, -"name", with = FALSE]
      setcolorder(t, c("Group",
                       paste0(sel.yr.fl[1], "_", runs[1]),
                       paste0(sel.yr.fl[2], "_", runs[1]),
                       "r1.baseyr",
                       "r1.baseyr.per",
                       "r1.avgann",
                       paste0(sel.yr.fl[2], "_", runs[2]),
                       "r2.baseyr",
                       "r2.baseyr.per",
                       "r2.avgann",
                       "Change",
                       "Per.Change"
      ))
      t1 <- t[, c(2:4,7:8) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = c(2:4,7:8)]
    } else {
      pt <- dcast.data.table(tsPwtypeTable, Group ~ year + run, value.var = "estimate")
      pt[ , name := factor(Group, levels = newOrder)]
      t0 <- pt[with(pt, order(name)),]
      t <- t0[, -"name", with = FALSE]
      t1 <- t[, 2:ncol(t) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = 2:ncol(t)]
    }
    
    sketch <- sketch.basic(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic(t1, sketch)
  })
  
  # Filter table and calculate totals for PTYPE
  tsPtypeTable <- reactive({
    demogdt <- demogdt()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    
    t <- demogdt[demographic == 'persontype' & (run == runs[1] | run == runs[2]) & (year %in% sel.yr.fl)
                 ][, lapply(.SD, sum), by = list(Group = groups, run, year), .SDcols = "estimate"]
    t.sum <- t[, .(estimate = sum(estimate)), by = list(run, year)][, Group := "Sub-Total: Persons"]
    rbindlist(list(t, t.sum), use.names = TRUE)
  })
  
  # Display PTYPE summary table
  output$tpsht_ptype <- DT::renderDataTable({
    tsPtypeTable <- tsPtypeTable()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    newOrder <- c("full_time_worker", "part_time_worker", "non_working_adult_age_65_plus", "non_working_adult_age_16_64", 
      "university_student" , "hs_student_age_15_up" , "child_age_5_15" , "child_age_0_4" , "Sub-Total: Persons" )
    
    pt <- dcast.data.table(tsPtypeTable, Group ~ year + run, value.var = "estimate")
    setcolorder(pt, c("Group", paste0(sel.yr.fl[1],"_",runs[1]), paste0(sel.yr.fl[2],"_",runs[1]), paste0(sel.yr.fl[2],"_",runs[2]), paste0(sel.yr.fl[1],"_",runs[2])))
    pt[, ncol(pt) := NULL]
    pt <- calc.cols.tsTable(pt, sel.yr.fl)
    pt <- pt[ , name := factor(Group, levels = newOrder)]
    t0 <- pt[with(pt, order(name)),]
    t <- t0[, -"name", with = FALSE]
    setcolorder(t, c("Group",
                     paste0(sel.yr.fl[1], "_", runs[1]),
                     paste0(sel.yr.fl[2], "_", runs[1]),
                     "r1.baseyr",
                     "r1.baseyr.per",
                     "r1.avgann",
                     paste0(sel.yr.fl[2], "_", runs[2]),
                     "r2.baseyr",
                     "r2.baseyr.per",
                     "r2.avgann",
                     "Change",
                     "Per.Change"
    ))
    t1 <- t[, c(2:4,7:8) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = c(2:4,7:8)]
    
    sketch <- sketch.basic(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic(t1, sketch)
  })
  
  # Filter table and calculate totals for Income summary table
  tsIncTable <- reactive({
    demogdt <- demogdt()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)

    t <- demogdt[demographic == 'incomegroup' & (run == runs[1] | run == runs[2]) & (year %in% sel.yr.fl)
                 ][, lapply(.SD, sum), by = list(Group = groups, run, year), .SDcols = "estimate"]
    t.sum <- t[, .(estimate = sum(estimate)), by = list(run, year)][, Group := "Sub-Total: Households"]
    rbindlist(list(t, t.sum), use.names = TRUE)
  })
  
  # Display Households by Income summary table
  output$tpsht_hhInc <- DT::renderDataTable({
    tsIncTable <- tsIncTable()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    
    t <- dcast.data.table(tsIncTable, Group ~ year + run, value.var = "estimate")
    setcolorder(t, c("Group", paste0(sel.yr.fl[1],"_",runs[1]), paste0(sel.yr.fl[2],"_",runs[1]), paste0(sel.yr.fl[2],"_",runs[2]), paste0(sel.yr.fl[1],"_",runs[2])))
    t[, ncol(t) := NULL]
    t <- calc.cols.tsTable(t, sel.yr.fl)
    setcolorder(t, c("Group",
                     paste0(sel.yr.fl[1], "_", runs[1]),
                     paste0(sel.yr.fl[2], "_", runs[1]),
                     "r1.baseyr",
                     "r1.baseyr.per",
                     "r1.avgann",
                     paste0(sel.yr.fl[2], "_", runs[2]),
                     "r2.baseyr",
                     "r2.baseyr.per",
                     "r2.avgann",
                     "Change",
                     "Per.Change"
    ))
    t1 <- t[, c(2:4,7:8) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = c(2:4,7:8)]
    sketch <- sketch.basic(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic(t1, sketch)
  })
  
  # Filter table and calculate totals for largest RGCs
  tsGrowthCtr <- reactive({
    growctrdt <- growctrdt()
    runs <- runs()
    tsYear <- tsYear()
    sel.yrs.col <- paste0("yr", c(years[1], tsYear))
    
    t <- growctrdt[(indicator == 'Total Population' | indicator == 'Employment') & (run == runs[1] | run == runs[2])]
    t[indicator == 'Total Population', indicator := 'Population']
    t1 <- t[, lapply(.SD, sum), by = list(name, indicator, run), .SDcols = sel.yrs.col]
  })
  
  # Display largest RGCs summary table
  output$tpsht_rgc <- DT::renderDataTable({
    tsGrowthCtr <- tsGrowthCtr()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    sel.yrs.col <- paste0("yr", c(years[1], tsYear))
    lg.rgc <- c("Bellevue", "Everett", "SeaTac", "Seattle Downtown", "Seattle First Hill/Capitol Hill", "Seattle South Lake Union",
                "Seattle University Community", "Tacoma Downtown")

    t0 <- dcast.data.table(tsGrowthCtr, name ~ indicator + run, value.var = sel.yrs.col)
    t <- t0[name %in% lg.rgc,]
    setnames(t, "name", "Name")
    t1 <- create.exp.tsTable(t)
    
    sketch <- sketch.expanded(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.expanded(t1, sketch)
  })
  
  # Filter table and calculate totals for Special Places
  tsSplace <- reactive({
    alldt <- alldt()
    runs <- runs()
    tsYear <- tsYear()
    sel.yrs.col <- paste0("yr", c(years[1], tsYear))
    
    key.loc <- c("UW", "Amazon", "SeaTac Airport", "Microsoft Overlake", "Paine Field", "JBLM", "Bangor")
    t <- merge(alldt[geography == 'zone' & (run == runs[1] | run == runs[2]) & (indicator == 'Total Population' | indicator == 'Employment')], 
               splaces.lookup, by.x = "name_id", by.y = "zone_id")
    t1 <- t[, lapply(.SD, sum), by = list(Name = name, indicator, run), .SDcols = sel.yrs.col][Name %in% key.loc, ]
    t1[indicator == "Total Population", indicator := "Population"]
  })
  
  # Display Special Places summary table
  output$tpsht_splace <- DT::renderDataTable({
    tsSplace <- tsSplace()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    sel.yrs.col <- paste0("yr", c(years[1], tsYear))

    t <- dcast.data.table(tsSplace, Name ~ indicator + run, value.var = sel.yrs.col)
    t1 <- create.exp.tsTable(t)
    sketch <- sketch.expanded(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.expanded(t1, sketch)
  })
  


# Growth Topsheet Reactions and Rendering ---------------------------------
  
  
  # Display households summary table
  output$g_tpsht_hh <- DT::renderDataTable({
    tsTable <- tsTable()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    
    t <- tsTable[indicator == 'Households']
    t1 <- create.tsTable(t, "County") %>% select(1:3, 7, 4, 8, 5:6, 9)
    sketch <- sketch.basic.growth(colnames(t1)[1],  sel.yr.fl[1],  sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic.growth(t1, sketch) 

  })

  # Display population summary table
  output$g_tpsht_pop <- DT::renderDataTable({
    tsTable <- tsTable()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    
    t <- tsTable[indicator == 'Total Population']
    t1 <- create.tsTable(t, "County") %>% select(1:3, 7, 4, 8, 5:6, 9) 
    sketch <- sketch.basic.growth(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic.growth(t1, sketch) 
  })
  
  # Display employment summary table
  output$g_tpsht_emp <- DT::renderDataTable({
    tsTable <- tsTable()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    
    t <- tsTable[indicator == 'Employment']
    t1 <- create.tsTable(t, "County") %>% select(1:3, 7, 4, 8, 5:6, 9)
    sketch <- sketch.basic.growth(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic.growth(t1, sketch)
  })
  
  # Display population by RGC summary table
  output$g_tpsht_rgc_pop <- DT::renderDataTable({
    tsGrowthCtr <- tsGrowthCtr()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    sel.yrs.col <- paste0("yr", c(years[1], tsYear))
    
    mics <- rgc.lookup %>% filter(growth_center_id >= 600) %>% select(name) %>% as.data.table()
    
    t0 <- tsGrowthCtr[indicator == 'Population']
    t0 <- setDT(t0)[!mics, on = "name"]
    t.sum <- t0[, lapply(.SD, sum), by = list(indicator, run), .SDcols = sel.yrs.col][, name := "Sub-Total: All RGCs"]
    t <- rbindlist(list(t0, t.sum), use.names = TRUE)
    t1 <- create.tsTable(t, "name") %>% select(1:3, 7, 4, 8, 5:6, 9)
    setnames(t1, "name", "Name")
    sketch <- sketch.basic.growth(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic.growth(t1, sketch)
  })
  
  # Display employment by RGC summary table
  output$g_tpsht_rgc_emp <- DT::renderDataTable({
    tsGrowthCtr <- tsGrowthCtr()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    sel.yrs.col <- paste0("yr", c(years[1], tsYear))
    
    mics <- rgc.lookup %>% filter(growth_center_id >= 600) %>% select(name) %>% as.data.table()
    
    t0 <- tsGrowthCtr[indicator == 'Employment']
    t0 <- setDT(t0)[!mics, on = "name"]
    t.sum <- t0[, lapply(.SD, sum), by = list(indicator, run), .SDcols = sel.yrs.col][, name := "Sub-Total: All RGCs"]
    t <- rbindlist(list(t0, t.sum), use.names = TRUE)
    t1 <- create.tsTable(t, "name") %>% select(1:3, 7, 4, 8, 5:6, 9)
    setnames(t1, "name", "Name")
    sketch <- sketch.basic.growth(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic.growth(t1, sketch)
  })  
  
  # Display Population by Special Places summary table
  output$g_tpsht_splace_pop <- DT::renderDataTable({
    tsSplace <- tsSplace()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    sel.yrs.col <- paste0("yr", c(years[1], tsYear))
    
    t0 <- tsSplace[indicator == 'Population']
    t.sum <- t0[, lapply(.SD, sum), by = list(indicator, run), .SDcols = sel.yrs.col][, Name := "Sub-Total: Key Locations"]
    t <- rbindlist(list(t0, t.sum), use.names = TRUE)
    t1 <- create.tsTable(t, "Name") %>% select(1:3, 7, 4, 8, 5:6, 9)
    sketch <- sketch.basic.growth(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic.growth(t1, sketch)
  })
  
  # Display Employment by Special Places summary table
  output$g_tpsht_splace_emp <- DT::renderDataTable({
    tsSplace <- tsSplace()
    runs <- runs()
    tsYear <- tsYear()
    sel.yr.fl <- c(years[1], tsYear)
    sel.yrs.col <- paste0("yr", c(years[1], tsYear))
    
    t0 <- tsSplace[indicator == 'Employment']
    t.sum <- t0[, lapply(.SD, sum), by = list(indicator, run), .SDcols = sel.yrs.col][, Name := "Sub-Total: Key Locations"]
    t <- rbindlist(list(t0, t.sum), use.names = TRUE)
    t1 <- create.tsTable(t, "Name") %>% select(1:3, 7, 4, 8, 5:6, 9)
    sketch <- sketch.basic.growth(colnames(t1)[1], sel.yr.fl[1], sel.yr.fl[2], runs[1], runs[2])
    create.DT.basic.growth(t1, sketch)
  })
  
# Run Comparison Reactions ------------------------------------------------

  
  # determine if all years available, and update available years in Run Comparison UI
  observe({
    alldt <- alldt()

    dt1 <- alldt[(run == runname1() | run == cRun()), c("run", addn.yrs), with = FALSE]
    dt2 <- dt1[, lapply(.SD, sum), by=run, .SDcols= addn.yrs]
    dt3 <- dt2[, sumdt := rowSums(.SD), .SDcols = 2:ncol(dt2)][, .(run, sumdt)]
    luv.yr.only <- nrow(dt3[dt3$sumdt == 0])
    
    if (luv.yr.only > 0) {
      updateSelectInput(session,
                        "compare_select_year",
                        choices = luv.years,
                        selected = tail(luv.years, 1))
    }
  })
  
  output$compare_select_run2_ui <- renderUI({
    runname1 <- runname1()
    runnames2 <- runnames2()
    selectInput(inputId = "compare_select_run2",
                label = h4(paste0("Compare ",`runname1`," with")),
                choices = runnames2
    )
  })
  
  cRun <- reactive({
    input$compare_select_run2
  })
  
  # Check if runs 1 & 2 exist in strdt(), if not conditional panel disabled
  output$strdtavail <- reactive({
    strdt <- strdt()
    c1 <- runname1() %in% strdt[, run]
    c2 <- cRun() %in% strdt[, run]
    v <- c1 == c2
    return(v)
    
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
  
  cBaseYear <- reactive({
    paste0("yr", years[1])
  })
  
  cStructureType <- reactive({
    switch(as.integer(input$compare_structure_type),
           "All",
           "singlefamily",
           "multifamily")
  })

  cTable <- reactive({
    strdt <- strdt()
    alldt <- alldt()
    
    if (is.null(cStructureType()) | cStructureType() == "All" | (cIndicator() %in% c("Total Population", "Employment"))){
      dt1 <- alldt[run == runname1() & geography == cGeog() & indicator == cIndicator(),
                   .(name_id, geography, indicator, get(cBaseYear()), get(cYear()))]
      setnames(dt1, dt1[,c((ncol(dt1)-1), ncol(dt1))], c('baseyr', 'estrun1'))
      dt2 <- alldt[run == cRun() & geography == cGeog() & indicator == cIndicator(),
                   .(name_id, get(cYear()))]
      setnames(dt2, dt2[,ncol(dt2)], 'estrun2')
      dt <- merge(dt1, dt2, by = 'name_id')
    } else {
      dt1 <- strdt[run == runname1() & geography == cGeog() & (year == year[1] | year == input$compare_select_year) & indicator == cIndicator() & strtype == cStructureType()]
      dt1.cast <- dcast.data.table(dt1, name_id + indicator + geography ~ year, value.var = "estimate")
      setnames(dt1.cast, colnames(dt1.cast)[4:5], c('baseyr', 'estrun1'))
      dt2 <- strdt[run == cRun() & geography == cGeog() & year == input$compare_select_year & indicator == cIndicator() & strtype == cStructureType()]
      dt2.cast <- dcast.data.table(dt2, name_id ~ year, value.var = "estimate")
      setnames(dt2.cast, colnames(dt2.cast)[2], 'estrun2')
      dt <- merge(dt1.cast, dt2.cast, by = 'name_id')
    }
    dt[,"diff" := (estrun1-estrun2)]
    switch(as.integer(input$compare_select_geography),
           merge(dt, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
           merge(dt, faz.lookup, by.x = "name_id", by.y = "faz_id"),
           merge(dt, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name")
    )
  })
  
  # shapefile ready for visualization
  cShape <- reactive({
    joinShp2Tbl(input$compare_select_geography, cTable())
  })

  # leaflet layer control
  cGeo <- reactive({
    switch(as.integer(input$compare_select_geography),
           "TAZ",
           "FAZ",
           "City"
    )
  })


# Run Comparison Rendering ------------------------------------------------


  # Plotly
  output$compare_plot <- renderPlotly({
    if(!vars$submitted) return(NULL)
    if (is.null(cRun())) return(NULL)
    runname2.trim <- sapply(strsplit(cRun(),"[.]"), function(x) x[1])
    ctable <- cTable()
    scatterplot(ctable, "compare", ctable$estrun1, ctable$estrun2, runname1(), runname2.trim)
  })
  
  output$compare_dt <- renderDataTable({
    if(!vars$submitted) return(NULL)
    if (is.null(cRun())) return(NULL)
    runname2.trim <- sapply(strsplit(cRun(),"[.]"), function(x) x[1])
    select.year <- str_extract(cYear(), "\\d+")
    
    ctable0 <- cTable()
    ctable <- ctable0[, .(County, indicator, geography, name_id, Name, baseyr, estrun1, estrun2, diff)]
    setnames(ctable, c("County", "Indicator", "Geography", "ID", "Name",
                         paste0("Baseyear_", years[1]), 
                         paste0(runname1(), "_", select.year), 
                         paste0(runname2.trim, "_", select.year), 
                         "Difference"))
    create.DT.generic(ctable) 
  })
  
  # Leaflet
  output$compare_map <- renderLeaflet({
    if(!vars$submitted) return(NULL)
    cshape <- cShape()
    if (is.null(cRun()) || is.null(cshape$diff)) return(NULL)
    runname2.trim <- sapply(strsplit(cRun(),"[.]"), function(x) x[1])
    
    # Set up symbology and categorization
    colorBinResult <- map.colorBins(cshape$diff, input$compare_select_geography)
    pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=cshape$diff, pretty = FALSE)
    
    # popup setup
    cgeo <- cGeo()
    geo.popup1 <- c.map.shp.popup(cshape, 'baseyr', 'estrun1','estrun2', cgeo, runname1(), runname2.trim)
    geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)
    
    # Draw the map without selected geographies
    map <- map.layers(cshape, cgeo, paste0("Run difference by ", cgeo), geo.popup1, geo.popup3, pal)
    
    # Re-draw the map with selected geographies
    # Drag event for the scatterplot: will grab ids of selected points
    subdata <- select.items("compare", cshape)
    if(length(subdata) > 0)
      map <- map %>% addSelectedGeo(subdata, cgeo) %>% map.settings(cgeo)
    
    map
  })
  

# Growth Reactions --------------------------------------------------------

  
  # determine if all years available, and update available years in Years Slider
  observe({
    input$growth_select_year
    alldt <- alldt()
    
    gdt1 <- alldt[run == gRun(), c("run", addn.yrs), with = FALSE]
    gdt2 <- gdt1[, lapply(.SD, sum), by=run, .SDcols= addn.yrs]
    gdt3 <- gdt2[, gsumdt := rowSums(.SD), .SDcols = 2:ncol(gdt2)][, .(run, gsumdt)]
    gluv.yr.only <- nrow(gdt3[gdt3$gsumdt == 0])
    
    isolate({
      if (gluv.yr.only != 0){
        l <- floor(as.numeric(input$growth_select_year[1])/5)*5
        if (l == 2010) l <- 2014
        h <- ceiling(as.numeric(input$growth_select_year[2])/5)*5
        updateSliderInput(session,
                          "growth_select_year",
                          value = c(l, h))
      }
    })
  })
  
   output$growth_select_run_ui <- renderUI({
     runs <- runs()
     selectInput(inputId = "growth_select_run",
                 label = "Run",
                 choices = runs
     )
   })
  
   gRun <- reactive({
     input$growth_select_run
   })
   
   # Check if selected run exist in strdt()
   gRunInStrdt <- reactive({
     strdt <- strdt()
     v <- input$growth_select_run %in% strdt[, run]
     return(v)
   })
   
   # Check if selected run exist in strdt(), if not conditional panel disabled
   output$gstrdtavail <- reactive({
     strdt <- strdt()
     v <- input$growth_select_run %in% strdt[, run]
     return(v)
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
   
   gYear0 <- reactive({ # vector of two numbers, to query strdt() and for labeling
     input$growth_select_year
   })
  
   gYear <- reactive({ # vector of two numbers, to query alldt()
     paste0("yr", input$growth_select_year)
   })

   gYear.label1 <- reactive({
     gYear0 <- gYear0()
     gYear0[1]
   })

   gYear.label2 <- reactive({
     gYear0 <- gYear0()
     gYear0[2]
   })
   
   gStructureType <- reactive({
     switch(as.integer(input$growth_structure_type),
            "All",
            "singlefamily",
            "multifamily")
   })
   
   gTable <- reactive({
     if (is.null(gRun()) || is.null(input$growth_select_geography) || is.null(gYear())) return(NULL)
     strdt <- strdt()
     alldt <- alldt()
     gYear <- gYear()
     gYear0 <- gYear0()
     
     if (is.null(gStructureType()) || gStructureType() == "All" || gRunInStrdt() == FALSE || (gIndicator() %in% c("Total Population", "Employment"))){
       dt <- alldt[run == gRun() & geography == gGeog() & indicator == gIndicator(),
                   .(name_id, geography, run, indicator, get(gYear[1]), get(gYear[2]))]
       setnames(dt, c(dt[,ncol(dt)-1], dt[,ncol(dt)]), c('yr1', 'yr2'))
     } else {
       dt1 <- strdt[run == gRun() & geography == gGeog() & indicator == gIndicator() & strtype == gStructureType() & (year == gYear0[1] | year == gYear0[2]),
                   .(name_id, geography, run, indicator, strtype, year, estimate)]
       
       dt <- dcast.data.table(dt1, name_id + geography + run + indicator ~ year, value.var = "estimate")
       setnames(dt, colnames(dt)[(ncol(dt)-1):ncol(dt)], c('yr1', 'yr2'))
     }  
     dt[,"diff" := (yr2-yr1)]
     switch(as.integer(input$growth_select_geography),
       merge(dt, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
       merge(dt, faz.lookup, by.x = "name_id", by.y = "faz_id"),
       merge(dt, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name")
       )
   })

   # shapefile ready for visualization
   gShape <- reactive({
     joinShp2Tbl(input$growth_select_geography, gTable())
   })

   # leaflet layer control
   geo <- reactive({
     switch(as.integer(input$growth_select_geography),
            "TAZ",
            "FAZ",
            "City"
     )
   })
  

# Growth Rendering --------------------------------------------------------


  # Plotly
  output$growth_plot <- renderPlotly({
    if(!vars$submitted) return(NULL)
    gtable <- gTable()
    if (is.null(gtable)| all(gtable$yr1 == 0) | all(gtable$yr2 == 0)) return(NULL)
    scatterplot(gtable, "growth", gtable$yr1, gtable$yr2, gYear.label1(), gYear.label2())
  })
   
  output$growth_dt <- renderDataTable({
    if(!vars$submitted) return(NULL)
    gtable0 <- gTable()
    if (is.null(gtable0)| all(gtable0$yr1 == 0) | all(gtable0$yr2 == 0)) return(NULL)
    gtable <- gtable0[, .(run, County, indicator, geography, name_id, Name, yr1, yr2, diff)]
    setnames(gtable, c("Run", "County", "Indicator", "Geography", "ID", "Name",
                        gYear.label1(),
                        gYear.label2(),
                        "Growth"))
    create.DT.generic(gtable) 
  })

  # Leaflet
  output$growth_map <- renderLeaflet({
    if(!vars$submitted) return(NULL)
    gshape <- gShape()
    if (is.null(gshape$diff) | all(gshape$yr1 == 0) | all(gshape$yr2 == 0)) return(NULL)
    # Set up symbology and categorization
    colorBinResult <- map.colorBins(gshape$diff, input$growth_select_geography)
    pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=gshape$diff, pretty = FALSE)

    # popup setup
    geo.popup1 <- map.shp.popup(gshape,'yr1','yr2', geo(), gYear.label1(), gYear.label2())
    geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)

    # Draw the map without selected geographies
    geo <- geo()
    map <- map.layers(gshape, geo, paste0(gYear.label1(), "-", gYear.label2(), " growth by ", geo), geo.popup1, geo.popup3, pal)

    # Re-draw the map with selected geographies
    # Drag event for the scatterplot: will grab ids of selected points
    subdata <- select.items("growth", gshape)
    if(length(subdata) > 0)
      map <- map %>% addSelectedGeo(subdata, geo) %>% map.settings(geo)

    map
 })


# Employment by Sector Reactions and Rendering ----------------------------


  empGeog <- reactive({
    switch(as.integer(input$emp_display),
           file.path(trim.subdir, "qc_ts_emp_cnty.html"),
           file.path(trim.subdir, "qc_ts_emp_sp.html"))
  })
  
  output$empplots <- renderText({
    if(!vars$submitted) return(NULL)
    t <- paste0('<iframe height=2500 width=2500 frameBorder=0 seamless="seamless" scrolling="yes" src="', empGeog(),'">')
    return(t)
  })


# Time Series Reactions and Rendering -------------------------------------

  lgarea <- list("EastsideKing_1","EastsideKing_2","GreenRiver","SeattleandShoreline","SEKingandKingOther",
                 "SWKing","Central,North,andSouthKitsap","PeninsulaandTacoma","PierceOther_1","PierceOther_2",
                 "SWPierce","Everett","NWSnohomish","SnohomishOther","SWSnohomish_1","SWSnohomish_2")

  tsSelected_plot <- reactive({
    plot <- lgarea[[as.integer(input$select_tsplots)]]
    file <- remove.spaces(paste0(file.path(trim.subdir, 'qc_ts_city_'), plot,'.html'))
    return(file)
  })

  output$tsplots <- renderText({
    if(!vars$submitted) return(NULL)
    t <- paste0('<iframe height=5000 width=2000 frameBorder=0 seamless="seamless" scrolling="yes" src="', tsSelected_plot(),'">')
    return(t)
    })



# Demographic Indicators Reactions and Rendering --------------------------


    # Display graphs or text depending if demographic indicators exist
  output$condDemog_Plot <- renderUI({
    if (is.null(demogdt())){
      verbatimTextOutput("demog_plot_test")
    } else {
      plotlyOutput("demog_plot", height = "850px")
    }
  })

  output$demog_plot_test <- renderText({
    "Demographic indicators have not yet been generated for any of your selected runs"
  })

  # Returning a list of runs with demographic indicators or hide if not
  output$demog_Runs <- renderUI({
    if (is.null(demogdt())) return(NULL)
    demogdt <- demogdt()
    select.runs <- unique(demogdt[demographic == dDemographic(), run])
    selectInput(inputId = "demog_select_run",
                label = "Run",
                choices = select.runs)
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
    demogdt <- demogdt()
    drun <- dRun()
    if (is.null(demogdt) || is.null(drun)) return(NULL)
    ddemog <- dDemographic()
    if (input$demog_select_format == 1){
      demogdt[run == drun & demographic == ddemog,]
    } else if (input$demog_select_format == 2){
      main <- demogdt[run == drun & demographic == ddemog,]
      region.totals <- demogdt[run == drun & demographic == ddemog, .(total = sum(estimate)), by = year]
      setkey(main, year)[region.totals, percent := round((estimate/total)*100, 2)]
      dt <- main[,.(percent, year, run, groups, demographic)]
      setnames(dt, "percent", "estimate")
      return(dt)
    }
  })

  # Build bar charts
  output$demog_plot <- renderPlotly({
    dtable <- dTable()
    if (is.null(dtable)) return(NULL)
    
    dat2 <- subset(as.data.frame(dtable), year == "2014")

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

    data <- as.data.frame(dtable)

    p <- data %>%
      group_by(year) %>%
      do(p = one_plot(.)) %>%
      subplot(shareX = TRUE, titleX = FALSE, nrows = 6) %>%
      layout(yaxis = list(title = " "),
             font = list(family="Segoe UI", size = 13),
             margin = list(l=100, b=100, t=50, r=0)  
             )

    p
  })


# Development Capacity Reactions and Rendering ----------------------------


  # Display graphs or text depending if Development Capacity indicators exist
  output$condDcap_msg <- renderUI({
    if (is.null(devdt())){
      verbatimTextOutput("condDcap_msg_text")
    } else {
      return(NULL)
    }
  })
  
  output$condDcap_msg_text <- renderText({
    "Development indicators have not yet been generated for any of your selected runs"
  })
  
  # Returning a list of runs with DevCap indicators
  output$dcap_select_run <- renderUI({
    if (is.null(devdt())) return(NULL)
    devdt <- devdt()
    select.runs <- unique(devdt[, run])
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
    capdt <- capdt()
    devdt <- devdt()
    if (is.null(capdt) || is.null(devdt)) return(NULL)
    if (is.null(dcapRun()) || is.null(input$dcap_select_geography) || is.null(dcapYear())) return(NULL)
    
    t1 <- capdt[run == dcapRun() & geography == dcapGeog() & captype == "Total",][,.(name_id, capacity, captype)]
    t2 <- devdt[run == dcapRun() & geography == dcapGeog() & year == dcapYear() & devtype == "Building Sqft",]
    
    if (nrow(t1) == 0 | nrow(t2) == 0) {
      return(NULL)
    } else if (dcapGeog() == 'zone' & (nrow(t1) < 3700) | dcapGeog() == 'city' & (nrow(t1) < 140)){
      return(NULL)
    } else {
      t <- merge(t1, t2, by = c("name_id"))
      t0 <- t[, diff := capacity-estimate]
      return(switch(as.integer(input$dcap_select_geography),
                    merge(t0, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
                    merge(t0, faz.lookup, by.x = "name_id", by.y = "faz_id"),
                    merge(t0, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name"),
                    t0
      ))
    }
   
  })

  dcapTable_res <- reactive({
    capdt <- capdt()
    devdt <- devdt()
    
    if (is.null(capdt) || is.null(devdt)) return(NULL)
    if (is.null(dcapRun()) || is.null(input$dcap_select_geography) || is.null(dcapYear())) return(NULL)
    
    t1 <- capdt[run == dcapRun() & geography == dcapGeog() & captype == "Residential",][,.(name_id, capacity, captype)]
    t2 <- devdt[run == dcapRun() & geography == dcapGeog() & year == dcapYear() & devtype == "Residential Units",]
    
    if (nrow(t1) == 0 | nrow(t2) == 0) {
      return(NULL)
    } else if (dcapGeog() == 'zone' & (nrow(t1) < 3700) | dcapGeog() == 'city' & (nrow(t1) < 140)){
      return(NULL)
    } else {
      t <- merge(t1, t2, by = c("name_id"))
      t0 <- t[, diff := capacity-estimate]
      return(switch(as.integer(input$dcap_select_geography),
                    merge(t0, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
                    merge(t0, faz.lookup, by.x = "name_id", by.y = "faz_id"),
                    merge(t0, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name"),
                    t0
      ))
    }
  })

  dcapTable_nonres <- reactive({
    capdt <- capdt()
    devdt <- devdt()
    
    if (is.null(capdt) | is.null(devdt)) return(NULL)
    if (is.null(dcapRun()) || is.null(input$dcap_select_geography) || is.null(dcapYear())) return(NULL)
    
    t1 <- capdt[run == dcapRun() & geography == dcapGeog() & captype == "Non-Residential",][,.(name_id, capacity, captype)]
    t2 <- devdt[run == dcapRun() & geography == dcapGeog() & year == dcapYear() & devtype == "Non-Residential Sqft",]
    
    if (nrow(t1) == 0 | nrow(t2) == 0) {
      return(NULL)
    } else if (dcapGeog() == 'zone' & (nrow(t1) < 3700) | dcapGeog() == 'city' & (nrow(t1) < 140)){
      return(NULL)
    } else {
      t <- merge(t1, t2, by = c("name_id"))
      t0 <- t[, diff := capacity-estimate]
      return(switch(as.integer(input$dcap_select_geography),
                    merge(t0, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
                    merge(t0, faz.lookup, by.x = "name_id", by.y = "faz_id"),
                    merge(t0, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name"),
                    t0
      ))
    }
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
           "TAZ",
           "FAZ",
           "City",
           "Growth Center"
    )
  })

  # Total Dev Capacity map
  output$dcap_total_map <- renderLeaflet({
    dcapshapetot <- dcapShape_total()
    if (is.null(dcapshapetot$diff)) return(NULL)
      
      # Set up symbology and categorization
      colorBinResult <- map.colorBins(dcapshapetot$diff, input$dcap_select_geography)
      pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=dcapshapetot$diff, pretty = FALSE)

      # popup setup
      geo.popup1 <- map.shp.popup(dcapshapetot,'capacity','estimate',dcapGeo(), 'Total Max Development Capacity', 'Building Sqft')
      geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)

      if (as.integer(input$dcap_select_geography) == 4){
        map <- map.layers.basic(dcapshapetot, dcapGeo(), "Total Development Capacity", geo.popup1, pal)
      } else {
        map <- map.layers(dcapshapetot, dcapGeo(), "Total Development Capacity", geo.popup1, geo.popup3, pal)
      }

      map

  })

  # Residential Dev Capacity map
  output$dcap_res_map <- renderLeaflet({
    dcapshaperes <- dcapShape_res()
    if (is.null(dcapshaperes$diff)) return(NULL)
    
    # Set up symbology and categorization
      colorBinResult <- map.colorBins(dcapshaperes$diff, input$dcap_select_geography)
      pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=dcapshaperes$diff, pretty = FALSE)

      # popup setup
      geo.popup1 <- map.shp.popup(dcapshaperes,'capacity','estimate', dcapGeo(), 'Residential Max Development Capacity', 'Residential Units')
      geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)

      if (as.integer(input$dcap_select_geography) == 4){
        map <- map.layers.basic(dcapshaperes, dcapGeo(), "Residential Development Capacity", geo.popup1, pal)
      } else {
        map <- map.layers(dcapshaperes, dcapGeo(), "Residential Development Capacity", geo.popup1, geo.popup3, pal)
      }

      map

  })

  # Non-Residential Dev Capacity map
  output$dcap_nonres_map <- renderLeaflet({
    dcapshapenonres <- dcapShape_nonres()
    if (is.null(dcapshapenonres$diff)) return(NULL)
    
    # Set up symbology and categorization
      colorBinResult <- map.colorBins(dcapshapenonres$diff, input$dcap_select_geography)
      pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=dcapshapenonres$diff, pretty = FALSE)

      # popup setup
      geo.popup1 <- map.shp.popup(dcapshapenonres,'capacity','estimate',dcapGeo(), 'Non-Residential Max Development Capacity', 'Non-Residential Sqft')
      geo.popup3 <- paste0("<strong>Center: </strong>", centers$name_id)

      if (as.integer(input$dcap_select_geography) == 4){
        map <- map.layers.basic(dcapshapenonres, dcapGeo(), "Non-Residential Development Capacity", geo.popup1, pal)
      } else {
        map <- map.layers(dcapshapenonres, dcapGeo(), "Non-Residential Development Capacity", geo.popup1, geo.popup3, pal)
      }

      map

  })

# Vacancy Reactions and Rendering -----------------------------------------

  output$vacancy_select_run <- renderUI({
    if (is.null(vacdt())) return(NULL)
    vacdt <- vacdt()
    # browser()
    select.runs <- unique(vacdt[, run])
    selectInput(inputId = "vac_select_run",
                label = "Run",
                choices = select.runs)
  })
  
  output$vacancy_select_bldgtype <- renderUI({
    if (is.null(vacdt())) return(NULL)
    vacdt <- vacdt()
    select.runs <- unique(vacdt[, building_type])
    selectInput(inputId = "vac_select_bldgtype",
                label = "Building Type",
                choices = select.runs)
  })
  
  output$vac_key <- renderUI({
    HTML(paste("Res_4: Condo", "<br/>",
               "Res_12: Multi-Family", "<br/>",
               "Res_19: Single Family", "<br/>",
               "Nonres_3: Commercial","<br/>",
               "Nonres_8: Industrial","<br/>",
               "Nonres_13: Office","<br/>",
               "Nonres_20: TCU","<br/>",
               "Nonres_21: Warehouse"
               )
         )  
  })
  
  vTable <- reactive({
    vacdt <- vacdt()
    vacdt[run == input$vac_select_run & building_type == input$vac_select_bldgtype,]
  })
  
  output$vacancy_estimate_plot <- renderPlotly({
    vTable <- vTable() 
    v <- vTable[type == "estimate", ]
    p <- linegraph(v, v$year, v$value, v$county_id)
  })
  
  output$vacancy_rate_plot <- renderPlotly({
    vTable <- vTable() 
    v <- vTable[type == "rate", ]
    p <- linegraph(v, v$year, v$value, v$county_id)
  })
  
  outputOptions(output, 'strdtavail', suspendWhenHidden = FALSE)
  outputOptions(output, 'gstrdtavail', suspendWhenHidden = FALSE)
}# end server function
