function(input, output, session) {
  
# functions ---------------------------------------------------------------

  
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
  
  # Prepares generic topsheet table for main indicators (households, population, employment) 
  create.tsTable <- function(table){
    runs <- runs()
    
    t1 <- dcast.data.table(table, County ~ run, value.var = yr.col)
    setcolorder(t1, c("County", paste0(yr.col[1],"_",runs[1]), paste0(yr.col[2],"_",runs[1]), paste0(yr.col[2],"_",runs[2]), paste0(yr.col[1],"_",runs[2])))
    t1[, ncol(t1) := NULL]
    t1[, Change := (t1[[ncol(t1)-1]]-t1[[ncol(t1)]])][, Per.Change := round((Change/t1[[3]])*100, 2)]
    setnames(t1, colnames(t1), c("County", 
                                 paste0(yr.fl[1], "_", runs[1]),
                                 paste0(yr.fl[2], "_", runs[1]),
                                 paste0(yr.fl[2], "_", runs[2]),
                                 "Change",
                                 "Per.Change"))
    t1[, 2:5 := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = 2:5]
  }
  
  # Prepares expanded topsheet table for RGCs & Key Locations
  create.exp.tsTable <- function(table){
    runs <- runs()
    
    setcolorder(table, 
                c("Name",
                  paste0(yr.col[1], "_", "Population","_", runs[1]),
                  paste0(yr.col[2], "_", "Population","_", runs[1]),
                  paste0(yr.col[2], "_", "Population","_", runs[2]),
                  paste0(yr.col[1], "_", "Employment","_", runs[1]),
                  paste0(yr.col[2], "_", "Employment","_", runs[1]),
                  paste0(yr.col[2], "_", "Employment","_", runs[2]),
                  paste0(yr.col[1], "_", "Population","_", runs[2]),
                  paste0(yr.col[1], "_", "Employment","_", runs[2])))
    table[, (ncol(table)-1):ncol(table) := NULL] 
    
    t1<- table[, Pop.Change := (table[[3]]-table[[4]])
               ][, Pop.Per.Change := round((Pop.Change/table[[3]])*100, 2)
                 ][, Emp.Change := (table[[6]]-table[[7]])
                   ][, Emp.Per.Change := round((Emp.Change/table[[6]])*100, 2)]
    setcolorder(t1,
                c("Name",
                  paste0(yr.col[1], "_", "Population","_", runs[1]),
                  paste0(yr.col[2], "_", "Population","_", runs[1]),
                  paste0(yr.col[2], "_", "Population","_", runs[2]),
                  "Pop.Change",
                  "Pop.Per.Change",
                  paste0(yr.col[1], "_", "Employment","_", runs[1]),
                  paste0(yr.col[2], "_", "Employment","_", runs[1]),
                  paste0(yr.col[2], "_", "Employment","_", runs[2]),
                  "Emp.Change",
                  "Emp.Per.Change"))
    setnames(t1, colnames(t1), c("Name",
                                 paste0(yr.fl[1], "_", "Pop","_", runs[1]),
                                 paste0(yr.fl[2], "_", "Pop","_", runs[1]),
                                 paste0(yr.fl[2], "_", "Pop","_", runs[2]),
                                 "Pop.Change",
                                 "Pop.Per.Change",
                                 paste0(yr.fl[1], "_", "Emp","_", runs[1]),
                                 paste0(yr.fl[2], "_", "Emp","_", runs[1]),
                                 paste0(yr.fl[2], "_", "Emp","_", runs[2]),
                                 "Emp.Change",
                                 "Emp.Per.Change"))
    t1[, c(2:5,7:10) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = c(2:5,7:10)]
  }
  
# Initialize Dashboard ----------------------------------------------------

  
  # trim.subdir <- tempfile(pattern="sessiondir", tmpdir=".")
  trim.subdir <- tempfile(pattern="sessiondir", tmpdir="")
  subdir <- file.path("www", trim.subdir)
  vars <- reactiveValues(submitted=FALSE)
  
  base.dir <- reactive({
          base[[as.integer(input$init_select_server)]]
  })
  
  output$init_select_run1 <- renderUI({
    select.run1 <- list.files(base.dir())
    selectInput(inputId = "select_run1",
                label = "Run 1",
                choices = select.run1,
                width = "100%")
  })
  
  selectRun1.exclude <- reactive({
    input$select_run1
  })
  
  output$init_select_run2all <- renderUI({
    select.run2all <- list.files(base.dir())
    new.select.run2all <- setdiff(select.run2all, selectRun1.exclude())
    selectInput(inputId = "select_run2all",
                label = "Run 2 (select one or more)",
                choices = new.select.run2all,
                selected = new.select.run2all[1],
                multiple = TRUE,
                width = "100%")
  })
  
  output$init_select_resultsdir <- renderUI({
    select.resultsdir <- list.files(file.path(wrkdir, "results"))
    selectInput(inputId = "select_resultsdir",
                label = "Makefile Results Folder",
                choices = select.resultsdir,
                width = "100%")
  })
  
  selectResultsDir <- eventReactive(input$goButton, {
    input$select_resultsdir
    
  })
  
  resultsDir <- eventReactive(input$goButton, {
    file.path(wrkdir, "results", selectResultsDir())
  })
  
  # create sub-directory in 'www'
  # find text files from results dir and copy to www dir
  observeEvent(input$goButton, {
    if (!(file.exists(subdir))) dir.create(subdir)
    
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
    if(file.exists(indexf.dir)) {
    	file.copy(indexf.dir, subdir, recursive = TRUE)
    	indexdirs <- c()#'bootstrap-3.3.5', 'jquery-1.11.3'
    	for (dir in indexdirs)
      		unlink(file.path(subdir, 'index_files', dir), recursive = TRUE)
    }
    vars$submitted <- TRUE
  })
  
  output$link <- renderUI({HTML(paste0("<a href=", "'", file.path(trim.subdir, 'index.html'), "'", "target='blank'>View Index file</a>"))})
  
  selectRun1 <- eventReactive(input$goButton, {
    input$select_run1
  })
  
  selectRun2 <- eventReactive(input$goButton, {
    input$select_run2all
  })
  
  runnames <- eventReactive(input$goButton, {
    c(selectRun1(), selectRun2())
  })
  
  runname1 <- eventReactive(input$goButton, {
    unlist(strsplit(selectRun1(),"[.]"))[[1]]
  })
  
  runnames2 <- eventReactive(input$goButton, {
    sapply(strsplit(selectRun2(),"[.]"), function(x) x[1])
  })
  
  runs <- eventReactive(input$goButton, {
    c(runname1(), unlist(runnames2()))
  })
  
  # build general attributes source table
  alldt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
    
    alldata.table <- NULL
    for (r in 1:length(runnames)) {
      for (a in 1:length(geography)){
        for (i in 1:length(attribute)){
          table <- NULL
          filename <- paste0(geography[a],'__',"table",'__',attribute[i], extension)
          datatable <- read.csv(file.path(base.dir(), runnames[r],"indicators",filename), header = TRUE, sep = ",")
          column_id <- colnames(datatable)[grepl("_id",names(datatable))]
          column_est <- NULL
          for (y in 1: length(years)){
            column_est1 <- colnames(datatable)[grepl((years[y]),names(datatable))]
            ifelse (is.null(column_est1),
                    column_est <- column_est1,
                    column_est <- cbind(column_est, column_est1))
          }
          table <- datatable[,c(column_id,column_est)]
          colnames(table)[2:ncol(table)] <- paste0("yr", sapply(years, function(x) x[1]))
          colnames(table)[1] <- "name_id"
          table$indicator <- switch(attribute[i],
                                    "population"="Total Population",
                                    "households"="Households",
                                    "employment"="Employment",
                                    "residential_units"="Residential Units")
          
          table$geography <- geography[a]
          table$run <- runs[r]
          
          ifelse (is.null(alldata.table), alldata.table <- table, alldata.table <- rbind(alldata.table, table))

        } # end of attribute loop
      } # end of geography loop
      
      # alldt <- as.data.table(alldata.table)
    } # end of runnames loop
    
    return(as.data.table(alldata.table))
  })
  
  # build demographics indicators source table
  demogdt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
  
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
        demog.files <- as.list(list.files(file.path(base.dir(), runnames[r], "indicators"), pattern = paste0(demog.indicators[[d]], extension)))
        if (length(demog.files) > 0){
          for (f in 1:length(demog.files)){
            year <- str_match(demog.files[f] , "(\\d+){4}")[,1]
            datafile <- read.csv(file.path(base.dir(), runnames[r], "indicators", demog.files[f]), header = TRUE, sep = ",")
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
    ifelse(!is.null(demog.table), return(as.data.table(demog.table)), return(NULL))
  })
  
  # build max capacity source tables
  capdt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
    
    cap.geography <- c(geography, "growth_center")
    cap.type <- c("max_dev", "max_dev_nonresidential", "max_dev_residential")
    
    cap.table <- NULL

    for (r in 1:length(runnames)){
      cap.files <- as.list(list.files(file.path(base.dir(), runnames[r], "indicators"), pattern = paste0("max_dev(_\\w+)*", extension)))
      if (length(cap.files) > 6){
        for (g in 1:length(cap.geography)){
          for (c in 1:length(cap.type)){
            cap.tbl <- NULL
            cap.file <- paste0(cap.geography[g], '__table__', cap.type[c], "_capacity", extension)
            cap.tbl <- read.csv(file.path(base.dir(), runnames[r],"indicators", cap.file), header = TRUE, sep = ",")
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
      } else if (length(cap.files) <= 6){
        next
      } # end conditional
    } # end of runnames loop

    return(as.data.table(cap.table))
  })

  devdt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
    
    cap.geography <- c(geography, "growth_center")
    dev.type <- c("residential_units", "building_sqft", "nonres_sqft")
    
    dev.dt <- NULL
    
    for (r in 1:length(runnames)){
      dev.files <- as.list(list.files(file.path(base.dir(), runnames[r], "indicators"), pattern = paste0("sqft", extension)))
      if (length(dev.files) > 6){
        for (g in 1:length(cap.geography)){
          for (d in 1:length(dev.type)){
            dev.tbl <- NULL
            dev.file <- paste0(cap.geography[g], '__table__', dev.type[d], extension)
            dev.tbl <- fread(file.path(base.dir(), runnames[r],"indicators", dev.file), header = TRUE)
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
      } else if (length(dev.files) <= 6){
        next
      } # end conditional
    } # end of runnames loop
    return(dev.dt)
  })
  
  # Build Jobs by Sector table
  jobsectdt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
    
    sectorJobs.pat <- "census_tract__dataset_table__employment_by_aggr_sector__\\d+"
    
    sectorJobs.table <- NULL
    for (r in 1:length(runnames)) {
      sectorJobs.file <- list.files(file.path(base.dir(), runnames[r], "indicators"), pattern = paste0(sectorJobs.pat, extension))
      for (f in 1:length(sectorJobs.file)){
        # table <- NULL
        table <- read.csv(file.path(base.dir(), runnames[r], "indicators", sectorJobs.file[f]), header = TRUE, sep = ",")
        col.sum <- apply(table, 2, sum)
        sectorJobs.df <- transpose(data.frame(col.sum))
        colnames(sectorJobs.df) <- colnames(table)
        sectorJobs.df$run <- runs[r]
        sectorJobs.df$census_tract_id <- NULL
        sectorJobs.df$year <- str_match(sectorJobs.file[f], "(\\d+){4}")[,1]
        ifelse(is.null(sectorJobs.table), sectorJobs.table <- sectorJobs.df, sectorJobs.table  <- rbind(sectorJobs.table, sectorJobs.df))
      } # end sectorJobs.file loop
    } # end runnames loop
    
    sectorJobs.table <- as.data.table(sectorJobs.table)
    sj <- melt.data.table(sectorJobs.table, id.vars = c("run", "year"), measure.vars = colnames(sectorJobs.table)[1:(ncol(sectorJobs.table)-2)])
    setnames(sj, colnames(sj), c("run", "year", "sector", "estimate"))
    return(sj)
  })
  
  # Build Growth Centers table
  growctrdt <- eventReactive(input$goButton,{
    runnames <- runnames()
    runs <- runs()
    
    growctr.table <- NULL
    for (r in 1:length(runnames)) {
        for (i in 1:length(attribute)){
          table <- NULL
          filename <- paste0('growth_center__table','__',attribute[i], extension)
          datatable <- read.csv(file.path(base.dir(), runnames[r],"indicators",filename), header = TRUE, sep = ",")
          column_id <- colnames(datatable)[grepl("_id",names(datatable))]
          column_est <- NULL
          for (y in 1: length(years)){
            column_est1 <- colnames(datatable)[grepl((years[y]),names(datatable))]
            ifelse (is.null(column_est1),
                    column_est <- column_est1,
                    column_est <- cbind(column_est, column_est1))
          }
          table <- datatable[,c(column_id,column_est)]
          colnames(table)[2:ncol(table)] <- paste0("yr", sapply(years, function(x) x[1]))
          colnames(table)[1] <- "name_id"
          table$indicator <- switch(attribute[i],
                                    "population"="Total Population",
                                    "households"="Households",
                                    "employment"="Employment",
                                    "residential_units"="Residential Units")
          
          
          table$run <- runs[r]
          
          ifelse (is.null(growctr.table), growctr.table <- table, growctr.table <- rbind(growctr.table, table))
          
        } # end of attribute loop
    } # end of runnames loop
    
    rgc.lookup1 <- rgc.lookup[,c("growth_center_id", "name")]
    gc.table <- merge(growctr.table, rgc.lookup1, by.x = "name_id", by.y = "growth_center_id")
    
    return(as.data.table(gc.table))
  })
  
  # Data ready message
  index.ready <- reactive({
    file.path(resultsDir(), 'index.html')
  })
  
  output$submit_msg <- renderText({
    if (!is.null(index.ready())) "Data has been loaded, click on Index link or dashboard tabs"
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
    filename <- "Description.txt"
    desc.file <- readLines(file.path(base.dir(), selectRun1(),"indicators", filename), warn = FALSE)
    paste("<b>Description</b>:", desc.file)
  })
  
  output$ts_rest <- renderText({
    filename <- "Restrictions.txt"
    desc.file <- readLines(file.path(base.dir(), selectRun1(),"indicators", filename), warn = FALSE)
    paste("<b>Restrictions</b>:", desc.file)
  })
  
  # Filter table and calculate regional totals for general all-data-table
  tsTable <- reactive({
    alldt <- alldt()
    runs <- runs()
    
    t <- merge(alldt[geography == 'zone' & (run == runs[1] | run == runs[2])], zonecnty.lookup, by.x = "name_id", by.y = "TAZ")
    t1 <- t[, lapply(.SD, sum), by = list(County = COUNTY_NM, indicator, run), .SDcols = yr.col]
    t.sum <- t1[, .(yr2014 = sum(yr2014), yr2040 = sum(yr2040)), by = list(indicator, run)][, County := "Sub-Total: Region"]
    rbindlist(list(t1, t.sum), use.names = TRUE)
  })
  
  # Display households summary table
  output$tpsht_hh <- renderTable({
    tsTable <- tsTable()
    runs <- runs()
    
    t <- tsTable[indicator == 'Households']
    t1 <- create.tsTable(t)
  }, hover = TRUE, width = '75%', align = 'lrrrrr')
  
  # Display population summary table
  output$tpsht_pop <- renderTable({
    tsTable <- tsTable()
    runs <- runs()
    
    t <- tsTable[indicator == 'Total Population']
    t1 <- create.tsTable(t)
  }, hover = TRUE, digits = 1, width = '75%', align = 'lrrrrr')
  
  # Display employment summary table
  output$tpsht_emp <- renderTable({
    tsTable <- tsTable()
    runs <- runs()
    
    t <- tsTable[indicator == 'Employment']
    t1 <- create.tsTable(t)
  }, hover = TRUE, digits = 1, width = '75%', align = 'lrrrrr')
  
  # Filter table and calculate totals for PWTYPE
  tsPwtypeTable <- reactive({
    demogdt <- demogdt()
    runs <- runs()
    
    t <- demogdt[demographic == 'workertype' & (run == runs[1] | run == runs[2]) & (year %in% yr.fl)
                 ][, lapply(.SD, sum), by = list(Group = groups, run, year), .SDcols = "estimate"]
    t.sum <- t[, .(estimate = sum(estimate)), by = list(run, year)][, Group := "Sub-Total: Persons"]
    rbindlist(list(t, t.sum), use.names = TRUE)
  })
  
  # Display PWTYPE summary table
  output$tpsht_pwtype <- renderTable({
    tsPwtypeTable <- tsPwtypeTable()
    runs <- runs()
    newOrder <- c("full_time", "part_time", "non_workers_no_job", "workers_no_job", "Sub-Total: Persons")
    
    if (length(unique(tsPwtypeTable$run)) == 2) {
      pt <- dcast.data.table(tsPwtypeTable, Group ~ year + run, value.var = "estimate")
      setcolorder(pt, c("Group", paste0(yr.fl[1],"_",runs[1]), paste0(yr.fl[2],"_",runs[1]), paste0(yr.fl[2],"_",runs[2]), paste0(yr.fl[1],"_",runs[2])))
      pt[, ncol(pt) := NULL]
      pt[, Change := (pt[[ncol(pt)-1]]-pt[[ncol(pt)]])][, Per.Change := round((Change/pt[[3]])*100, 2)][ , name := factor(Group, levels = newOrder)]
      t0 <- pt[with(pt, order(name)),]
      t <- t0[, -"name", with = FALSE]
      t[, 2:5 := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = 2:5]
    } else {
      pt <- dcast.data.table(tsPwtypeTable, Group ~ year + run, value.var = "estimate")
      pt[ , name := factor(Group, levels = newOrder)]
      t0 <- pt[with(pt, order(name)),]
      t <- t0[, -"name", with = FALSE]
      t[, 2:ncol(t) := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = 2:ncol(t)]
    }
  }, hover = TRUE, digits = 1, width = '75%', align = 'lrrrrr')
  
  # Filter table and calculate totals for PTYPE
  tsPtypeTable <- reactive({
    demogdt <- demogdt()
    runs <- runs()
    
    t <- demogdt[demographic == 'persontype' & (run == runs[1] | run == runs[2]) & (year %in% yr.fl)
                 ][, lapply(.SD, sum), by = list(Group = groups, run, year), .SDcols = "estimate"]
    t.sum <- t[, .(estimate = sum(estimate)), by = list(run, year)][, Group := "Sub-Total: Persons"]
    rbindlist(list(t, t.sum), use.names = TRUE)
  })
  
  # Display PTYPE summary table
  output$tpsht_ptype <- renderTable({
    tsPtypeTable <- tsPtypeTable()
    runs <- runs()
    newOrder <- c("full_time_worker", "part_time_worker", "non_working_adult_age_65_plus", "non_working_adult_age_16_64", 
      "university_student" , "hs_student_age_15_up" , "child_age_5_15" , "child_age_0_4" , "Sub-Total: Persons" )
    
    pt <- dcast.data.table(tsPtypeTable, Group ~ year + run, value.var = "estimate")
    setcolorder(pt, c("Group", paste0(yr.fl[1],"_",runs[1]), paste0(yr.fl[2],"_",runs[1]), paste0(yr.fl[2],"_",runs[2]), paste0(yr.fl[1],"_",runs[2])))
    pt[, ncol(pt) := NULL]
    pt[, Change := (pt[[ncol(pt)-1]]-pt[[ncol(pt)]])][, Per.Change := (Change/pt[[3]])*100][ , name := factor(Group, levels = newOrder)]
    t0 <- pt[with(pt, order(name)),]
    t <- t0[, -"name", with = FALSE]
    t[, 2:5 := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = 2:5]
  }, hover = TRUE, digits = 1, width = '75%', align = 'lrrrrr')
  
  # Filter table and calculate totals for Income summary table
  tsIncTable <- reactive({
    demogdt <- demogdt()
    runs <- runs()

    t <- demogdt[demographic == 'incomegroup' & (run == runs[1] | run == runs[2]) & (year %in% yr.fl)
                 ][, lapply(.SD, sum), by = list(Group = groups, run, year), .SDcols = "estimate"]
    t.sum <- t[, .(estimate = sum(estimate)), by = list(run, year)][, Group := "Sub-Total: Households"]
    rbindlist(list(t, t.sum), use.names = TRUE)
  })
  
  # Display Households by Income summary table
  output$tpsht_hhInc <- renderTable({
    tsIncTable <- tsIncTable()
    runs <- runs()
    
    t <- dcast.data.table(tsIncTable, Group ~ year + run, value.var = "estimate")
    setcolorder(t, c("Group", paste0(yr.fl[1],"_",runs[1]), paste0(yr.fl[2],"_",runs[1]), paste0(yr.fl[2],"_",runs[2]), paste0(yr.fl[1],"_",runs[2])))
    t[, ncol(t) := NULL]
    t[, Change := (t[[ncol(t)-1]]-t[[ncol(t)]])][, Per.Change := (Change/t[[3]])*100]
    t[, 2:5 := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = 2:5]
  }, hover = TRUE, digits = 1, width = '75%', align = 'lrrrrr')
  
  # Filter table and calculate totals for Jobs by Sector table
  tsSectorJobs <- reactive({
    jobsectdt <- jobsectdt()
    runs <- runs()
    
    t <- jobsectdt[(run == runs[1] | run == runs[2]) & (year %in% yr.fl)]
    t.sum <- t[, .(estimate = sum(estimate)), by = list(run, year)][, sector := "Sub-Total: Jobs"]
    rbindlist(list(t, t.sum), use.names = TRUE)
  })
  
  # Display Jobs by sector summary table
  output$tpsht_jobs <- renderTable({
    tsSectorJobs <- tsSectorJobs()
    runs <- runs()
    
    t <- dcast.data.table(tsSectorJobs, sector ~ year + run, value.var = "estimate")
    setnames(t, "sector", "Sector")
    setcolorder(t, c("Sector", paste0(yr.fl[1],"_",runs[1]), paste0(yr.fl[2],"_",runs[1]), paste0(yr.fl[2],"_",runs[2]), paste0(yr.fl[1],"_",runs[2])))
    t[, ncol(t) := NULL]
    t[, Change := (t[[ncol(t)-1]]-t[[ncol(t)]])][, Per.Change := round((Change/t[[3]])*100, 2)]
    t[, 2:5 := lapply(.SD, FUN=function(x) prettyNum(x, big.mark=",")), .SDcols = 2:5]
  }, hover = TRUE, digits = 1, width = '75%', align = 'lrrrrr')
  
  # Filter table and calculate totals for largest RGCs
  tsGrowthCtr <- reactive({
    growctrdt <- growctrdt()
    runs <- runs()
    
    lg.rgc <- c("Bellevue", "Everett", "SeaTac", "Seattle Downtown", "Seattle First Hill/Capitol Hill", "Seattle South Lake Union", 
                "Seattle University Community", "Tacoma Downtown")
    t <- growctrdt[(indicator == 'Total Population' | indicator == 'Employment') & (run == runs[1] | run == runs[2])]
    t[indicator == 'Total Population', indicator := 'Population']
    t1 <- t[, lapply(.SD, sum), by = list(name, indicator, run), .SDcols = yr.col]
    t1 <- t1[name %in% lg.rgc, ]
  })
  
  # Display largest RGCs summary table
  output$tpsht_rgc <- renderTable({
    tsGrwothCtr <- tsGrowthCtr()
    runs <- runs()

    t <- dcast.data.table(tsGrwothCtr, name ~ indicator + run, value.var = yr.col)
    setnames(t, "name", "Name")
    create.exp.tsTable(t)
  }, hover = TRUE, digits = 1, width = '100%', align = 'lrrrrrrrrrr')
  
  # Filter table and calculate totals for Special Places
  tsSplace <- reactive({
    alldt <- alldt()
    runs <- runs()
    
    key.loc <- c("UW", "Amazon", "SeaTac Airport", "Microsoft Overlake", "Paine Field", "JBLM", "Bangor")
    t <- merge(alldt[geography == 'zone' & (run == runs[1] | run == runs[2]) & (indicator == 'Total Population' | indicator == 'Employment')], 
               splaces.lookup, by.x = "name_id", by.y = "zone_id")
    t1 <- t[, lapply(.SD, sum), by = list(Name = name, indicator, run), .SDcols = yr.col][Name %in% key.loc, ]
    t1[indicator == "Total Population", indicator := "Population"]
  })
  
  # Display Special Places summary table
  output$tpsht_splace <- renderTable({
    tsSplace <- tsSplace()
    runs <- runs()

    t <- dcast.data.table(tsSplace, Name ~ indicator + run, value.var = yr.col)
    create.exp.tsTable(t)
  }, hover = TRUE, digits = 1, width = '100%', align = 'lrrrrrrrrrr')
  

# Run Comparison Reactions ------------------------------------------------

  
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
    alldt <- alldt()
    dt1 <- alldt[run == runname1() & geography == cGeog() & indicator == cIndicator(),
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


# Run Comparison Rendering ------------------------------------------------


  # Plotly
  output$compare_plot <- renderPlotly({
    if(!vars$submitted) return(NULL)
    if (is.null(cRun())) return(NULL)
    runname2.trim <- sapply(strsplit(cRun(),"[.]"), function(x) x[1])
    scatterplot(cTable(), "compare", cTable()$estrun1, cTable()$estrun2, runname1(), runname2.trim)
  })
  
  # Leaflet
  output$compare_map <- renderLeaflet({
    if(!vars$submitted) return(NULL)
    if (is.null(cRun()) | is.null(cShape()$diff)) return(NULL)
    runname2.trim <- sapply(strsplit(cRun(),"[.]"), function(x) x[1])
    
    # Set up symbology and categorization
    colorBinResult <- map.colorBins(cShape()$diff, input$compare_select_geography)
    pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin, domain=cShape()$diff, pretty = FALSE)
    
    # popup setup
    geo.popup1 <- map.shp.popup(cShape(),'estrun1','estrun2', cGeo(), runname1(), runname2.trim)
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
  

# Growth Reactions --------------------------------------------------------


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
     if (is.null(gRun()) || is.null(input$growth_select_geography) || is.null(gYear())) return(NULL)
     alldt <- alldt()
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
  

# Growth Rendering --------------------------------------------------------


  # Plotly
  output$growth_plot <- renderPlotly({
    if(!vars$submitted) return(NULL)
    if (is.null(gTable())) return(NULL)
    scatterplot(gTable(), "growth", gTable()$yr1, gTable()$yr2, as.character(years[[1]]), gYear.label())
  })

  # Leaflet
  output$growth_map <- renderLeaflet({
    if(!vars$submitted) return(NULL)
    if (is.null(gShape()$diff)) return(NULL)
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
    if (is.null(demogdt()) || is.null(dRun())) return(NULL)
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
    if (is.null(dTable())) return(NULL)
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
    if (is.null(capdt()) | is.null(devdt())) return(NULL)
    if (is.null(dcapRun()) || is.null(input$dcap_select_geography) || is.null(dcapYear())) return(NULL)
    
    capdt <- capdt()
    devdt <- devdt()

    t1 <- capdt[run == dcapRun() & geography == dcapGeog() & captype == "Total",][,.(name_id, capacity, captype)]
    t2 <- devdt[run == dcapRun() & geography == dcapGeog() & year == dcapYear() & devtype == "Building Sqft",]
    t <- merge(t1, t2, by = c("name_id"))
    t0 <- t[, diff := capacity-estimate]

    return(switch(as.integer(input$dcap_select_geography),
           merge(t0, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
           merge(t0, faz.lookup, by.x = "name_id", by.y = "faz_id"),
           merge(t0, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name"),
           t0
    ))
  })

  dcapTable_res <- reactive({
    if (is.null(capdt()) | is.null(devdt())) return(NULL)
    if (is.null(dcapRun()) || is.null(input$dcap_select_geography) || is.null(dcapYear())) return(NULL)
    
    capdt <- capdt()
    devdt <- devdt()

    t1 <- capdt[run == dcapRun() & geography == dcapGeog() & captype == "Residential",][,.(name_id, capacity, captype)]
    t2 <- devdt[run == dcapRun() & geography == dcapGeog() & year == dcapYear() & devtype == "Residential Units",]
    t <- merge(t1, t2, by = "name_id")
    t0 <- t[, diff := capacity-estimate]

    return(switch(as.integer(input$dcap_select_geography),
           merge(t0, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
           merge(t0, faz.lookup, by.x = "name_id", by.y = "faz_id"),
           merge(t0, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name"),
           t0
    ))
  })

  dcapTable_nonres <- reactive({
    if (is.null(capdt()) | is.null(devdt())) return(NULL)
    if (is.null(dcapRun()) || is.null(input$dcap_select_geography) || is.null(dcapYear())) return(NULL)
    
    capdt <- capdt()
    devdt <- devdt()

    t1 <- capdt[run == dcapRun() & geography == dcapGeog() & captype == "Non-Residential",][,.(name_id, capacity, captype)]
    t2 <- devdt[run == dcapRun() & geography == dcapGeog() & year == dcapYear() & devtype == "Non-Residential Sqft",]
    t <- merge(t1, t2, by = "name_id")
    t0 <- t[, diff := capacity-estimate]

    return(switch(as.integer(input$dcap_select_geography),
           merge(t0, zone.lookup, by.x = "name_id", by.y = "zone_id") %>% merge(faz.lookup, by = "faz_id"),
           merge(t0, faz.lookup, by.x = "name_id", by.y = "faz_id"),
           merge(t0, city.lookup, by.x = "name_id", by.y = "city_id") %>% setnames("city_name", "Name"),
           t0
    ))
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
    if (is.null(dcapShape_total()$diff)) return(NULL)
      
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

  })

  # Residential Dev Capacity map
  output$dcap_res_map <- renderLeaflet({
    if (is.null(dcapShape_res()$diff)) return(NULL)
    
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

  })

  # Non-Residential Dev Capacity map
  output$dcap_nonres_map <- renderLeaflet({
    if (is.null(dcapShape_nonres()$diff)) return(NULL)
    
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

  })

}# end server function