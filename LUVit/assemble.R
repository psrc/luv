# Script for assembling LUV-it spreadsheet from simulation results
# Hana Sevcikova, PSRC
# Last update: June 14, 2023

library(data.table)
library(openxlsx2)

# set working directory to the location of this source file (e.g. in Rstudio or using setwd())
setwd("I:/LandUseForecast/luvrepo/LUVit")
#setwd("~/psrc/ForecastProducts/luv/LUVit")
#setwd("C:/code_repos/luv/LUVit")

# which run to use
run <- "run_120.run_2023_05_11_12_57" 

# where are the indicators
run.dir <- "N:/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs/awsmodel04"
#run.dir <- "~/n$/vision2050/opusgit/urbansim_data/data/psrc_parcel/runs/awsmodel04"

# whole path to the group-quarters Excel file
gq.file <- "Upd_OFM_SAEP_GQPop_Block_TAZ_Corres.xlsx" 
#gq.file <- "M:/LandUseForecast/luvrepo/LUVit/Upd_OFM_SAEP_GQPop_Block_TAZ_Corres.xlsx" 

# Name of the excel file that contains meta info
# The LUV-it results will be attached to whatever sheets are already in this file
metafile <- "LUVit_metadata.xlsx"

indicators.dir <- file.path(run.dir, run, "indicators")

# load BG crosswalks
# (contains columns county_id, census_2010_block_id, census_block_id, census_tract_id, 
# faz_id, zone_id, city_id, control_id, control_hct_id, target_id, census_2010_tract_id)
bgXwalk10 <- fread("data/2010_census_block_crosswalk.csv")
bgXwalk10[, census_2010_block_id := as.character(census_2010_block_id)]
bgXwalk10[, census_2010_tract_id := as.character(census_2010_tract_id)]
bgXwalk10[, subreg_id := control_hct_id]

bgXwalk20 <- fread("data/2020_census_block_crosswalk.csv")
#bgXwalk20[, census_2020_block_id := as.character(census_2020_block_id)]
#bgXwalk20[, census_2020_tract_id := as.character(census_2020_tract_id)]
bgXwalk20[, census_2020_block_geoid := as.character(census_2020_block_geoid)]
bgXwalk20[, census_2020_tract_geoid := as.character(census_2020_tract_geoid)]
bgXwalk20[, subreg_id := control_hct_id]

county.names <- data.table(county_id = c(33, 35, 53, 61), 
                            County = c("King", "Kitsap", "Pierce", "Snohomish"))

# load GQ info and merge with various geo cross walks
#gq <- data.table(read_xlsx(gq.file, startRow = 5, cols = c(22, 25, 27, 30:33, 28))) # read the actual numbers
gq10 <- data.table(read_xlsx(gq.file, startRow = 3, cols = 1:9, sheet = "updated_block10_gqpop")) # read the actual numbers
gq10[, census_2010_block_id := as.character(Blocks2010)][, Blocks2010 := NULL]
gqcols10 <- colnames(gq10)[-ncol(gq10)]
gq10 <- merge(bgXwalk10, gq10, by = "census_2010_block_id")

gq20 <- data.table(read_xlsx(gq.file, startRow = 3, cols = 1:9, sheet = "updated_block20_gqpop")) # read the actual numbers
gq20[, census_2020_block_geoid := as.character(Blocks2020)][, Blocks2020 := NULL]
gqcols20 <- colnames(gq20)[-ncol(gq20)]
gq20 <- merge(bgXwalk20, gq20, by = "census_2020_block_geoid")

tazXwalk <- fread("data/zones.txt")
tractXwalk10 <- fread("data/census_tracts.csv")
tractXwalk20 <- fread("data/census_2020_tracts.csv")

# load military
mil <- data.table(read_xlsx(gq.file, sheet = "enlisted w 2020 census block"))
setnames(mil, c("2017", "2045"), c("2018", "2044")) # use 2017 as 2018 and 2045 as 2044
miltimecols <- colnames(mil)[(ncol(mil)-7) : ncol(mil)]
mil <- mil[, c("census_block_id 2010", "geoid20", miltimecols), with = FALSE]
setnames(mil, c("census_block_id 2010", "geoid20"), c("census_block_id", "census_2020_block_geoid"))
mil10 <- merge(bgXwalk10, mil, by = "census_block_id")
mil20 <- merge(bgXwalk20, mil, by = "census_2020_block_geoid")

# derive number of time points and the actual years
hh <- fread(file.path(indicators.dir, "county__table__households.csv"))
years <- as.integer(substr(colnames(hh)[-1], 12, 15))
ntime <- length(years)

# load controls & cities names
control.names <- fread("data/control_hct_crosswalk.csv")

#wb <- createWorkbook()
#headSty <- createStyle(fgFill = "#DCE6F1", halign = "center", border = "TopBottomLeftRight")

# Functions
###########
assemble.hh.jobs <- function(dataset.name, id.name){
    hh <- assemble.residential.table(paste0(dataset.name, "__table__households.csv"), 
                                     paste0(dataset.name, "__table__population.csv"),
                                     id.name)
    jobs <- assemble.employment.table(paste0(dataset.name, "__table__employment.csv"), 
                                      paste0(dataset.name, "__dataset_table__employment_by_aggr_sector__XXXX.tab"), 
                                    id.name)
    return(merge(hh, jobs, by = id.name, all = TRUE))
}

assemble.residential.table <- function(file.hh, file.hhpop, id.name){
    hh <- fread(file.path(indicators.dir, file.hh))
    hhpop <- fread(file.path(indicators.dir, file.hhpop))
    setnames(hhpop, colnames(hhpop)[-1], gsub("population_", "HHPop_", colnames(hhpop)[-1])) # change column names to HHPop
    hhall <- merge(hh, hhpop, by = id.name)
    # Incorporate GQ (convert hhpop and gq into long format and join)
    hhpopl <- melt(hhpop, id.vars = id.name)
    hhpopl[, year := as.integer(substr(variable, 7, 10))]
    if(id.name == "census_2020_tract_id"){
        gq <-  gq20
        gqcols <- gqcols20
    } else {
        gq <-  gq10
        gqcols <- gqcols10
    }
    GQl <- melt(gq[, c(id.name, gqcols), with = FALSE], id.vars = id.name, variable.factor = FALSE)
    #GQl[, year := as.integer(substr(variable, 3,6))]
    GQl[, year := as.integer(variable)]
    GQs <- GQl[, .(gq = round(sum(value), 0)), by = c(id.name, "year")] # aggregate to the desired geography
    hhpop.gq <- merge(hhpopl, GQs, by = c(id.name, "year"), all.x = TRUE)
    hhpop.gq[is.na(gq), gq := 0]
    hhpop.gq[, Pop := value + gq][, `:=`(value = NULL, gq = NULL, variable = NULL)]
    # convert to wide format
    pop <- dcast(hhpop.gq, ... ~ year, value.var = "Pop")
    colnames(pop)[-1] <- paste0("TotPop_", colnames(pop)[-1])
    GQ <- dcast(GQs, ... ~ year, value.var = "gq")
    colnames(GQ)[-1] <- paste0("GQ_", colnames(GQ)[-1])
    # attach to households
    hhall <- merge(merge(hhall, GQ, by = id.name), pop, by = id.name)
    hhall
}

assemble.employment.table <- function(file.jobs, file.jobs.by.sector, id.name){
    jobs <- fread(file.path(indicators.dir, file.jobs))
    setnames(jobs, colnames(jobs)[-1], gsub("employment_", "AllJobs", colnames(jobs)[-1])) # change column names to AllJobs
    # account for enlisted personnel (convert to long format and join with total jobs)
    jobsl <- melt(jobs, id.vars = id.name)
    jobsl[, year := as.integer(substr(variable, 8, 11))]
    mil <- if(id.name == "census_2020_tract_id") mil20 else mil10
    mill <- melt(mil[, c(id.name, miltimecols), with = FALSE], id.vars = id.name, variable.factor = FALSE)
    mill[, year := as.integer(variable)]
    mils <- mill[, .(mil = round(sum(value), 0)), by = c(id.name, "year")] # aggregate to the desired geography
    jobs.mil <- merge(jobsl, mils, by = c(id.name, "year"), all.x = TRUE)
    jobs.mil[is.na(mil), mil := 0]
    jobs.mil[, Emp := value + mil][, `:=`(value = NULL, mil = NULL, variable = NULL)]
    # convert to wide format
    jobs <- dcast(jobs.mil, ... ~ year, value.var = "Emp")
    colnames(jobs)[-1] <- paste0("AllJobs", colnames(jobs)[-1])
    
    # get jobs by sector
    all.jobs <- NULL
    for(y in years){
        file.jobs.by.sector.year <- sub("XXXX", y, file.jobs.by.sector)
        secjobs <- fread(file.path(indicators.dir, file.jobs.by.sector.year))
        # add enlisted personnel to Gov jobs
        secjobs <- merge(secjobs, mils[year == y, c(id.name, "mil"), with = FALSE], by = id.name, all.x = TRUE)
        secjobs[is.na(mil), mil := 0]
        secjobs[, Gov := Gov + mil][, mil := NULL]
        # change column names
        setnames(secjobs, colnames(secjobs)[-1], paste0(y - 2000, colnames(secjobs)[-1]))
        # merge with total jobs
        yjobs <- merge(jobs[, c(id.name, paste0("AllJobs", y)), with = FALSE], secjobs, by = id.name)
        all.jobs <- if(is.null(all.jobs)) yjobs else merge(all.jobs, yjobs, by = id.name)
    }
    all.jobs
}

add.header.style <- function(wbk, ntime, nwhite = 1, nsectors = 6){
    startcol.hh <- nwhite + 1
    startcol.hhpop <- startcol.hh + ntime
    startcol.gq <- startcol.hhpop + ntime
    startcol.pop <- startcol.gq + ntime
    startcol.emp <- startcol.pop + ntime
    wbk <- wb_add_font(wbk, dims = paste0("A1:", int2col(nwhite + 5 * ntime + nsectors * ntime), 1), color = wb_colour(hex = "#000000"))$ # black font of the header
        add_fill(dims = paste0("A1:", int2col(nwhite), 1), color = wb_colour(name = "white"))$ # first nwhite columns are white
        add_fill(dims = paste0(int2col(startcol.hh), "1:", int2col(startcol.hhpop - 1), 1), color = wb_colour(name = "grey"))$     # households
        add_fill(dims = paste0(int2col(startcol.hhpop), "1:", int2col(startcol.gq - 1), 1), color = wb_colour(name = "lightcyan"))$   # HHPop
        add_fill(dims = paste0(int2col(startcol.gq), "1:", int2col(startcol.pop - 1), 1), color = wb_colour(name = "lightcyan2"))$  # GQ
        add_fill(dims = paste0(int2col(startcol.pop), "1:", int2col(startcol.emp - 1), 1), color = wb_colour(name = "lightblue1"))$  # Pop
        add_fill(dims = paste0(int2col(startcol.emp - nsectors), "1:", int2col(startcol.emp + (nsectors + 1) * ntime - 1), 1), 
                 color = wb_colour(name = "khaki3"), every_nth_col = nsectors+1) # Total Emp
    startcol.sec <- startcol.emp + 1
    for(y in 1:ntime){
        wbk <- wb_add_fill(wbk, dims = paste0(int2col(startcol.sec), "1:", int2col(startcol.sec + nsectors - 1), 1),
                 color = wb_colour(name = "khaki1")) # Emp by sector for one year
        startcol.sec <- startcol.sec + nsectors + 1
    }
    wbk
}


# Read LUVit template that contains Metadata only
#########################

wb <- wb_load(metafile)


# assemble county results
#########################
# households & pop

cat("\nAssembling county results ...")
geores <- assemble.hh.jobs("county", "county_id")

geores <- merge(county.names, geores)[, county_id := NULL]

geores <- rbind(geores, cbind(data.table(County = "Region"), 
                                geores[, lapply(.SD, sum), .SDcols = colnames(geores)[-1]]))

wb <- wb_add_worksheet(wb, "County")$
    freeze_pane(firstCol = TRUE)$
    add_data_table(x = geores, withFilter = FALSE, bandedRows = FALSE)$
    set_col_widths(cols = 2:ncol(geores), widths = 15)$  # width of numeric columns
    set_col_widths(cols = 1, widths = 10)$  # width of County column
    add_font(dims = paste0("A6:", int2col(ncol(geores)), 6), bold = "single") # Region in boldface
wb <- add.header.style(wb, ntime = ntime)


# assemble cities
#########################

cat("\nAssembling cities' results ...")
geores <- assemble.hh.jobs("city", "city_id")
geonames <- merge(county.names, 
                  unique(control.names[, .(county_id, city_id, city_name)]), 
                  by = "county_id")[, county_id := NULL]
geores <- merge(geonames, geores, by = "city_id", all = TRUE)
geores <- geores[!is.na(city_id)]
setcolorder(geores, "County") # make County the first column 

nfreeze <- 3
wb <- wb_add_worksheet(wb, "City")$
    freeze_pane(firstActiveCol = nfreeze + 1, firstActiveRow = 2)$
    add_data_table(x = geores, withFilter = FALSE, bandedRows = FALSE)$
    set_col_widths(cols = (nfreeze + 1):(ncol(geores)+nfreeze), widths = 15)$ # width of numeric columns
    set_col_widths(cols = 1, widths = 10)$  # width of County column
    set_col_widths(cols = 2, widths = 6)$  # width of id columns
    set_col_widths(cols = nfreeze, widths = 20)  # width of name column

wb <- add.header.style(wb, ntime = ntime, nwhite = nfreeze)
#wb <- wb_add_filter(wb, rows = 1, cols = c(2,3))

# assemble targets
#########################

cat("\nAssembling targets' results ...")
geores <- assemble.hh.jobs("target", "target_id")
geonames <- merge(county.names,
                  unique(control.names[, .(county_id, target_id, target_name)]),
                  by = "county_id")[, county_id := NULL]
geores <- merge(geonames, geores, by = "target_id", all = TRUE)[order(County, target_id)]
setcolorder(geores, "County") # make County the first column 

nfreeze <- 3
wb <- wb_add_worksheet(wb, "Target")$
    freeze_pane(firstActiveCol = nfreeze + 1, firstActiveRow = 2)$
    add_data_table(x = geores, withFilter = FALSE, bandedRows = FALSE)$
    set_col_widths(cols = (nfreeze + 1):(ncol(geores)+nfreeze), widths = 15)$ # width of numeric columns
    set_col_widths(cols = 1, widths = 10)$  # width of County column
    set_col_widths(cols = 2, widths = 8)$  # width of id columns
    set_col_widths(cols = nfreeze, widths = 20)  # width of name column

wb <- add.header.style(wb, ntime = ntime, nwhite = nfreeze)
#wb <- wb_add_filter(wb, rows = 1, cols = 1:nfreeze)

# assemble controls
#########################

cat("\nAssembling controls' results ...")
geores <- assemble.hh.jobs("control", "control_id")
geonames <- merge(county.names,
                  unique(control.names[, .(county_id, control_id, target_id, control_name)]),
                  by = "county_id")[, county_id := NULL]
geores <- merge(geonames, geores, by = "control_id", all = TRUE)[order(County, control_id)]
setcolorder(geores, "County") # make County the first column 

nfreeze <- 4
wb <- wb_add_worksheet(wb, "Control")$
    freeze_pane(firstActiveCol = nfreeze + 1, firstActiveRow = 2)$
    add_data_table(x = geores, withFilter = FALSE, bandedRows = FALSE)$
    set_col_widths(cols = (nfreeze + 1):(ncol(geores)+nfreeze), widths = 15)$ # width of numeric columns
    set_col_widths(cols = 1, widths = 10)$  # width of County column
    set_col_widths(cols = 2:(nfreeze - 1), widths = 8)$  # width of id columns
    set_col_widths(cols = nfreeze, widths = 20)  # width of name column

wb <- add.header.style(wb, ntime = ntime, nwhite = nfreeze)
#wb <- wb_add_filter(wb, rows = 1, cols = 1:nfreeze)



# assemble subregs (control_hct_id) results
#########################

cat("\nAssembling control_hct results ...")
#geores <- assemble.hh.jobs("control_hct", "control_hct_id")
geores <- assemble.hh.jobs("subreg", "subreg_id")
setnames(geores, "subreg_id", "control_hct_id")
geonames <- merge(county.names, 
                  unique(control.names[, .(county_id, control_hct_id, control_id, target_id, control_hct_name)]),
                    by = "county_id") [, county_id := NULL]
geores <- merge(geonames, geores, by = "control_hct_id", all = TRUE)[order(County, control_id, control_hct_id)]
setcolorder(geores, "County") # make County the first column 

nfreeze <- 5
wb <- wb_add_worksheet(wb, "Control HCT")$
    freeze_pane(firstActiveCol = nfreeze + 1, firstActiveRow = 2)$
    add_data_table(x = geores, withFilter = FALSE, bandedRows = FALSE)$
    set_col_widths(cols = (nfreeze + 1):(ncol(geores)+nfreeze), widths = 15)$ # width of numeric columns
    set_col_widths(cols = 1, widths = 10)$  # width of County column
    set_col_widths(cols = 2:(nfreeze - 1), widths = 9)$  # width of id columns
    set_col_widths(cols = nfreeze, widths = 20)  # width of name column

wb <- add.header.style(wb, ntime = ntime, nwhite = nfreeze)
#wb <- wb_add_filter(wb, rows = 1, cols = 1:4)


# assemble FAZ results
#########################
cat("\nAssembling FAZ results ...")

geores <- assemble.hh.jobs("faz", "faz_id")

# add faz names and county info
faz.cnty <- fread("data/faz_names.txt")
geores <- merge(faz.cnty[, .(County, faz_id, Name)], geores, by = "faz_id")
setcolorder(geores, c("County", "faz_id", "Name"))
setnames(geores, c("faz_id", "Name"), c("FAZ", "FAZ Description"))

nfreeze <- 3
wb <- wb_add_worksheet(wb, "FAZ")$
    freeze_pane(firstActiveCol = nfreeze + 1, firstActiveRow = 2)$
    add_data_table(x = geores, withFilter = FALSE, bandedRows = FALSE)$
    set_col_widths(cols = (nfreeze + 1):(ncol(geores)+nfreeze), widths = 15)$ # width of numeric columns
    set_col_widths(cols = 1, widths = 10)$  # width of County column
    set_col_widths(cols = 2:(nfreeze - 1), widths = 6)$  # width of id columns
    set_col_widths(cols = nfreeze, widths = 40)  # width of name column

wb <- add.header.style(wb, ntime = ntime, nwhite = nfreeze)
#wb <- wb_add_filter(wb, rows = 1, cols = 1:3)


# assemble TAZ results
#########################
cat("\nAssembling TAZ results ...")

geores <- assemble.hh.jobs("zone", "zone_id")

# add county info
geores <- merge(tazXwalk[, .(County, zone_id)], geores, by = "zone_id")

# replace NAs (caused by missing zone_id in geores (TAZes with no parcels))
na.rows <- c()
for(col in setdiff(colnames(geores), c("County", "zone_id"))){
    idx <- which(is.na(geores[[col]]))
    if(length(idx) > 0){
        geores[idx, (col) := 0]
        na.rows <- c(na.rows, idx)
    }
}
na.rows <- unique(na.rows)
if(length(na.rows) > 0) warning("NAs found for zones ", paste(geores[na.rows, zone_id], collapse = ", "),
                                ". Values set to 0.")

setcolorder(geores, c("County", "zone_id"))
setnames(geores, "zone_id", "TAZ")

nfreeze <- 2
wb <- wb_add_worksheet(wb, "TAZ")$
    freeze_pane(firstActiveCol = nfreeze + 1, firstActiveRow = 2)$
    add_data_table(x = geores, withFilter = FALSE, bandedRows = FALSE)$
    set_col_widths(cols = (nfreeze + 1):(ncol(geores)+nfreeze), widths = 15)$ # width of numeric columns
    set_col_widths(cols = 1, widths = 10)$  # width of County column
    set_col_widths(cols = 2:nfreeze, widths = 6)  # width of id columns

wb <- add.header.style(wb, ntime = ntime, nwhite = nfreeze)
#wb <- wb_add_filter(wb, rows = 1, cols = 1:2)


# assemble 2010 Tract results
#########################
cat("\nAssembling 2010 Tract results ...")

geores <- assemble.hh.jobs("census_tract", "census_tract_id")

# add county info
geores <- merge(tractXwalk10[, .(county_id, census_tract_id, geoid10)], geores, by = "census_tract_id")
geores <- merge(county.names, geores, by = "county_id")[, `:=`(county_id = NULL, census_tract_id = NULL)]
#setcolorder(res, c("County", "zone_id"))
setnames(geores, "geoid10", "Tract")

nfreeze <- 2
wb <- wb_add_worksheet(wb, "Tract")$
    freeze_pane(firstActiveCol = nfreeze + 1, firstActiveRow = 2)$
    add_data_table(x = geores, withFilter = FALSE, bandedRows = FALSE)$
    set_col_widths(cols = (nfreeze + 1):(ncol(geores)+nfreeze), widths = 15)$ # width of numeric columns
    set_col_widths(cols = 1, widths = 10)$  # width of County column
    set_col_widths(cols = 2:nfreeze, widths = 15)  # width of id columns

wb <- add.header.style(wb, ntime = ntime, nwhite = nfreeze)
#wb <- wb_add_filter(wb, rows = 1, cols = 1:2)

# assemble 2020 Tract results
#########################
cat("\nAssembling 2020 Tract results ...")

geores <- assemble.hh.jobs("census_2020_tract", "census_2020_tract_id")

# add county info
geores <- merge(tractXwalk20[, .(county_id, census_2020_tract_id, census_2020_tract_geoid)], 
                geores, by = "census_2020_tract_id")
geores <- merge(county.names, geores, by = "county_id")[, `:=`(county_id = NULL, census_2020_tract_id = NULL)]
setnames(geores, "census_2020_tract_geoid", "Tract2020")

nfreeze <- 2
wb <- wb_add_worksheet(wb, "Tract2020")$
    freeze_pane(firstActiveCol = nfreeze + 1, firstActiveRow = 2)$
    add_data_table(x = geores, withFilter = FALSE, bandedRows = FALSE)$
    set_col_widths(cols = (nfreeze + 1):(ncol(geores)+nfreeze), widths = 15)$ # width of numeric columns
    set_col_widths(cols = 1, widths = 10)$  # width of County column
    set_col_widths(cols = 2:nfreeze, widths = 15)  # width of id columns

wb <- add.header.style(wb, ntime = ntime, nwhite = nfreeze)
#wb <- wb_add_filter(wb, rows = 1, cols = 1:2)



# save results
wb_save(wb, paste0("LUVit-", Sys.Date(), ".xlsx"), overwrite = TRUE)


