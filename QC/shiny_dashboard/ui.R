ui <- function(request) {
  fluidPage(title="", windowTitle="USim QC Dashboard",
  navbarPage(theme = shinytheme("readable"),
           title = div("USim QC Dashboard", HTML("&nbsp;"), bookmarkButton(label = "", id = "bookmark1")), 
           tabPanel("Selection",
                    fluidPage(
                      column(width = 4
                        
                      ), # end column
                      column(width = 4,
                             h4(class="header", checked=NA,
                                tags$b("Select the following to view or update Dashboard content")
                             ),
                             br(),
                             tags$style(type='text/css',
                                        ".selectize-dropdown-content {
                                         max-height: 500px;
                                        }"),
                             selectizeInput(inputId = "init_select_allruns",
                                            label = "Select Runs (minimum 2)",
                                            choices = allruns,
                                            width = "100%",
                                            multiple = TRUE),
                             # uiOutput("init_select_resultsdir"), # dynamic, lists dirs in QC/results
                             br(),
                             actionButton("goButton", "Submit"),
                             br(),
                             br(),
                             br(),
                             verbatimTextOutput("submit_msg")#,
                             # uiOutput('link')
                      ), # end column
                      column(width = 4
 
                      ) # end column
                    ) # end fluidPage
           ), # end tabPanel
           tabPanel("Topsheet",
                    fluidPage(
                      fluidRow(
                        column(width = 12,
                               htmlOutput('ts_currRun'),
                               htmlOutput('ts_desc'),
                               htmlOutput('ts_rest'),
                               br(),
                               selectInput(inputId = "ts_select_year",
                                           label = "Select Comparison Year",
                                           choices = years[2:length(years)],
                                           selected = years[length(years)]),
                               br()
                        )# end column
                      ), # end fluidRow
                    tabsetPanel(type = "tabs",
                                tabPanel(title = "Group: Modellers",
                                         br(),
                                         br(),
                                        fluidPage(
                                          fluidRow(
                                            column(width = 1
                                            ), # end column
                                            column(width = 10,
                                                   tags$head(tags$style(type = "text/css", HTML("th { text-align: center; }"))),
                                                   h4(class="header", checked=NA, tags$b("Households by County")),
                                                   DT::dataTableOutput('tpsht_hh'),
                                                   br(),
                                                   
                                                   h4(class="header", checked=NA, tags$b("Population by County")),
                                                   DT::dataTableOutput('tpsht_pop'),
                                                   br(),
                                                   
                                                   h4(class="header", checked=NA, tags$b("Employment by County")),
                                                   DT::dataTableOutput('tpsht_emp'),
                                                   br(),
                                                   
                                                   h4(class="header", checked=NA, tags$b("Jobs by Sector")),
                                                   DT::dataTableOutput('tpsht_jobs'),
                                                   br(),
                                                   
                                                   h4(class="header", checked=NA, tags$b("Worker Type")),
                                                   DT::dataTableOutput('tpsht_pwtype'),
                                                   br(),
                                                   
                                                   h4(class="header", checked=NA, tags$b("Person Type")),
                                                   DT::dataTableOutput('tpsht_ptype'),
                                                   br(),
                                                   
                                                   h4(class="header", checked=NA, tags$b("Households by Income Group")),
                                                   DT::dataTableOutput('tpsht_hhInc'),
                                                   br(),
                                                   
                                                   h4(class="header", checked=NA, tags$b("Largest RGCs")),
                                                   DT::dataTableOutput('tpsht_rgc'),
                                                   br(),
                                                   
                                                   h4(class="header", checked=NA, tags$b("MICs")),
                                                   DT::dataTableOutput('tpsht_mic'),
                                                   br(),
                                                   
                                                   h4(class="header", checked=NA, tags$b("Key Locations")),
                                                   DT::dataTableOutput('tpsht_splace')
                                            ), # end column
                                            column(width = 1
                                            ) # end column
                                          ) # end fluidRow
                                        ) # end fluidPage
                                ), ### end tabPanel
                                tabPanel(title = "Group: Growth",
                                         br(),
                                         br(),
                                         fluidPage(
                                           fluidRow(
                                             # column(width = 1
                                             # ), # end column
                                             column(width = 12,
                                                    tags$head(tags$style(type = "text/css", HTML("th { text-align: center; }"))),
                                                    h4(class="header", checked=NA, tags$b("Households by County")),
                                                    DT::dataTableOutput('g_tpsht_hh'),
                                                    br(),
                                                    
                                                    h4(class="header", checked=NA, tags$b("Population by County")),
                                                    DT::dataTableOutput('g_tpsht_pop'),
                                                    br(),
                                                    
                                                    h4(class="header", checked=NA, tags$b("Employment by County")),
                                                    DT::dataTableOutput('g_tpsht_emp'),
                                                    br(),
                                                  
                                                    h4(class="header", checked=NA, tags$b("Population by RGCs")),
                                                    DT::dataTableOutput('g_tpsht_rgc_pop'),
                                                    br(),
                                                    
                                                    h4(class="header", checked=NA, tags$b("Employment by RGCs")),
                                                    DT::dataTableOutput('g_tpsht_rgc_emp'),
                                                    br(),
                                                    
                                                    h4(class="header", checked=NA, tags$b("Population by Key Locations")),
                                                    DT::dataTableOutput('g_tpsht_splace_pop'),
                                                    br(),
                                                    
                                                    h4(class="header", checked=NA, tags$b("Employment by Key Locations")),
                                                    DT::dataTableOutput('g_tpsht_splace_emp')
                                             ) # end column
                                           ) # end fluidRow
                                         ) # end fluidPage
                                  
                                ), ### end tabPanel
                                tabPanel(title = "Regional Growth Strategy",
                                         br(),
                                         fluidPage(
                                           fluidRow(
                                             column(width = 12,
                                                    uiOutput("r_tpsht_select_run_ui"),
                                                    h4(class="header", checked=NA, tags$b("Population by County")),
                                                    DT::dataTableOutput("r_tpsht_pop"),
                                                    br(),
                                                    
                                                    h4(class="header", checked=NA, tags$b("Employment by County")),
                                                    DT::dataTableOutput("r_tpsht_emp"),
                                                    br(),
                                                    
                                                    h4(class="header", checked=NA, tags$b("Population for all RGCs")),
                                                    DT::dataTableOutput("r_tpsht_rgc_pop"),
                                                    br(),
                                                    
                                                    h4(class="header", checked=NA, tags$b("Employment for all RGCs")),
                                                    DT::dataTableOutput("r_tpsht_rgc_emp"),
                                                    br()
                                             )# end column 
                                           )# end fluidRow
                                         )# end fluidPage
                                ), ### end tabPanel
                                tabPanel(title = "Other Summaries",
                                         br(),
                                         uiOutput("mk_tpsht_select_run_ui"),
                                         navlistPanel(
                                                      tabPanel("Control Total Mismatch",
                                                               column(width = 1),
                                                               column(width = 11,
                                                                 h3("Control Total Mismatch"),
                                                                 h4(em("(applicable to LUV runs only)")),
                                                                 br(),
                                                                 h4("Summary"),
                                                                 div(DT::dataTableOutput("mk_tpsht_ctm"), style = "width: 95%"), # top level summary,
                                                                 br(),
                                                                 br(),
                                                                 h4("Detail"),
                                                                 br(),
                                                                 div(DT::dataTableOutput("mk_tpsht_ctm_rec"), style = "width: 95%") # data
                                                               ) # end column
                                                              ), # end tabPanel
                                                       tabPanel("Decreases",
                                                                column(width = 1
                                                                       ),# end column
                                                                column(width = 11,
                                                                       helpText("To view results, select a comparison year, run, and the following thresholds"),
                                                                       fluidRow(
                                                                         column(width = 4, numericInput("mk_tpsht_dec_abs", label = h6("Absolute Threshold"), value = 10)),
                                                                         column(width = 4, numericInput("mk_tpsht_dec_per", label = h6("Percent Threshold"), value = 1)),
                                                                         column(width = 4, 
                                                                               h5("Text", style = "color: white"),
                                                                                actionButton("mk_tpsht_dec_goButton", label = "Enter"))
                                                                       ),# end column
                                                                       fluidRow(
                                                                         h3("Decreases"),
                                                                         br(),
                                                                         h4("Summary"),
                                                                         div(DT::dataTableOutput("mk_tpsht_dec"), style = "width: 95%"), 
                                                                         br(),
                                                                         br(),
                                                                         h4("Detail"),
                                                                         br(),
                                                                         div(DT::dataTableOutput("mk_tpsht_dec_rec"), style = "width: 95%")
                                                                       )
                                                                      ) # end column
                                                                ), # end tabPanel
                                                       tabPanel("Share of RGCs (by region & cities)",
                                                                column(width = 1
                                                                ), # end column
                                                                column(width = 11,
                                                                       h3("Allocation to RGCs"),
                                                                       br(),
                                                                       h4("Share of region (%)"),
                                                                       div(DT::dataTableOutput("mk_tpsht_shr_reg"), style = "width: 95%"),
                                                                       br(),
                                                                       h4("Share of cities with RGCs (%)"),
                                                                       div(DT::dataTableOutput("mk_tpsht_shr_city"), style = "width: 95%"),
                                                                       br(),
                                                                       h4("Details on Share of Cities with RGCs (%)"),
                                                                       br(),
                                                                       div(DT::dataTableOutput("mk_tpsht_shr_city_detail"), style = "width: 95%")
                                                                ) # end column
                                                                ), # end tabPanel
                                                      tabPanel("Share of RGCs (by type & rest of region & cities)",
                                                               column(width = 1
                                                               ), # end column
                                                               column(width = 11,
                                                                      h3("Allocation to RGCs (by type & rest of region & cities)"),
                                                                      br(),
                                                                      div(DT::dataTableOutput("mk_tpsht_shr_rest"), style = "width: 95%")
                                                               )# end column
                                                      ), # end tabPanel
                                                      tabPanel("Activity Units",
                                                               column(width = 1
                                                               ), # end column
                                                               column(width = 11,
                                                                      h3("Activity Units"),
                                                                      br(),
                                                                      h4("RGCs"),
                                                                      div(DT::dataTableOutput("mk_tpsht_au_rgc"), style = "width: 95%"),
                                                                      br(),
                                                                      h4("MICs"),
                                                                      div(DT::dataTableOutput("mk_tpsht_au_mic"), style = "width: 95%"),
                                                                      br(),
                                                                      h4("Details"),
                                                                      div(DT::dataTableOutput("mk_tpsht_au_rec"), style = "width: 95%")
                                                               )# end column
                                                      ), # end tabPanel
                                                      tabPanel("Employment in MICs",
                                                               column(width = 1
                                                               ), # end column
                                                               column(width = 11,
                                                                      h3("Employment in MICs"),
                                                                      div(DT::dataTableOutput("mk_tpsht_emp_mic"), style = "width: 95%")
                                                               )# end column
                                                      ), # end tabPanel
                                                      tabPanel("Special Places",
                                                               column(width = 1
                                                               ), # end column
                                                               column(width = 11,
                                                                      h3("Households"),
                                                                      div(DT::dataTableOutput("mk_tpsht_sp_hh"), style = "width: 95%"),
                                                                      br(),
                                                                      h3("Population"),
                                                                      div(DT::dataTableOutput("mk_tpsht_sp_pop"), style = "width: 95%"),
                                                                      br(),
                                                                      h3("Employment"),
                                                                      div(DT::dataTableOutput("mk_tpsht_sp_emp"), style = "width: 95%")
                                                               )# end column
                                                      ), # end tabPanel
                                                      widths = c(2, 10),
                                                      well = FALSE
                                                       # ) # end tabPanel  
                                        ) # end navlistPanel
                                )### end tabPanel
                    ) ### end tabSetPanel
                    ) ### end fluidPage
           ), # end tabPanel
           tabPanel("Run Comparison",
                    fluidPage(
                      fluidRow(
                        column(width = 2,
                               h4(class="header", checked=NA,
                                  tags$b("Select the following to compare runs")
                               ),
                               br(),
                               uiOutput("compare_select_run2_ui"), # dynamic, lists runs based on input from selection page
                               h4("and select the following"),
                               selectInput(inputId = "compare_select_geography",
                                           label = "Geography",
                                           choices = c("TAZ"=1,
                                                       "FAZ"=2,
                                                       "City"=3),
                                           selected = 2),
                               selectInput(inputId = "compare_select_indicator",
                                           label = "Indicator",
                                           choices = c("Total Population"=1,
                                                       "Households"=2,
                                                       "Employment"=3,
                                                       "Residential Units"=4),
                                           selected = 1),
                               conditionalPanel(condition = "(input.compare_select_indicator == 4 | input.compare_select_indicator == 2) && output.strdtavail &&
                                                input.compare_select_geography == 2",
                                                radioButtons("compare_structure_type",
                                                            label = h5("Categories"),
                                                            choices = list("All" = 1, "Single Family" = 2, "Multi-Family" = 3),
                                                            selected = 1)
                                                ),
                               selectInput(inputId = "compare_select_year",
                                           label = "Year",
                                           choices = years,
                                           selected = tail(years, n=1)
                                           ), 
                               br(),
                               helpText("Use the 'Box Select' or 'Lasso Select' option in the scatterplot to select
                                        points and view its location on the map.")
                        ), # end column
                        column(width = 5,
                               plotlyOutput("compare_plot", height = "725px")
                        ), # end column
                        column(width = 5,
                               leafletOutput("compare_map", height = "725px")
                        ) # end column
                      ), # end fluidRow
                      br(),
                      fluidRow(
                        column(width = 2),
                        column(width = 10,
                               DT::dataTableOutput('compare_dt')
                               ) # end column
                      ) # end fluidRow

                    ) # end fluidPage

           ), # end tabPanel
           tabPanel("Growth",
                    fluidPage(
                      fluidRow(
                        column(width = 2,
                               h4(class="header", checked=NA,
                                  tags$b("Select the following to see growth for a selected time period")
                               ),
                               br(),
                               uiOutput("growth_select_run_ui"), # dynamic, lists runs based on input from selection page
                               selectInput(inputId = "growth_select_geography",
                                           label = "Geography",
                                           choices = c("TAZ"=1,
                                                       "FAZ"=2,
                                                       "City"=3),
                                           selected = 2),
                               selectInput(inputId = "growth_select_indicator",
                                           label = "Indicator",
                                           choices = c("Total Population"=1,
                                                       "Households"=2,
                                                       "Employment"=3,
                                                       "Residential Units"=4),
                                           selected = 1),
                               conditionalPanel(condition = "(input.growth_select_indicator == 4 | input.growth_select_indicator == 2) && output.gstrdtavail &&
                                                input.growth_select_geography == 2",
                                                radioButtons("growth_structure_type",
                                                             label = h5("Categories"),
                                                             choices = list("All" = 1, "Single Family" = 2, "Multi-Family" = 3),
                                                             selected = 1)
                                               ),
                               sliderInput(inputId = "growth_select_year",
                                           label = "Time Period",
                                           min = years[1],
                                           max = years[length(years)],
                                           value = c(years[1], years[length(years)]),
                                           step = 1,
                                           sep = ""),
                               br(),
                               helpText("Use the 'Box Select' or 'Lasso Select' option in the scatterplot to select
                                        points and view its location on the map.")
                        ), # end column
                        column(width = 5,
                               plotlyOutput("growth_plot", height = "725px")
                        ), # end column
                        column(width = 5,
                               leafletOutput("growth_map", height = "725px")
                        ) # end column
                      ), # end fluidRow
                      br(),
                      fluidRow(
                        column(width = 2),
                        column(width = 10,
                               DT::dataTableOutput('growth_dt')
                        ) # end column
                      ) # end fluidRow
                    ) # end fluidPage
           ),# end tabPanel
           # tabPanel("Employment by Sector",
           #          fluidPage(
           #            fluidRow(h4(class="header", checked=NA,
           #                        tags$b("Select the following")
           #            )),
           #            fluidRow(
           #              column(width = 5,
           #                     selectInput(inputId = "emp_display",
           #                                 label = "Employment Geography",
           #                                 choices = c("County"=1,
           #                                             "Special Places"=2),
           #                                 selected = 1)
           #              ) # end column
           #            ), # end fluidRow
           #            fluidRow(
           #              column(width = 12,
           #                     htmlOutput("empplots")
           #              ) # end column
           #            ) # end fluidRow
           #          ) # end fluidPage
           # 
           # ), # end tabPanel
           tabPanel("Employment by Sector",
                    fluidPage(
                      fluidRow(
                        h4(class="header", checked=NA, tags$b("Select the following to view estimates by sector from 2014 to 2050")),
                        selectInput(inputId = "empSector_display",
                                    label = "Geography",
                                    choices = c("County"=1,
                                                "Special Places"=2),
                                    selected = 1)
                        ), # end fluidRow
                      fluidRow(
                        column(width = 12,
                               plotlyOutput("empSector_plots")
                        ) # end column
                      ) # end fluidRow
                    ) # end fluidPage
           ), # end tabPanel
           # tabPanel("Time Series",
           #            fluidPage(
           #              fluidRow(
           #                column(width = 2,
           #                       h4(class="header", checked=NA,
           #                          tags$b("Select the following to see time series trends by jurisdiction and indicator")
           #                       ),
           #                       br(),
           #                       selectInput(inputId = "select_tsplots",
           #                                   label = "FAZ Large Area Groups",
           #                                   choices = c("Eastside King (1)"=1,
           #                                               "Eastside King (2)"=2,
           #                                               "Green River"=3,
           #                                               "Seattle and Shoreline"=4,
           #                                               "SE King and King Other"=5,
           #                                               "SW King"=6,
           #                                               "Central, North, and South Kitsap"=7,
           #                                               "Peninsula and Tacoma"=8,
           #                                               "Pierce Other (1)"=9,
           #                                               "Pierce Other (2)"=10,
           #                                               "SW Pierce"=11,
           #                                               "Everett"=12,
           #                                               "NW Snohomish"=13,
           #                                               "Snohomish Other"=14,
           #                                               "SW Snohomish (1)"=15,
           #                                               "SW Snohomish (2)"=16
           #                                   ),
           #                                   selected = 1)
           # 
           #                       ), # end column
           #                column(width = 10,
           #                       htmlOutput("tsplots")
           #                ) # end column
           #              ) # end fluidRow
           #    ) # end fluidPage
           # ), # end tabPanel
           tabPanel("Time Series", ###transfer make-all timeseries to shiny
                    fluidPage(
                      fluidRow(
                               h4(class="header", checked=NA,
                                  tags$b("Select the following to view city estimates by indicator from 2014 to 2050")
                               ),
                               selectInput(inputId = "ts_select_lgarea",
                                           label = "FAZ Large Area Groups",
                                           choices = c("Eastside King (1)",
                                                       "Eastside King (2)",
                                                       "Green River",
                                                       "Seattle and Shoreline",
                                                       "SE King and King Other",
                                                       "SW King",
                                                       "Central, North, and South Kitsap",
                                                       "Peninsula and Tacoma",
                                                       "Pierce Other (1)",
                                                       "Pierce Other (2)",
                                                       "SW Pierce",
                                                       "Everett",
                                                       "NW Snohomish",
                                                       "Snohomish Other",
                                                       "SW Snohomish (1)",
                                                       "SW Snohomish (2)"
                                           ),
                                           selected = 1)
                      ), # end fluidRow
                      br(),
                      fluidRow(
                        column(width = 12,
                               plotlyOutput("ts_plots")
                        ) # end column
                      ) # end fluidRow
                    ) # end fluidPage
           ), # end tabPanel
           tabPanel("Demographics",
                    fluidPage(
                      fluidRow(
                        column(width = 2,
                               h4(class="header", checked=NA,
                                  tags$b("Select the following to visualize demographic indicators")
                               ),
                               br(),
                               uiOutput("demog_Runs"), # dynamic, only runs with demographic indicators will be listed
                               selectInput(inputId = "demog_select_demographic",
                                           label = "Demographic",
                                           choices = c("Persons by 5 year Age Group" = 1,
                                                       "Persons by Age Groups of Interest" = 2,
                                                       "Households by 30-60-90k Income Groups" = 3,
                                                       "Households by New 2014 Income Groups" = 4,
                                                       "Persons Type" = 5),
                                           selected = 1),
                               radioButtons(inputId = "demog_select_format",
                                            label = "Display as",
                                            choices = list("Total Number" = 1,
                                                           "Percentage of Region" = 2),
                                            selected = 1)
                        ), # end column
                        column(width = 10,
                               uiOutput("condDemog_Plot") # dynamic, depending on availability of demographic indicators
                        ) # end column
                      ) # end fluidRow
                    ) # end fluidPage
           ), # end tabPanel
           tabPanel("Development Capacity",
                    fluidPage(
                      fluidRow(h4(class="header", checked=NA,
                                  tags$b("Select the following to view the remaining developable capacity")
                      )),
                      ###check dynamic UI here:
                      fluidRow(
                        column(width = 1,
                               uiOutput("dcap_select_run") # dynamic, only runs with dev cap indicators will be listed
                        ), # end column
                        column(width = 1,
                               selectInput(inputId = "dcap_select_year",
                                           label = "Year",
                                           choices = years,
                                           selected = tail(years, n=1))#, #select the last element of years
                        ), # end column
                        column(width = 2,
                               selectInput(inputId = "dcap_select_geography", 
                                           label = "Geography",
                                           choices = c("TAZ"=1,
                                                       "FAZ"=2,
                                                       "City"=3,
                                                       "Growth Center"=4),
                                           selected = 2)
                        ), # end column
                        column(width = 8,
                               uiOutput("condDcap_msg")
                        ) # end column
                      ), # end fluidRow
                      fluidRow(
                        column(width = 4,
                               leafletOutput("dcap_total_map", height = "800px")
                        ), # end column
                        column(width = 4,
                               leafletOutput("dcap_res_map", height = "800px")
                        ), # end column
                        column(width = 4,
                               leafletOutput("dcap_nonres_map", height = "800px")
                        ) # end column
                      ) # end fluidRow
                   ) # end fluidPage
           ), # end tabPanel
           tabPanel("Vacancy",
                    fluidPage(
                      column(width = 2,
                             h4(class="header", checked=NA, tags$b("Vacancy by County")),
                             uiOutput("vacancy_select_run"),
                             br(),
                             uiOutput("vacancy_select_bldgtype"),
                             br(),
                             h6(class="header", checked=NA, tags$b("Building Type Key")),
                             htmlOutput("vac_key")
                      ), # end column
                      column(width = 10,
                             h5(class="header", checked=NA, tags$b("Units or Square Feet")),
                             fluidRow(plotlyOutput("vacancy_estimate_plot", height = "400px")), # end fluidRow
                             h5(class="header", checked=NA, tags$b("Rate")),
                             fluidRow(plotlyOutput("vacancy_rate_plot", height = "400px"))# end fluidRow
                      ) # end column
                    ) # end fluidPage
                    ), # end tabPanel
           fluid = TRUE
)# end navbarPage
)# end fluidPage
}

 
