ui <- function(request) {
  fluidPage(title="", windowTitle="LUV QC Dashboard",
  navbarPage(theme = shinytheme("readable"),
           title = div("LUV QC Dashboard", HTML("&nbsp;"), bookmarkButton(label = "", id = "bookmark1")), 
           tabPanel("Selection",
                    fluidPage(
                      column(width = 4
                        
                      ), # end column
                      column(width = 4,
                             h4(class="header", checked=NA,
                                tags$b("Select the following to view or update Dashboard content")
                             ),
                             br(),
                             selectInput(inputId = "init_select_server",
                                         label = "Server",
                                         choices = structure(c(1:length(base)), names=names(base))
                             ),
                             uiOutput("init_select_run1"), # dynamic, lists runs based on selected modelsrv
                             uiOutput("init_select_run2all"), # dynamic, lists runs based on selected modelsrv
                             uiOutput("init_select_resultsdir"), # dynamic, lists dirs in QC/results
                             br(),
                             actionButton("goButton", "Submit"),
                             br(),
                             br(),
                             br(),
                             verbatimTextOutput("submit_msg"),
                             uiOutput('link'),
                             helpText("*affects Index file, and the Employment by Sector and Time Series tabs")
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
                                  
                                ) ### end tabPanel
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
                               conditionalPanel(condition = "(input.compare_select_indicator == 4 | input.compare_select_indicator == 2) && output.strdtavail",
                                                radioButtons("compare_structure_type",
                                                            label = h5("Categories"),
                                                            choices = list("All" = 1, "Single Family" = 2, "Multi-Family" = 3),
                                                            selected = 1)
                                                ),
                               selectInput(inputId = "compare_select_year",
                                           label = "Year",
                                           choices = years,
                                           selected = tail(years, n=1)), #select the last element of years
                               br(),
                               helpText("Use the 'Box Select' or 'Lasso Select' option in the scatterplot to select
                                        points and view its location on the map.")
                        ), # end column
                        column(width = 5,
                               plotlyOutput("compare_plot", height = "925px")
                        ), # end column
                        column(width = 5,
                               leafletOutput("compare_map", height = "925px")
                        ) # end column
                      ) # end fluidRow

                    ) # end fluidPage

           ), # end tabPanel
           tabPanel("Growth",
                    fluidPage(
                      fluidRow(
                        column(width = 2,
                               h4(class="header", checked=NA,
                                  tags$b("Select the following to see growth since 2014")
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
                               conditionalPanel(condition = "(input.growth_select_indicator == 4 | input.growth_select_indicator == 2) && output.gstrdtavail",
                                                radioButtons("growth_structure_type",
                                                             label = h5("Categories"),
                                                             choices = list("All" = 1, "Single Family" = 2, "Multi-Family" = 3),
                                                             selected = 1)
                                               ),
                               sliderInput(inputId = "growth_select_year",
                                           label = "End Year",
                                           min = years[2],
                                           max = years[length(years)],
                                           value = years[length(years)],
                                           step = 5,
                                           sep = ""),
                               br(),
                               helpText("Use the 'Box Select' or 'Lasso Select' option in the scatterplot to select
                                        points and view its location on the map.")
                        ), # end column
                        column(width = 5,
                               plotlyOutput("growth_plot", height = "925px")
                        ), # end column
                        column(width = 5,
                               leafletOutput("growth_map", height = "925px")
                        ) # end column
                      ) # end fluidRow
                    ) # end fluidPage
           ),# end tabPanel
           tabPanel("Employment by Sector",
                    fluidPage(
                      fluidRow(h4(class="header", checked=NA,
                                  tags$b("Select the following")
                      )),
                      fluidRow(
                        column(width = 5,
                               selectInput(inputId = "emp_display",
                                           label = "Employment Geography",
                                           choices = c("County"=1,
                                                       "Special Places"=2),
                                           selected = 1)
                        ) # end column
                      ), # end fluidRow
                      fluidRow(
                        column(width = 12,
                               htmlOutput("empplots")
                        ) # end column
                      ) # end fluidRow
                    ) # end fluidPage
       
           ), # end tabPanel
           tabPanel("Time Series",
                      fluidPage(
                        fluidRow(
                          column(width = 2,
                                 h4(class="header", checked=NA,
                                    tags$b("Select the following to see time series trends by jurisdiction and indicator")
                                 ),
                                 br(),
                                 selectInput(inputId = "select_tsplots",
                                             label = "FAZ Large Area Groups",
                                             choices = c("Eastside King (1)"=1,
                                                         "Eastside King (2)"=2,
                                                         "Green River"=3,
                                                         "Seattle and Shoreline"=4,
                                                         "SE King and King Other"=5,
                                                         "SW King"=6,
                                                         "Central, North, and South Kitsap"=7,
                                                         "Peninsula and Tacoma"=8,
                                                         "Pierce Other (1)"=9,
                                                         "Pierce Other (2)"=10,
                                                         "SW Pierce"=11,
                                                         "Everett"=12,
                                                         "NW Snohomish"=13,
                                                         "Snohomish Other"=14,
                                                         "SW Snohomish (1)"=15,
                                                         "SW Snohomish (2)"=16
                                             ),
                                             selected = 1)

                                 ), # end column
                          column(width = 10,
                                 htmlOutput("tsplots")
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
           fluid = TRUE
)# end navbarPage
)# end fluidPage
}

 
