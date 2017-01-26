navbarPage(theme = shinytheme("readable"),
           "LUV QC Dashboard",
           tabPanel("Index",
                        includeHTML(file.path(result.dir,'index.html'))
           ), # end tabPanel
           tabPanel("Run Comparison",
                    fluidPage(
                      fluidRow(
                        column(width = 2,
                               h4(class="header", checked=NA,
                                  tags$b("Select the following to compare runs")
                               ),
                               br(),
                               selectInput(inputId = "compare_select_run2",
                                           label = h4(paste0("Compare ",`runname1`," with")),
                                           choices = runnames2
                               ),
                               h4("and select the following"),
                               selectInput(inputId = "compare_select_geography",
                                           label = "Geography",
                                           choices = c("TAZ"=1,
                                                       "FAZ"=2,
                                                       "City"=3),
                                           selected = 1),
                               selectInput(inputId = "compare_select_indicator",
                                           label = "Indicator",
                                           choices = c("Total Population"=1,
                                                       "Households"=2,
                                                       "Employment"=3,
                                                       "Residential Units"=4),
                                           selected = 1),
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
                               selectInput(inputId = "growth_select_run",
                                           label = "Run", 
                                           choices = runs
                               ),
                               selectInput(inputId = "growth_select_geography",
                                           label = "Geography",
                                           choices = c("TAZ"=1,
                                                       "FAZ"=2,
                                                       "City"=3),
                                           selected = 1),
                               selectInput(inputId = "growth_select_indicator",
                                           label = "Indicator",
                                           choices = c("Total Population"=1,
                                                       "Households"=2,
                                                       "Employment"=3,
                                                       "Residential Units"=4),
                                           selected = 1),
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
           navbarMenu("Employment by Sector",
                      tabPanel("County",
                              includeHTML('www/qc_ts_emp_cnty.html')
                      ), # end tabPanel
                      tabPanel("Special Places",
                              includeHTML('www/qc_ts_emp_sp.html')
                      ) # end tabPanel
           ), # end navbarMenu
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
                               # selectInput(inputId = "demog_select_run",
                               #             label = "Run",
                               #             choices = runs
                               #             ),
                               uiOutput("demog_Runs"),
                               selectInput(inputId = "demog_select_demographic",
                                           label = "Demographic",
                                           choices = c("Persons by 5 year Age Group" = 1),
                                           selected = 1),
                               radioButtons(inputId = "demog_select_format", 
                                            label = "Display as",
                                            choices = list("Total Number" = 1, 
                                                           "Percentage of Region" = 2), 
                                            selected = 1)
                        ), # end column
                        column(width = 10,
                               plotlyOutput("demog_plot", height = "800px")
                        ) # end column
                      ) # end fluidRow
                    ) # end fluidPage
           ), # end tabPanel
           fluid = TRUE
)# end navbarPage
