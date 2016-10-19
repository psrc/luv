navbarPage("LUV QC Dashboard",
           tabPanel("Index",
                      if(make){
                        includeHTML(file.path(result.dir,'index.html'))
                      } else {
                        includeHTML('C:/Users/Christy/Desktop/luv/QC/results/run99/index.html')
                      }
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
           ),
           fluid = TRUE
)# end navbarPage
