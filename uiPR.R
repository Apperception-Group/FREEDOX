tabsetPanel(
  
  # Data Tab       
  tabPanel("Profile Data Input/Processing",
           fluidPage(
             fluidRow(
               
               # Data Tab Left Panel
               column( width = 6,
                 tabsetPanel(
                   tabPanel("Data Input",
                            fluidRow(
                              column(7, 
                                     fluidRow(
                                       column(6,
                                              fileInput( "prFls",
                                                         "Select profile data files:",
                                                         accept = c(".json, .xml, .csv, .xls, .xlsx"),
                                                         multiple = TRUE)
                                       ),
                                       column(6,
                                              fileInput( "prVolt",
                                                         "Select FREEDOX file:",
                                                         accept = c(".volt"),
                                                         multiple = FALSE)
                                       )
                                     ),
                                     fluidRow(
                                       column(6, textInput("prName", "Profile name:")),
                                       column(6, numericInput("prStr", "Start (mm):", value = -10))
                                     ),
                                     fluidRow(
                                       column(6, numericInput("prStpSiz", "Step size (mm):", value = 2)),
                                       column(6, sliderInput("prSmthSpn", "Loess smoothing span:",
                                                             min = 0.01, max = .1, value = 0.05))
                                     ),
                                     wellPanel(
                                       h4("Profile Details:"),
                                       textOutput("prName"),
                                       textOutput("prFlsDesCntType"),
                                       textOutput("prFlsDesScan"),
                                       textOutput("prDesStr"),
                                       textOutput("prDesStp"),
                                       textOutput("prDesLen"),
                                       br(),
                                       actionButton("prCompile", "Compile Profile",
                                                icon("list"), 
                                                style="color: #000000; background-color: #F1BE48")
                                     )
                              ),
                              column(5, 
                                     h4("Input files:"),
                                     div(dataTableOutput("prViewRawFls"), style = "font-size:70%")
                              )
                            )
                    ),
                   tabPanel("Modify Baseline",
                            fluidRow(
                              column(6, 
                                     selectInput("prBaseMeth", "Select method: ",
                                                 choices = c("Mod_Slowey_Dipasquale", "Secondary_Convex_Hull"))
                                     ),
                              column(6, uiOutput("prModRowOut"))
                            ),
                            conditionalPanel("input.prBaseMeth == 'Mod_Slowey_Dipasquale'",
                              fluidRow(
                                column(4, numericInput("prBase1", "distB-A:", value = 25)),
                                column(4, numericInput("prBase2", "distE-D:", value = 25)),
                                column(4, numericInput("prBase3", "parD:", value = 3))
                              ),
                              fluidRow(
                                column(4, numericInput("prBase4", "distC-A:", value = 20)),
                                column(4, numericInput("prBase5", "distF-D:", value = 30)),
                                column(4, numericInput("prBase6", "wgtInt:", value = .005))
                              )
                            ),
                            conditionalPanel("input.prBaseMeth == 'Secondary_Convex_Hull'",
                              fluidRow(
                                column(4, numericInput("prBase7", "St_Pot:", value = -0.2)),
                                column(4, h6(".")),
                                column(4, h6("."))
                              ),
                              fluidRow(
                                column(4, h6(".")),
                                column(4, h6(".")),
                                column(4, h6("."))
                              )
                            ),
                            div(style = "padding: 0px 0px; margin:0%",
                              plotOutput("prBasePlot", width = "100%", height = "290px")
                            ),
                            actionButton("prModBase", "Modify Baseline",
                                         icon("wrench"), 
                                         style="color: #000000; background-color: #F1BE48")
                    )
                 )
               ),
               
               # Data Tab Right Panel
              column(6,
                     uiOutput("prPickScan"),
                     div(style = "padding: 0px 0px; margin:0%",
                         plotOutput("prPlotRawData", height = "500px")
                     )
              )
           )
    )
  ),
  
  # Visualization Tab       
  tabPanel("Profile Visualization",
           fluidPage(
             
             # Visualize Left Panel
             column(width = 2,
                    selectInput("prSWI", "Show SWI:", choices = c("Yes", "No")),
                    
                    selectInput("prContour", "Show Contours:", choices = c("Yes", "No"), selected = "No"),
                    conditionalPanel("input.prContour == 'Yes'",
                                     colourpicker::colourInput("prConCol", "Contour Color", value = "#000000",
                                                 allowTransparent = TRUE),
                                     numericInput("prConLwd", "Line width", min = 0.1, max = 4, value = 1) 
                                     
                    ),
                    
                    selectInput("prHMCols", "Change Heat map colors:", choices = c("Yes", "No"), selected = "No"),
                    conditionalPanel("input.prHMCols == 'Yes'",
                      fluidRow(
                        column(6, colourpicker::colourInput("prHM1", "Low", value = "#FFFFFF")
                        ),
                        column(6, colourpicker::colourInput("prHM2", "MedLow", value = "#080875")
                        )
                      ),
                      fluidRow(
                        column(6, colourpicker::colourInput("prHM3", "MedHigh", value = "#FF0000")
                        ),
                        column(6, colourpicker::colourInput("prHM4", "High", value = "#FFFF00")
                        )
                      )
                    ),
                    h4("Download Options:"),
                    selectInput("prHMType", "File Type", c("pdf", "png")),
                    downloadButton('prHMPlot', "Download"),
                    tags$style(type='text/css',
                               "#prHMPlot {color: #000000; background-color: #F1BE48}")
                    
             ),
             
             # Visualize Center Panel
             column( width = 5,
                     tabsetPanel( id = "prfHeatTbs",
                                  tabPanel("Profile Heat Map",
                                           plotOutput("prHeatMap2", height = "70vh", width = "100%", click = "prHM2_clk")
                                  ),
                                  tabPanel("prfSpeciesPlot",
                                           plotOutput("prSpeciesPlot", height = "70vh", width = "100%")
                                  )
                      )
             ),
             
             # Visualize Right Panel
             column( width = 5,
                     tabsetPanel( id = "prfTabs",
                                  tabPanel("Selected Scan Plot",
                                               plotOutput("prSlice1", height = "70vh", width = "100%")
                                  ),
                                  tabPanel("Selected Species Plot",
                                               plotOutput("prSlice2", height = "70vh", width = "100%")
                                  ),
                                  tabPanel("Profile Data Table",
                                           rHandsontableOutput("prfDataTbl", height = "70vh")
                                  )
                     ),
                     fluidRow(
                       column(6,
                              actionButton("prfAddFirst", "Record Species Profile",
                                           icon("wrench"), 
                                           style="color: #000000; background-color: #F1BE48")
                       ),
                       column(6,
                              NULL
                       )
                     )
                     
            )
           )
  )
)
