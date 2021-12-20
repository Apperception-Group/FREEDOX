# This block of code recognizes when the user inputs files and returns a dataframe
# of their names and file path to their temporary copies on the server
prFls <- reactive({
  
  prData <- NULL
  
  inFls <- input$prFls
  
  if(is.null(inFls))
    return(NULL) 
  else(return(inFls))

})

#Create Reactive Values to hold data for the app
prData <- reactiveValues(data = list(NULL),
                         speciesData = NULL,
                         prDepth = NULL
                         )

observeEvent(c(input$input$prStr, input$prStpSiz, prFls()), {
  # browser()
  req(input$prStr, input$prStpSiz, !is.null(prFls()))
  prData$prDepth <- seq(input$prStr, ((nrow(prFls())-1)*input$prStpSiz)+input$prStr, by = input$prStpSiz)
  prData$speciesData <- data.frame(Depth_mm = prData$prDepth)
})

# This block of code splits the first input file name by "." to get the file type
prFlType <- reactive({
  strsplit(as.character(prFls()[1,1]),"\\.")[[1]][2]
})

# This block of code parses the first input file to determine scan type
prScanType <- reactive({
  if(prFlType() == "json") {
    foo <- fromJSON(prFls()[1,4])$techName
    foo <- gsub(" ", "_", foo)
  }
  if(prFlType() == "XML") {
    foo <- xmlParse(prFls()[1,4])
    foo <- xmlToList(foo)
    foo <- as.character(foo$experiment$.attrs[2])
    foo <- gsub(" ", "_", foo)
  }
  if(prFlType() == "csv") {
    foo <- read_csv(prFls()[1,4], skip = 10, n_max = 1)
    foo <- strsplit(as.character(foo), ": ")[[1]][2]
    foo <- gsub(" ", "_", foo)
  }
  return(foo)
})

# This block of code creates a reactive dataframe of the input file names, depths, and scan type
prFlsDF <- reactive({
  # browser()
  req(prFls(), !is.null(prData$prDepth))
  data.frame(ID = 1:length(prFls()[,1]), Depth_mm = prData$prDepth, File = prFls()[,1])
})

# This block of code renders input file dataframe to the UI
output$prViewRawFls <- renderDataTable(
  prFlsDF(),
  options = list(
    paging = FALSE,
    scrollY = 450,
    searching = FALSE
  )
)

# These lines of code output profile description text to the UI
output$prName <- renderText({
  paste0("Name: ", input$prName)
})
output$prFlsDesCntType <- renderText({
  paste0("Files: ", nrow(prFls()), " .", prFlType(), " files")
})
output$prFlsDesScan <- renderText({
  paste0("Scan type: ", prScanType())
})
output$prDesStr <- renderText({
  paste0("Start: ", input$prStr, " mm")
})
output$prDesStp <- renderText({
  paste0("Stop: ", max(prData$prDepth), " mm")
})
output$prDesLen <- renderText({
  paste0("Length: ", max(prData$prDepth)-min(prData$prDepth), " mm")
})

# This block of code activates when the user presses "Compile" on the data input section.
# It iteratively parses the input files into a list, one entry for each file.
# Parsing is done by a seperate function for each file-scan combination.
# During parsing, SW forward and reverse are combined into a resultant wave.
# Parsing buils new columns for median (last 5 scans in Bursts, or exact for single scans),
# Smoothed loess regression, and convex hull baseline.
observeEvent(input$prCompile, {
  
  if(prFlType() == "json" & prScanType() == "Cyclic_Voltammetry_Burst") {
    rah <- lapply(prFls()[,4], parseCVB, span = input$prSmthSpn)
  }
  
  if(prFlType() == "json" & prScanType() == "Square_Wave_Voltammetry_Burst") {
    rah <-lapply(prFls()[,4], parseSWB, span = input$prSmthSpn)
  }
  
  if(prFlType() == "XML" & prScanType() == "Cyclic_Voltammetry") {
    rah <- lapply(prFls()[,4], parseCV, span = input$prSmthSpn)
  }
  
  if(prFlType() == "XML" & prScanType() == "Square_Wave_Voltammetry") {
    rah <- lapply(prFls()[,4], parseSW, span = input$prSmthSpn)
  }
  
  if(prFlType() == "csv" & prScanType() == "Cyclic_Voltammetry") {
    rah <- lapply(prFls()[,4], parseCV2, span = input$prSmthSpn)
  }
  
  if(prFlType() == "csv" & prScanType() == "Square_Wave_Voltammetry") {
    rah <- lapply(prFls()[,4], parseSW2, span = input$prSmthSpn)
  }
  
  prData$data <- rah  
})

# This block of code renders the compiled data to the UI
output$prCompiledData <- renderPrint({
  print(prData$data)
})

# This block of code pre-renders a slider input box that allows user to select a data file
# from the list of uploaded files
output$prPickScan <- renderUI({
  
  sliderInput("prScan", "Select ID of data file to view:",
              min = 1, max = nrow(prFls()), step = 1, value = 1,
              animate = TRUE, width = "100%")
})

# This block of code plots raw data, smoothed raw data, and chull
output$prPlotRawData <- renderPlot({
  if(prScanType() == "Cyclic_Voltammetry_Burst" | prScanType() == "Square_Wave_Voltammetry_Burst") {
    
    data <- prData$data[[as.numeric(input$prScan)]]
    
    layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
    
    plot(data$potential, data[,7], col = 'gray50', type = 'l', lwd = 2,
         main = paste0(input$prName, "; Depth = ", prData$prDepth[as.numeric(input$prScan)], "mm"),
         ylab = "I (A)",
         xlab = "E(V vs. Ag/AgCl)")
    for( i in 8:11) {
      lines(data$potential, data[,i], col = 'gray50', lwd = 2)
    }
    lines(data$potential, data$avg, col = 1, lwd = 2)
    lines(data$potential, data$smth, col = 2, lwd = 2)
    lines(data$potential, data$base, col = 3, lwd = 2)
    legend("topright", legend = c("Raw Data", "Mean", "Loess Reg.", "Baseline"), col = c('gray50', 1,2,3), lty = 1, lwd = 2)
    
    plot(data$potential, data$blr, type = 'l', lwd = 2,
         main = "Current above baseline",
         ylab = "I above baseline (A)",
         xlab = "E(V vs. Ag/AgCl)")

  } else {
    
    data <- prData$data[[as.numeric(input$prScan)]]
    
    layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE))
    
    plot(data$potential, data$avg, col = 1, lwd = 2, type = 'l',
         main = paste0(input$prName, "; Depth = ", prData$prDepth[as.numeric(input$prScan)], "mm"),
         ylab = "I (A)",
         xlab = "E(V vs. Ag/AgCl)")
    lines(data$potential, data$smth, col = 2, lwd = 2)
    lines(data$potential, data$base, col = 3, lwd = 2)
    legend("topright", legend = c("Raw Data", "Mean", "Loess Reg.", "Baseline"), col = c('gray50', 1,2,3), lty = 1, lwd = 2)
    
    plot(data$potential, data$blr, type = 'l', lwd = 2,
         main = "Current above baseline",
         ylab = "I above baseline (A)",
         xlab = "E(V vs. Ag/AgCl)")
  }
  
})

output$prModRowOut <- renderUI({
  
  pickerInput(
    inputId = "prModRows", 
    label = "Select scans to modify:", 
    choices = 1:nrow(prFls()),
    selected = 1:nrow(prFls()),
    multiple = TRUE,
    options = list(
      `actions-box` = TRUE, 
      size = 10,
      `selected-text-format` = "count > 3"
    )
  )
  
})

# This block of code watches for the user to click the baseline modification
# action button. It then applies whatever baseline modification function the
# user has selected to the prRawData list.
observeEvent(input$prModBase, {
  
  dat <- prData$data[as.numeric(input$prModRows)]

  if(input$prBaseMeth == "Mod_Slowey_Dipasquale") {
    pars <- c(input$prBase1, input$prBase2, input$prBase3, input$prBase4, input$prBase5, input$prBase6)
    temp <- lapply(dat, Mod_SD, pars = pars)
  }
  
  if(input$prBaseMeth == "Secondary_Convex_Hull") {
    pars <- c(input$prBase7)
    temp <- lapply(dat, Mod_CH, pars = pars)
  }
  
  prData$data[as.numeric(input$prModRows)] <- lapply(temp, "[[", 1)
  
})

output$prBasePlot <- renderPlot({
  
  dat <- prData$data[[as.numeric(input$prScan)]]
  
  if(input$prBaseMeth == "Mod_Slowey_Dipasquale") {
    pars <- c(input$prBase1, input$prBase2, input$prBase3, input$prBase4, input$prBase5, input$prBase6)
    Mod_SD(dat, pars = pars)[[2]]
  }
  
  if(input$prBaseMeth == "Secondary_Convex_Hull") {
    pars <- c(input$prBase7)
    Mod_CH(dat, pars = pars)[[2]]
  }
  
})

# This block of code builds the color ramp function for the profile heatmap
prColorFunc <- reactive({
  if(is.null(input$prHM1)){
    colfunc <- colorRampPalette(c("#FFFFFF", "#080875", "#FF0000", "#FFFF00"))
    colfunc(4)
  } else {
    colfunc <- colorRampPalette(c(input$prHM1, input$prHM2, input$prHM3, input$prHM4))
    colfunc(4)
  }
})

# Heat map stuff
prHMdata <- reactive({
  numPots <- length(prData$data[[1]]$potential)
  datx <- seq(max(prData$data[[1]]$potential), min(prData$data[[1]]$potential), length.out = numPots)
  datz <- rbindlist(prData$data)[,c("blr")]
  daty <- unlist(lapply(prData$prDepth,function(x) rep(x,numPots)))
  p.hm <- data.frame(x = datx, y = daty, z = datz)
  names(p.hm) <- c("x", "y", "z")
  p.hm
  
})

## Set up buffer, to keep the click.  
prHM2clk <- reactiveValues(singleclick = NULL)
## Save the click, once it occurs.
observeEvent(input$prHM2_clk, {
  dat <- prHMdata()
  pts <- nearPoints(dat, input$prHM2_clk, threshold = 50, maxpoints = 5)
  prHM2clk$singleclick <- list(x = pts[1,1], y = pts[1,2])
})

prHM2clk2 <- reactive({
  if(is.null(prHM2clk$singleclick)){list(x = max(prData$data[[1]]$potential), y = 0)}
  else{prHM2clk$singleclick}
})

prHeatMap2 <- reactive({

  p.hm <- prHMdata()

  P2 <- ggplot(data = p.hm, aes(x = x, y = y)) +
    geom_tile(aes(fill = z * 1000000))

  if(input$prContour == "Yes") {
    P2 <- P2 +
      geom_contour(aes(z = z), color = input$prConCol, size = input$prConLwd)
  }

  if(input$prSWI == "Yes") {
    P2 <- P2 +
      geom_hline(yintercept = 0, size = 1.1) +
      annotate("text", label = "SWI", x = max(p.hm$x - .05), y = .3, size = 5)
  }

  P2 +
    geom_vline(xintercept = prHM2clk2()$x, color = "gray50", linetype = 2, size = 1.1) +
    geom_hline(yintercept = prHM2clk2()$y, color = "gray50", linetype = 2, size = 1.1) +
    xlab("E(V vs. Ag/AgCl)") +
    scale_x_continuous(breaks = round(seq(min(p.hm$x), max(p.hm$x), by = .2), 1), expand = c(0,0)) +
    ylab("Depth (mm)") +
    scale_y_continuous(trans = "reverse", breaks = seq(min(p.hm$y), max(p.hm$y), by = 5), expand = c(0,0)) +
    scale_fill_gradientn(colors = prColorFunc(), name = "i-B (uA)") +
    theme_bw()+
    theme(legend.position=c(.95,.1),
          plot.margin=unit(c(0,0,0,0), "mm"))

})

prSlice1 <- reactive({
  
  temp <- which(prData$prDepth == prHM2clk2()$y)
  
  dat2 <- prData$data[[temp]]
  
  ggplot(data = dat2, aes(x = potential, y = blr*1000000)) +
    geom_line(size = 1.1) +
    ylab("i-B (uA)") +
    scale_x_continuous(expand=c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) +
    theme_classic()

})

prSlice2 <- reactive({
  
  pots <- seq(max(prData$data[[1]]$potential), min(prData$data[[1]]$potential),
              length.out = length(prData$data[[1]]$potential))
  temp <- which(pots == prHM2clk2()$x)
  dat3 <- rbindlist(lapply(prData$data,"[", temp, "blr", drop=FALSE))
  dat3$depth <- prData$prDepth
  
  foo <- ggplot(data = dat3, aes(x = depth, y = blr*1000000)) +
    geom_line(size = 1.1) +
    ylab("i-B (uA)") +
    scale_x_continuous(trans = "reverse", expand = c(0,0)) + 
    scale_y_continuous(expand=c(0,0)) +
    coord_flip()+
    theme_classic()
  
  list(data = dat3, plot = foo)
})

prSpeciesPlot <- reactive({
  
  req(prData$speciesData)
  
  # browser()
  
  plot.data <- pivot_longer(prData$speciesData, cols=-1, names_to = "Species", values_to = "current_uA")

  ggplot(plot.data, aes(x = current_uA, y = Depth_mm, color = Species)) +
    scale_y_reverse() +
    geom_path(size = 1.5) +
    theme_classic()
  
})

observeEvent(input$prfAddFinal, {
  
  # browser()
  
  old.names <- names(prData$speciesData)
  new.species <- cbind(prData$speciesData, prSlice2()$data$blr)
  names(new.species) <- c(old.names, paste0(input$prfAddName, "_uA"))
  prData$speciesData <- data.frame(new.species)
  
  # removeModal()
  toggleModal(session, "prfAddDataMdl", toggle = "toggle")
  
})

observeEvent(input$prfDataTbl, {
  req(input$prfDataTbl)
  # browser()
  prData$speciesData <- hot_to_r(input$prfDataTbl)
})

# Renderers!

# Renders the species data table
output$prfDataTbl <- renderRHandsontable({
  rhandsontable(prData$speciesData, readOnly = FALSE, contextMenu = TRUE)
})

# Plot render calls
output$prHeatMap2 <- renderPlot({
  prHeatMap2()
  })

output$prSpeciesPlot <- renderPlot({
  prSpeciesPlot()
  })

output$prSlice1 <- renderPlot({
  prSlice1()
  })

output$prSlice2 <- renderPlot({
  prSlice2()$plot
  })

# This block of code creates a download handler and downloads a plot
output$prHMPlot <- downloadHandler(
  
  filename =  function() {
    paste(paste0(input$prName,"_heatmap"), input$prHMType, sep=".")
  },
  
  content = function(file) {
    ggsave(file, plot = prHeatMap1(), width = 10, height = 7, device = input$prHMType)
  }
)