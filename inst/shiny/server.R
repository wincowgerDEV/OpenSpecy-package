#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny

#Libraries ----
library(shiny)
library(ggplot2)
library(smoother)
library(dplyr)
library(plotly)
library(data.table)
library(signal)
library(viridis)
library(shinyjs)
library(shinythemes)
library(DT)
library(rdrop2)
library(shinyhelper)
library(hyperSpec)
library(readr)
library(tidyr)
library(hexView)
library(curl)
#library(OpenSpecy)


#Required Data ----
load("data/Costs.RData")
load("data/Donations.RData")
load("data/FTIRLibrary.RData")
load("data/FTIRLibraryMetadata.RData")
load("data/RamanLibrary.RData")
load("data/RamanLibraryMetadata.RData")
load("data/FTIRLibraryPeak.RData")
load("data/RamanLibraryPeak.RData")
load("data/testdata.RData")
WavelengthApprox <- seq(0,6000, by = 0.1)


#Check for Auth Tokens and setup ----
droptoken <- file.exists("data/droptoken.rds")

outputDir <- "Spectra"

if(droptoken){
  drop_auth(rdstoken = "data/droptoken.rds")
}

#Required functions ----

human_timestamp <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

#Function adjusts spectra with negative intensity values so that all intensity values are greater than 1.
adjust_negative <- function(x){
  if(min(x) < 1){
    x + abs(min(x)) + 1
  }
  else{
    x
  }
}


#Function is the imodpolyfit function described by Zhao 2007.
iModPolyFit <- function(x, y, n) {
  OriginalWavelengths <- x #Need the original wavelengths
  dev_prev = 0# DEV_PREV is the standard deviation residuals for the last
  # iteration of polyfit. Set initially to 0.
  first_iter = TRUE
  criteria_met = FALSE
  while (!criteria_met) {

    #Predict the intensity using the polynomial of specified length.
    paramVector = lm(y~stats::poly(x,n, raw = TRUE))

    residual = paramVector$residuals

    mod_poly = paramVector$fitted.values

    dev_curr = sd(residual)

    #Remove peaks.
    if (first_iter){
      Peaks <- c()
      for (i in 1:length(y)) {
        if (y[i] > mod_poly[i] + dev_curr){
          Peaks <- c(Peaks,i)
        }
      }
      y <- y[-Peaks]
      mod_poly = mod_poly[-Peaks]
      x <- x[-Peaks]
      first_iter = FALSE
    }

    #Replace data with lower value if polynomial is lower.
    for(j in 1:length(y)) {
      if (mod_poly[j] + dev_curr > y[j]){
        y[j] = y[j]
      }
      else {
        y[j] = mod_poly[j]
      }
    }

    #Test criteria.
    criteria_met <- abs((dev_curr - dev_prev)/dev_curr) <= 0.05

    #Approximate the intensity back to the original wavelengths, allows below the peak to be interpolated.
    if(criteria_met) {
      return(unname(unlist(approx(x, y, xout = OriginalWavelengths, rule = 2, method = "linear", ties=mean)[2])))
    }

    #Update previous residual metric.
    dev_prev = dev_curr
  }
}

#Min max normalization
minmax <- function(x) {
  (x - min(x))/(max(x)-min(x))
}

# This is the actual server functions, all functions before this point are not reactive.
server <- shinyServer(function(input, output, session) {

  #For desktop version of the app.
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }

  #File sharing functions ----
  #Share if share is selected on upload
  observeEvent(input$file1, {
    if(input$ShareDecision == "Share" & curl::has_internet() & droptoken){
      withProgress(message = 'Sharing Spectrum to Community Library', value = 3/3, {
        inFile <- input$file1
        UniqueID <- digest::digest(preprocesseddata(), algo = "md5")
        drop_upload(inFile$datapath, path = paste0(outputDir, "/", UniqueID), mode = "add")
      })
    }
  })

  #Metadata fields we want to save.
  fieldsAll <- c("User Name",
                 "Contact Info",
                 "Affiliation",
                 "Data Citation",
                 "Spectrum Identity",
                 "Spectrum Type",
                 "Color",
                 "CAS Number",
                 "Material Producer",
                 "Material Phase",
                 "Material Form",
                 "Other Material Form Description",
                 "Material Purity",
                 "Material Quality",
                 "Instrument Used",
                 "Instrument Accessories",
                 "Instrument Mode",
                 "Spectral Resolution",
                 "LaserLight Used",
                 "Number of Accumulations",
                 "Total Acquisition Time",
                 "Data Processing Procedure",
                 "Level of Confidence in Identification",
                 "Description of Identification",
                 "Other Information",
                 "smoother",
                 "baseline",
                 "range") #Small bug, range is getting replaced with the datetime somehow.

  #Save data to cloud.
  saveData <- function(data, UniqueID) {
    # Create a unique file name
    fileName <- paste0(paste(human_timestamp(), UniqueID, sep = "_"), ".csv")#sprintf("%s_%s.csv", as.integer(Sys.time()), digest(data))
    # Write the data to a temporary file locally
    filepath <- file.path(tempdir(), fileName)
    write.csv(data, filepath, row.names = FALSE, quote = TRUE)
    # Upload the file to dropbox
    UniqueID <- digest::digest(preprocesseddata(), algo = "md5")
    drop_upload(filepath, path = outputDir)
  }

  #How to save the metadata
  saveDataForm <- function(data, UniqueID) {
    data <- data.frame(input = data, variable = c(fieldsAll))
    # Create a unique file name
    fileName <- paste0(paste(human_timestamp(), UniqueID, "form", sep = "_"), ".csv")#sprintf("%s_%s.csv", as.integer(Sys.time()), digest(data))
    # Write the data to a temporary file locally
    filepath <- file.path(tempdir(), fileName)
    write.csv(data, filepath, row.names = FALSE, quote = TRUE)
    # Upload the file to Dropbox
    drop_upload(filepath, path = outputDir)
  }

  #Compile metadata input
  formData <- reactive({
    data <- sapply(fieldsAll[1:(length(fieldsAll)-1)], function(x) input[[x]])
    data <- c(data, paste(input[[fieldsAll[length(fieldsAll)]]][1], input[[fieldsAll[length(fieldsAll)]]][2], sep = "_"), timestamp = human_timestamp())
    data
  })

  #Save the metadata and data submitted upon pressing the button.
  observeEvent(input$submit,{
    if(curl::has_internet() & droptoken){
    UniqueID <- digest::digest(data(), algo = "md5")
    saveDataForm(formData(), UniqueID)
    saveData(data(), UniqueID)
    shinyjs::alert(paste("Thank you for sharing your data! Your data will soon be available @ https://osf.io/stmv4/")) #Your Fortune For Today is:  ", sample(Fortunes$Fortunes, 1), sep = ""))
    }
  })

  #Helper icon requirement ----
  #Needs to run to add the little helper icons to the data.
  observe_helpers(withMathJax = TRUE)


  #Read in data when uploaded based on the file type----
  preprocesseddata <- reactive({
    req(input$file1)
    inFile <- input$file1
    filename <- tolower(as.character(inFile$datapath))

    shiny::validate(shiny::need(any(c(endsWith(filename, ".jdx"), endsWith(filename, ".spc"), endsWith(filename, ".spa"), endsWith(filename, ".0"), endsWith(filename, ".csv"))), "Uploaded data type is not currently supported please check help icon (?) and About tab for details on data formatting."))

    if(endsWith(filename, ".jdx")){
      jdx <- read.jdx(inFile$datapath)
      data.frame(Wavelength = jdx@wavelength, Absorbance = as.numeric(unname(jdx@data$spc[1,])))
    }
    else if(endsWith(filename, ".spc")){
      spc <- read.spc(inFile$datapath)
      data.frame(Wavelength = spc@wavelength, Absorbance = as.numeric(unname(spc@data$spc[1,])))
    }
    else if(endsWith(filename, ".spa")){
      to.read = file(inFile$datapath, "rb");
      lines <- read_table(inFile$datapath)
      begining <- lines$`Spectral Data File`[grepl("Resolution", lines$`Spectral Data File`)]

      start <- as.numeric(strsplit(begining, " ")[[1]][4])
      end <- as.numeric(strsplit(begining, " ")[[1]][6])

      # Read the start offset
      seek(to.read, 386, origin="start");
      startOffset <- readBin(to.read, "int", n=1, size=2);
      # Read the length
      seek(to.read, 390, origin="start");
      readLength <- readBin(to.read, "int", n=1, size=2);

      # seek to the start
      seek(to.read, startOffset, origin="start");

      # we'll read four byte chunks
      floatCount <- readLength/4

      # read all our floats
      floatData <- c(readBin(to.read,"double",floatCount, size=4))

      data.frame(Wavelength = seq(end, start, length = length(floatData)), Absorbance = floatData)

    }
    else if(endsWith(filename, ".0")){
      file.name = inFile$datapath
      pa <- hexView::readRaw(file.name, offset = 0, nbytes = file.info(file.name)$size, human = "char", size = 1, endian = "little")
      pr <- pa$fileRaw

      codes=c("ZFF","RES","SNM","DAT","LWN","FXV","LXV","NPT","MXY","MNY","END","TIM")

      ## Get positions where the following parameters are found in the file
      z <- grepRaw(codes[1],pr,all=TRUE)[1]+5
      re <- grepRaw(codes[2],pr,all=TRUE)[1]+5
      snm <- grepRaw(codes[3],pr,all=TRUE)[1]+7
      dat <- grepRaw(codes[4],pr,all=TRUE)[1]+7
      lwn <- grepRaw(codes[5],pr,all=TRUE)[1]+7
      fx <- grepRaw(codes[6],pr,all=TRUE)[3]+7
      lx <- grepRaw(codes[7],pr,all=TRUE)[3]+7
      npt0 <- grepRaw(codes[8],pr,all=TRUE)[2]+3
      npt1 <- grepRaw(codes[8],pr,all=TRUE)[3]+7
      mxy <- grepRaw(codes[9],pr,all=TRUE)[1]+7
      mny <- grepRaw(codes[10],pr,all=TRUE)[3]+7
      end <- grepRaw(codes[11],pr,all=TRUE)+11
      tim <- grepRaw(codes[12],pr,all=TRUE)+11

      ## calculate end and start of each block:
      offs <- sapply(5:10, function(x){end[x]})
      byts <- diff(offs)
      ZFF <- readRaw(file.name, offset=z, nbytes=4, human="int", size=2)[[5]][1]
      RES <- readRaw(file.name, offset=re, nbytes=4, human="int", size=2)[[5]][1]
      snm.lab.material <- blockString(readRaw(file.name, offset = snm, nbytes = 22, human = "char", size = 1, endian = "little"))

      ## Get number of data points for each spectra data block
      NPT0 <- readRaw(file.name, offset=npt0, nbytes=12, human="int", size=4)[[5]][2]
      NPT1 <- readRaw(file.name, offset=npt1, nbytes=4, human="int", size=4)[[5]][1]
      fxv <- readRaw(file.name, offset=fx, nbytes=16, human="real", size=8)[[5]][1] ## fxv:	Frequency of first point
      lxv <- readRaw(file.name, offset=lx, nbytes=16, human="real", size=8)[[5]][1] ## lxv:	Frequency of last point
      Wavenumbers <- rev(seq(lxv, fxv, (fxv-lxv)/(NPT1-1)))

      ## Read all through all the data blocks inside the OPUS file:
      nbytes1 <- NPT0*4 ## initial parameters
      smxa <- c()
      smna <- c()
      nbytes.f <- NPT1*4
      if(offs[1]<2000){
        offs.f<-offs[3]
      }

      if(offs[1]>20000){
        offs.f<-offs[2]
      }

      ## Selected spectra block
      opus.p <- readRaw(file.name,width=NULL,offset=offs.f-4,nbytes=nbytes.f,human="real",size=4,endian="little")
      spectra <- opus.p[[5]]

      data.frame(Wavelength = Wavenumbers, Absorbance = spectra)

    }
    else{
      shiny::validate(need(all(c("Wavelength", "Absorbance") %in% colnames(fread(inFile$datapath))), "Uploaded data format is not currently supported please check help icon (?) and About tab for details on data formatting."))

      fread(inFile$datapath)


    }

  })

  #Corrects spectral intensity units using the user specified correction ----
  absorbance <- reactive({
    if(input$IntensityCorr == "Transmittance"){log10(1/adjust_negative(preprocesseddata()$Absorbance))}
    else if(input$IntensityCorr == "Reflectance"){(1-adjust_negative(preprocesseddata()$Absorbance))^2/(2*adjust_negative(preprocesseddata()$Absorbance))}
    else{adjust_negative(preprocesseddata()$Absorbance)}
  })

  #Formatting the dataset for further processing ----
  data <- reactive({
    preprocesseddata() %>%
      mutate(AbsorbanceAdj = absorbance()) %>%
      mutate(NormalizedIntensity = minmax(absorbance())) %>%
      mutate(BaselineRemove = NormalizedIntensity) #This is here to allow the user to choose the original data or the baseline corrected data to use.
  })

  #compute spectral resolution ----
  SpectralResolution <- reactive({
    (max(data()$Wavelength) - min(data()$Wavelength))/length(data()$Wavelength)
  })

  #All cleaning of the data happens here. Smoothing and Baseline removing. ----
  baselinedata <- reactive({
    testdata <- data() %>% dplyr::filter(Wavelength > input$MinRange & Wavelength < input$MaxRange)
    test <-  nrow(testdata) < 3
    if(test){
      data() %>%
        mutate(Smoothed = if(input$smoother != 0) signal::filter(filt = sgolay(p = input$smoother, n = 11), x = AbsorbanceAdj) else AbsorbanceAdj) %>%
        mutate(BaselineRemove = if(input$baseline != 0) Smoothed - iModPolyFit(Wavelength,Smoothed, input$baseline) else Smoothed) %>%
        mutate(BaselineRemove = minmax(BaselineRemove))
    }
    else{
      data() %>%
      dplyr::filter(Wavelength > input$MinRange & Wavelength < input$MaxRange) %>%
      mutate(Smoothed = if(input$smoother != 0) signal::filter(filt = sgolay(p = input$smoother, n = 11), x = AbsorbanceAdj) else AbsorbanceAdj) %>%
      mutate(BaselineRemove = if(input$baseline != 0) Smoothed - iModPolyFit(Wavelength,Smoothed, input$baseline) else Smoothed) %>%
      mutate(BaselineRemove = minmax(BaselineRemove))
    }
  })


  #Create file view and preprocess view.
  output$MyPlot <- renderPlotly({
    plot_ly(data(), type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~Wavelength, y = ~NormalizedIntensity, name = 'Uploaded Spectrum', line = list(color = 'rgba(240,236,19, 0.8)')) %>%
      layout(yaxis = list(title = "Absorbance Intensity"), xaxis = list(title = "Wavenumber (1/cm)"), plot_bgcolor='rgb(17,0,73)', paper_bgcolor='black', font = list(color = '#FFFFFF'))
  })
  output$MyPlotB <- renderPlotly({
   plot_ly(type = 'scatter', mode = 'lines') %>%
      add_trace(data = baselinedata(), x = ~Wavelength, y = ~BaselineRemove, name = 'Processed Spectrum', line = list(color = 'rgb(240,19,207)')) %>%
      add_trace(data = data(), x = ~Wavelength, y = ~NormalizedIntensity, name = 'Uploaded Spectrum', line = list(color = 'rgba(240,236,19,0.8)')) %>% #Dark blue rgb(63,96,130) https://www.rapidtables.com/web/color/RGB_Color.html https://www.color-hex.com/color-names.html
      layout(yaxis = list(title = "Absorbance Intensity"), xaxis = list(title = "Wavenumber (1/cm)"), plot_bgcolor='rgb(17,0,73)', paper_bgcolor='black', font = list(color = '#FFFFFF'))

  })

  #Choose which spectrum to use ----
  DataR <- reactive({
    if(input$Data == "uploaded"){
      data()
    }
    else if(input$Data == "processed"){
      baselinedata()
    }
  })

  #React Reference library to library type ----
  Library <- reactive({
    if(input$Spectra== "Raman" & input$Library == "Full"){
      RamanLibrary
    }
    else if(input$Spectra== "FTIR" & input$Library == "Full"){
      FTIRLibrary
    }
    else if(input$Spectra== "Raman" & input$Library == "Peaks"){
      RamanLibraryPeak
    }
    else if(input$Spectra== "FTIR" & input$Library == "Peaks"){
      FTIRLibraryPeak
    }
  })


  #React to metadata library to library type choice----
  Metadata <- reactive({
    if(input$Spectra== "Raman"){
      RamanLibraryMetadata
    }
    else{
      FTIRLibraryMetadata
    }
  })

  #Identify Spectra function ----
  #Joins their spectrum to the internal database and computes correlation.
  MatchSpectra <- reactive ({
    req(input$tabs == "tab3")
    input
    withProgress(message = 'Analyzing Spectrum', value = 1/3, {

      incProgress(1/3, detail = "Finding Match")
      WavelengthSubset <- WavelengthApprox[WavelengthApprox >= min(DataR()$Wavelength) & WavelengthApprox <= max(DataR()$Wavelength)]

    Lib <- Library() %>%
        inner_join(dplyr::rename(data.frame(approx(DataR()$Wavelength, DataR()$BaselineRemove, xout = WavelengthSubset , rule = 2, method = "linear", ties=mean)), "Wavelength" = x), by = "Wavelength") %>%
        dplyr::rename("BaselineRemove" = y) %>%
        group_by(group, SampleName) %>%
        mutate(BaselineRemove = BaselineRemove - min(BaselineRemove)) %>%
        ungroup() %>%
        mutate(BaselineRemove = minmax(BaselineRemove)) %>%
        group_by(SampleName) %>%
        dplyr::summarise(rsq = cor(Intensity, BaselineRemove)) %>%
        top_n(1000,rsq) %>%
        inner_join(select(Metadata(), -rsq), by = "SampleName") %>%
        select(SampleName, SpectrumIdentity, rsq, Organization) %>%
        arrange(desc(rsq)) %>%
        mutate(rsq = round(rsq, 2))

      incProgress(1/3, detail = "Making Plot")

    })
    return(Lib)
  })


#Create the data tables ----
  output$event <- DT::renderDataTable({
    datatable(MatchSpectra() %>%
                 dplyr::rename("Material" = SpectrumIdentity) %>%
                 dplyr::select(-SampleName) %>%
                 dplyr::rename("Pearson's r" = rsq), options = list(searchHighlight = TRUE, sDom  = '<"top">lrt<"bottom">ip', lengthChange = FALSE, pageLength = 5),  filter = 'top',caption = "Selectable Matches", style = 'bootstrap', selection = list(mode = 'single', selected = c(1)))
  })

  output$costs <- DT::renderDataTable({
    datatable(Costs, options = list(searchHighlight = TRUE, sDom  = '<"top">lrt<"bottom">ip', lengthChange = FALSE, pageLength = 5),  filter = 'top',caption = "Operation Costs", style = 'bootstrap', selection = list(mode = 'single', selected = c(1)))
  })

  output$donations <- DT::renderDataTable({
    datatable(Donations, options = list(searchHighlight = TRUE, sDom  = '<"top">lrt<"bottom">ip', lengthChange = FALSE, pageLength = 5),  filter = 'top',caption = "Donations", style = 'bootstrap', selection = list(mode = 'single', selected = c(1)))
  })

  output$eventmetadata <- DT::renderDataTable({
    datatable(Metadata() %>%
                select(SpectrumIdentity, Organization, ContactInfo, SpectrumType, InstrumentUsed, InstrumentAccessories, InstrumentMode, LaserLightUsed, TotalAcquisitionTime_s,
                       NumberofAccumulations, LevelofConfidenceinIdentification, CASNumber, MaterialProducer, MaterialPurity, MaterialForm, MaterialQuality, SpectralResolution, DataProcessingProcedure,
                       OtherInformation, SampleName) %>%
                rename("Material" = SpectrumIdentity, "Contact Information" = ContactInfo, "Spectrum Type" = SpectrumType, "Instrument Type" = InstrumentUsed,
                "Spectral Resolution" = SpectralResolution, "Number of Accumulations" = NumberofAccumulations,
                "Material Form" = MaterialForm,  "Instrument Mode" = InstrumentMode, "Other Information" = OtherInformation, "Material Producer" = MaterialProducer,
                 "CAS Number" = CASNumber, "Material Purity" = MaterialPurity, "Material Quality" = MaterialQuality,
                "Instrument Accessories" = InstrumentAccessories, "Laser or Light Used" = LaserLightUsed, "Total Acquisition Time" = TotalAcquisitionTime_s,
                "Data Processing Procedure" = DataProcessingProcedure, "Level of Confidence in Identification" = LevelofConfidenceinIdentification) %>%
                inner_join(MatchSpectra()[input$event_rows_selected,,drop = FALSE] %>% select(-Organization, -SpectrumIdentity), by = "SampleName") %>%
                select_if(function(x){!all(x == "" | is.na(x))}),
                escape=FALSE,
                options = list(dom = 't', bSort = F, lengthChange = FALSE, rownames=FALSE, info = FALSE),
                style = 'bootstrap', caption = "Selection Metadata", selection = list(mode = 'none'))
  })

#Display matches based on table selection ----
    output$MyPlotC <- renderPlotly({
    if(!length(input$event_rows_selected)) {
        plot_ly(DataR()) %>%
        add_lines(x = ~Wavelength, y = ~BaselineRemove, line = list(color = 'rgba(255,255,255,0.8)'))%>%
        layout(yaxis = list(title = "Absorbance Intensity"), xaxis = list(title = "Wavenumber (1/cm)"), plot_bgcolor='rgb(17,0, 73)', paper_bgcolor='black', font = list(color = '#FFFFFF'))
    }
    else if(length(input$event_rows_selected)){
      TopTens <- Library() %>%
        inner_join(MatchSpectra()[input$event_rows_selected,,drop = FALSE], by = "SampleName") %>%
        select(Wavelength, Intensity, SpectrumIdentity)

      OGData <- DataR() %>%
        select(Wavelength, BaselineRemove) %>%
        dplyr::rename(Intensity = BaselineRemove) %>%
        mutate(SpectrumIdentity = "Spectrum to Analyze")

        plot_ly(TopTens, x = ~Wavelength, y = ~Intensity) %>%
        add_lines(data = TopTens, x = ~Wavelength, y = ~Intensity, color = ~factor(SpectrumIdentity), colors = "#FF0000")%>% #viridisLite::plasma(7, begin = 0.2, end = 0.8)
        add_lines(data = OGData, x = ~Wavelength, y = ~Intensity, line = list(color = 'rgba(255,255,255,0.8)'), name = 'Spectrum to Analyze')%>%
        layout(yaxis = list(title = "Absorbance Intensity"), xaxis = list(title = "Wavenumber (1/cm)"), plot_bgcolor='rgb(17,0, 73)', paper_bgcolor='black', font = list(color = '#FFFFFF'))
    }})


#Data Download options ----
  output$downloadData5 <- downloadHandler(
    filename = function() {
      paste(deparse(substitute(FTIRLibrary)), '.csv', sep='')
    },
    content = function(file) {
      fwrite(FTIRLibrary, file)
    }
  )

  output$downloadData6 <- downloadHandler(
    filename = function() {
      paste(deparse(substitute(RamanLibrary)), '.csv', sep='')
    },
    content = function(file) {
      fwrite(RamanLibrary, file)
    }
  )

  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste(deparse(substitute(RamanLibraryMetadata)), '.csv', sep='')
    },
    content = function(file) {
      fwrite(RamanLibraryMetadata, file)
    }
  )
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste(deparse(substitute(FTIRLibraryMetadata)), '.csv', sep='')
    },
    content = function(file) {
      fwrite(FTIRLibraryMetadata, file)
    }
  )

  output$downloadData7 <- downloadHandler(
    filename = function() {
      paste('testdata', '.csv', sep='')
    },
    content = function(file) {
      fwrite(testdata, file)
    }
  )

  ##Download their own data.----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', human_timestamp(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(baselinedata(), file)
    }
  )

  #Hide functions which shouldn't exist when there is no internet or when the API token doesnt exist ----
  observe({
    if(droptoken & curl::has_internet()){
      show("ShareDecision")
      show("btn")
      show("helper1")
    }
    else{
      hide("ShareDecision")
      hide("btn")
      hide("helper1")
    }
  })

  observeEvent(input$btn, {
    toggle("User Name")
    toggle("Contact Info")
    toggle("Affiliation")
    toggle("Data Citation")
    toggle("Spectrum Identity")
    toggle("Spectrum Type")
    toggle("Color")
    toggle("CAS Number")
    toggle("Material Producer")
    toggle("Material Phase")
    toggle("Material Form")
    toggle("Other Material Form Description")
    toggle("Material Purity")
    toggle("Material Quality")
    toggle("Instrument Used")
    toggle("Instrument Accessories")
    toggle("Instrument Mode")
    toggle("Spectral Resolution")
    toggle("LaserLight Used")
    toggle("Number of Accumulations")
    toggle("Total Acquisition Time")
    toggle("Data Processing Procedure")
    toggle("Level of Confidence in Identification")
    toggle("Description of Identification")
    toggle("Other Information")
    toggle("submit")

  })

output$translate <- renderUI({
  if(file.exists("data/googletranslate.html") & curl::has_internet()){
            includeHTML("data/googletranslate.html")
  }
  else{
    NULL
  }
})

output$analytics <- renderUI({
  if(file.exists("data/google-analytics.js") & curl::has_internet()){
    includeScript("data/google-analytics.js")
  }
  else{
    NULL
  }
})

})

