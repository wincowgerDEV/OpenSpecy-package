
library(plotly)

plot(raw = test_noise, selected = "Uploaded")

plot_ly(type = 'scatter', mode = 'lines', source = "B") %>%
    add_trace(x = test_noise$wavenumber, 
              y = test_noise$spectra$intensity)

plot <- function(raw = NULL, processed = NULL, number = NULL, match = NULL, selected = NULL, signal_to_noise = NULL, correlation = NULL){
    plot_ly(type = 'scatter', mode = 'lines', source = "B") %>%
        add_trace(x = if(!is.null(raw)) {raw$wavenumber} else{NULL}, 
                  y = if(!is.null(raw)){make_rel(raw$spectra[[number]], na.rm = T)} else{NULL},
                  name = ifelse(is.null(selected), "Uploaded", ifelse(selected == "Uploaded", 'Uploaded (Identified)', NA)),
                  line = list(color = 'rgba(240,236,19,0.8)')) %>%
        add_trace(x = if(!is.null(processed)){processed$wavenumber} else{NULL}, 
                  y = if(!is.null(processed)){make_rel(processed$spectra[[number]], na.rm = T)} else{NULL},
                  name = ifelse(is.null(selected), "Processed", ifelse(selected == "Processed", 'Processed (Identified)', NA)),
                  line = list(color = 'rgb(240,19,207)')) %>%
        add_trace(x = if(!is.null(match)){match$wavenumber} else{NULL}, 
                  y = if(!is.null(match)){make_rel(match$spectra[[number]], na.rm = T)} else{NULL},
                  name = 'Matched',
                  line = list(color = 'rgb(125,249,255)')) %>%
        # Dark blue rgb(63,96,130)
        # https://www.rapidtables.com/web/color/RGB_Color.html https://www.color-hex.com/color-names.html
        layout(yaxis = list(title = "absorbance intensity [-]"),
               xaxis = list(title = "wavenumber [cm<sup>-1</sup>]",
                            autorange = "reversed"),
               plot_bgcolor = 'rgb(17,0,73)',
               paper_bgcolor = 'rgba(0,0,0,1)',
               title = list(
                   text = if(!is.null(processed)) paste0(paste0("Signal to Noise = ", round(signal_noise, 2)), if(input$active_identification) paste0("; ", "Max Correlation = ", correlation)) else "",
                   x = 0
               ), 
               font = list(color = '#FFFFFF')) %>%
        config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
}

#Still needs reworking, internet is too slow to download my tiny test file for this. 
output$heatmap <- renderPlotly({
    req(input$file1)
    #req(ncol(data()) > 2)
    plot_ly(source = "heat_plot") %>%
        add_trace(
            x = preprocessed$data$coords$x, #Need to update this with the new rout format. 
            y = preprocessed$data$coords$y, 
            z = if(input$active_identification){ifelse(signal_noise() < input$MinSNR | max_cor() < input$MinCor, NA, max_cor())} else {ifelse(signal_noise() > input$MinSNR, signal_noise(), NA)
            }, 
            type = "heatmap",
            hoverinfo = 'text',
            colors = if(input$active_identification){} else {heat.colors(n = sum(signal_noise() > input$MinSNR))
            },
            text = ~paste(
                "x: ", preprocessed$data$coords$x,
                "<br>y: ", preprocessed$data$coords$y,
                "<br>z: ", if(input$active_identification){round(max_cor(), 2)} else{round(signal_noise(), 2) 
                },
                "<br>Filename: ", preprocessed$data$coords$filename)) %>%
        layout(
            xaxis = list(title = 'x',
                         zeroline = F,
                         showgrid = F
            ),
            yaxis = list(title = 'y',
                         zeroline = F,
                         showgrid = F),
            plot_bgcolor = 'rgba(17,0,73, 0)',
            paper_bgcolor = 'rgba(0,0,0,0.5)',
            font = list(color = '#FFFFFF'),
            #legend= list(title=list(text= '<b> Correlation </b>')),
            title = if(input$active_identification) paste0(round((1 - sum(signal_noise() < input$MinSNR | max_cor() < input$MinCor)/length(signal_noise())), 2) * 100, "% Good ID")  else paste0(round((1 - sum(signal_noise() < input$MinSNR)/length(signal_noise())), 2) * 100, "% Good Signal")) %>%
        event_register("plotly_click") 
})

# Need to think about how best to incorporate this into a workflow, 
# I am imaging a user wanting a utility function where they can input 
# their correlation matrix or the library and just save what they need from it. 
# I think having the user specify the library, the correlation matrix and the unknown 
# Open Specy plus any specific spectrum in the specy and then a single name for what report they want
# with the option for "all" should do it. This will allow us to let the user save all the 
# rich analysis data at once and in the shiny app we can just have one download button
# with a drop down that specifies the value they want. Then in the shiny app we go from 
# 100 lines of code to this single line function. 
reports <- function(x){
    output$downloadData5 <- downloadHandler(
        filename = function() {"ftir_library.csv"},
        content = function(file) {fwrite(spec_lib[["ftir"]][["library"]], file)}
    )
    
    output$downloadData6 <- downloadHandler(
        filename = function() {"raman_library.csv"},
        content = function(file) {fwrite(spec_lib[["raman"]][["library"]], file)}
    )
    
    output$downloadData4 <- downloadHandler(
        filename = function() {"raman_metadata.csv"},
        content = function(file) {fwrite(spec_lib[["raman"]][["metadata"]], file)}
    )
    
    output$downloadData3 <- downloadHandler(
        filename = function() {"ftir_metadata.csv"},
        content = function(file) {fwrite(spec_lib[["ftir"]][["metadata"]], file)}
    )
    
    output$download_testdata <- downloadHandler(
        filename = function() {"testdata.csv"},
        content = function(file) {fwrite(testdata, file)}
    )
    
    output$download_testbatch <- downloadHandler(
        filename = function() {"testbatch.zip"},
        content = function(file) {zip(zipfile = file, files = c("data/HDPE__1.csv", "data/HDPE__2.csv", "data/HDPE__3.csv"))}
    )
    
    ## Download own data ----
    
    output$download_conformed <- downloadHandler(
        filename = function() {paste('data-conformed-', human_ts(), '.csv', sep='')},
        content = function(file){fwrite(data()%>% mutate(wavenumber = conform_wavenumber(preprocessed$data$wavenumber)), file)}
    )
    
    output$downloadData <- downloadHandler(
        filename = function() {paste('data-processed-', human_ts(), '.csv', sep='')},
        content = function(file) {fwrite(baseline_data() %>% mutate(wavenumber = conform_wavenumber(preprocessed$data$wavenumber)), file)}
    )
    
    output$downloadsnr <- downloadHandler(
        filename = function() {paste('data-snr-', human_ts(), '.csv', sep='')},
        content = function(file) {fwrite(data.table(x = preprocessed$data$coords$x, y = preprocessed$data$coords$y, filename = preprocessed$data$coords$filename, signal_to_noise = signal_noise(), good_signal = signal_noise() > input$MinSNR), file)}
    )
    
    
    ## Download selected data ----
    output$download_selected <- downloadHandler(
        filename = function() {paste('data-selected-', human_ts(), '.csv', sep='')},
        content = function(file) {fwrite(match_selected() %>% select(-SpectrumIdentity), file)}
    )
    
    ## Download matched data ----
    output$download_matched <- downloadHandler(
        filename = function() {paste('data-matched-', human_ts(), '.csv', sep='')},
        content = function(file) {fwrite(DataR_plot() %>% select(-SpectrumIdentity), file)}
    )
    
    ## Download matched data ----
    output$download_metadata <- downloadHandler(
        filename = function() {paste('data-analysis-metadata-', human_ts(), '.csv', sep='')},
        content = function(file) {fwrite(user_metadata(), file)}
    )
    
    ## Download validation data ----
    output$validation_download <- downloadHandler(
        filename = function() {paste('data-analysis-validation-', human_ts(), '.csv', sep='')},
        content = function(file) {fwrite(validation$data, file)}
    )
    
    ## Download correlation matrix ----
    output$correlation_download <- downloadHandler(
        filename = function() {paste('data-analysis-correlations-', human_ts(), '.csv', sep='')},
        content = function(file) {fwrite(correlation %>% mutate(library_names = names(libraryR())), file)}
    )
    
    output$topmatch_metadata_download <- downloadHandler(
        filename = function() {paste('data-analysis-topmatch-metadata-', human_ts(), '.csv', sep='')},
        content = function(file) {fwrite(data.table(x = preprocessed$data$coords$x, y = preprocessed$data$coords$y, filename = preprocessed$data$coords$filename, signal_to_noise = signal_noise(), good_signal = signal_noise() > input$MinSNR, max_cor = max_cor(), good_cor = max_cor() > input$MinCor, max_cor_id = max_cor_id()) %>% left_join(meta, by = c("max_cor_id" = "sample_name")), file)}
    )
}
