

#' @examples
#' \dontrun{
#' data("raman_hdpe")
#' plot_OpenSpecy(raw = raman_hdpe, processed = process_spectra(raman_hdpe), selected = "Processed", match = process_spectra(raman_hdpe))
#' data("CA_tiny_map")
#' tiny_map <- read_
#' }
#' 
#' @importFrom magrittr %>%
#' @import plotly
#' @export
#' 
#' 
plot_OpenSpecy <- function(raw = NULL, processed = NULL, selected = NULL, selected_number = 1, signal_option = "signal_to_noise", match = NULL, matched_number = 1){ #Could probably add the signal to noise calculations to the open specy metadata and then harvest it from there.
    raw_or_processed = (if(is.null(selected)){NULL} 
                        else if(selected == "Processed"){processed} 
                        else if(selected == "Raw"){raw})
    
    plot_ly(type = 'scatter', mode = 'lines', source = "B") %>%
        add_trace(x = if(!is.null(raw)) {raw$wavenumber} else{NULL}, 
                  y = if(!is.null(raw)){make_rel(raw$spectra[[selected_number]], na.rm = T)} else{NULL},
                  name = ifelse(is.null(selected), "Raw", ifelse(selected == "Raw", 'Raw (Identified)', "Raw")),
                  line = list(color = 'rgba(240,236,19,0.8)')) %>%
        add_trace(x = if(!is.null(processed)){processed$wavenumber} else{NULL}, 
                  y = if(!is.null(processed)){make_rel(processed$spectra[[selected_number]], na.rm = T)} else{NULL},
                  name = ifelse(is.null(selected), "Processed", ifelse(selected == "Processed", 'Processed (Identified)', "Processed")),
                  line = list(color = 'rgb(240,19,207)')) %>%
        add_trace(x = if(!is.null(match)){match$wavenumber} else{NULL}, 
                  y = if(!is.null(match)){make_rel(match$spectra[[matched_number]], na.rm = T)} else{NULL},
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
                   text = if(!is.null(raw_or_processed)) 
                       paste0(paste0("Signal to Noise = ", 
                         round(signal_noise(raw_or_processed$wavenumber, 
                         raw_or_processed$spectra[[selected_number]], return = signal_option), 2)), 
                         if(!is.null(match)) paste0("; ", "Correlation = ", 
                         cor(raw_or_processed$spectra[[selected_number]], 
                         match$spectra[[matched_number]]))) else "",
                   x = 0
               ), 
               font = list(color = '#FFFFFF')) %>%
        config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape"))
}

map <- read_zip("inst/extdata/CA_tiny_map.zip")

heatmap_OpenSpecy(map)
heatmap_OpenSpecy(map, signal_noise = vapply(map$spectra, function(x) {signal_noise(wavenumber = map$wavenumber, intensity = x)}, FUN.VALUE = numeric(1)), type = "signal_noise", min_sn = 0.05)

#Still needs reworking, internet is too slow to download my tiny test file for this. 
heatmap_OpenSpecy <- function(spectra = NULL, signal_noise = NULL, cors = NULL, type = NULL, min_sn = NULL, min_cors = NULL)({
    plot_ly(source = "heat_plot") %>%
        add_trace(
            x = spectra$metadata$x, #Need to update this with the new rout format. 
            y = spectra$metadata$y, 
            z = if("signal_noise" %in% type & !is.null(min_sn)) ifelse(signal_noise > min_sn, signal_noise, NA) else if("cors" %in% type & !is.null(min_cors)) ifelse(cors > min_cors, cors, NA) else vapply(spectra$spectra, mean, na.rm = T, FUN.VALUE = numeric(1)), 
            type = "heatmap",
            hoverinfo = 'text',
            colors = if("signal_noise" %in% type & !is.null(min_sn)){heat.colors(n = sum(signal_noise > min_sn))} else if("cors" %in% type & !is.null(min_cors)) {hcl.colors(n = sum(cors > min_cors), palette = "viridis", alpha = NULL, rev = FALSE, fixup = TRUE)} else{cm.colors(n = length(spectra$metadata$x), rev = FALSE)},
            text = ~paste(
                "x: ", spectra$metadata$x,
                "<br>y: ", spectra$metadata$y,
                "<br>z: ", if("signal_noise" %in% type){round(signal_noise, 2)} else if("cors" %in% type) {round(cors, 2)} else{round(vapply(spectra$spectra, mean, na.rm = T, FUN.VALUE = numeric(1)), 2)},
                "<br>Filename: ", spectra$metadata$filename)) %>%
        layout(
            xaxis = list(title = 'x',
                         zeroline = F,
                         showgrid = F
            ),
            yaxis = list(title = 'y',
                         zeroline = F,
                         showgrid = F),
            plot_bgcolor = 'rgb(17,0,73)',
            paper_bgcolor = 'rgba(0,0,0,1)',
            font = list(color = '#FFFFFF'),
            #legend= list(title=list(text= '<b> Correlation </b>')),
            title = (if(!is.null(signal_noise) & !is.null(cors) & !is.null(min_sn) & !is.null(min_cors)) paste0(round((1 - sum(signal_noise < min_sn | cor < min_cors)/length(signal_noise)), 2) * 100, "% Good ID")  
                     else if (!is.null(signal_noise) & !is.null(min_sn)) paste0(round((1 - sum(signal_noise < min_sn)/length(signal_noise)), 2) * 100, "% Good Signal")
                     else "Mean Signal")) %>%
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
