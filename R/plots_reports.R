
library(plotly)

plot(raw = test_noise, selected = "Uploaded")

plot_ly(type = 'scatter', mode = 'lines', source = "B") %>%
    add_trace(x = test_noise$wavenumber, 
              y = test_noise$spectra$intensity)

plot <- function(raw = NULL, processed = NULL, match = NULL, selected = NULL, signal_to_noise = NULL, correlation = NULL){
    plot_ly(type = 'scatter', mode = 'lines', source = "B") %>%
        add_trace(x = if(!is.null(raw)) {raw$wavenumber} else{NULL}, 
                  y = if(!is.null(raw)){make_rel(raw$spectra[[1]], na.rm = T)} else{NULL},
                  name = ifelse(is.null(selected), "Uploaded", ifelse(selected == "Uploaded", 'Uploaded (Identified)', NA)),
                  line = list(color = 'rgba(240,236,19,0.8)')) %>%
        add_trace(x = if(!is.null(processed)){processed$wavenumber} else{NULL}, 
                  y = if(!is.null(processed)){make_rel(processed$spectra, na.rm = T)} else{NULL},
                  name = ifelse(is.null(selected), "Processed", ifelse(selected == "Processed", 'Processed (Identified)', NA)),
                  line = list(color = 'rgb(240,19,207)')) %>%
        add_trace(x = if(!is.null(match)){processed$wavenumber} else{NULL}, 
                  y = if(!is.null(match)){make_rel(processed$spectra, na.rm = T)} else{NULL},
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
