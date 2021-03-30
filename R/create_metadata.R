setwd("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/OpenSpecy/Code/Github/inst/shiny")

load("G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/OpenSpecy/Code/Github/inst/shiny/data/namekey.RData")

data_creation <- function(user_id,
                         data_id,
                         smooth_decision,
                         smoother,
                         baseline_decision,
                         baseline,
                         range_decision,
                         min_range,
                         max_range,
                         spectra_type,
                         spectrum_to_analyze,
                         library,
                         row,
                         time) {
    
                    output_dir = paste("data/", user_id, sep = "")
                    if (!dir.exists(output_dir)) {dir.create(output_dir)}
    
    
                    saveRDS(tibble(
                        "user_id" = user_id,
                        "data_id" = data_id,
                        "smooth_decision" = smooth_decision,
                        "smoother" = smoother,
                        "baseline_decision" = baseline_decision,
                        "baseline" = baseline,
                        "range_decision" = range_decision,
                        "min_range" = min_range,
                        "max_range" = max_range,
                        "spectra_type" = spectra_type,
                        "spectrum_to_analyze" = spectrum_to_analyze,
                        "library" = library,
                        "row" = row,
                        "time" = time
                    ), paste(output_dir, "/", time, ".rds", sep = ""))    
         }

                    data_creation(user_id = 1,
                                  data_id = 2,
                                  smooth_decision = TRUE,
                                  smoother = 5,
                                  baseline_decision = FALSE,
                                  baseline = 4,
                                  range_decision = FALSE,
                                  min_range = 6,
                                  max_range = 7,
                                  spectra_type = "full",
                                  spectrum_to_analyze = "raman",
                                  library = "peaks",
                                  row = 3,
                                  time = 234)

                    output_dir = paste("data/", user_id, sep = "")
                    if (!dir.exists(output_dir)) {dir.create(output_dir)}
                    
                    
                    saveRDS(data, paste(output_dir, "/", time, ".rds", sep = ""))
saveRDS(data, "G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/OpenSpecy/Code/Github/inst/shiny/data/user_base_tbl.rds")

#Save user information whenever they click a trigger
observeEvent(req(input$tabs %in% c("tab2","tab3")){
    data_creation(user_id = 1,
                  data_id = 2,
                  smooth_decision = TRUE,
                  smoother = 5,
                  baseline_decision = FALSE,
                  baseline = 4,
                  range_decision = FALSE,
                  min_range = 6,
                  max_range = 7,
                  spectra_type = "full",
                  spectrum_to_analyze = "raman",
                  library = "peaks",
                  row = 3,
                  time = 234)
})



