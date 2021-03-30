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
                    saveRDS(tibble(
                        "user_id" = character(),
                        "data_id" = character(),
                        "smooth_decision" = logical(),
                        "smoother" = integer(),
                        "baseline_decision" = logical(),
                        "baseline" = integer(),
                        "range_decision" = logical(),
                        "min_range" = numeric(),
                        "max_range" = numeric(),
                        "spectra_type" = character(),
                        "spectrum_to_analyze" = character(),
                        "library" = character(),
                        "row" = integer(),
                        "time" = character()
                    ), paste("data/", user_id, time, ".rds"))    
         }


saveRDS(data, "G:/My Drive/GrayLab/Projects/Plastics/ActiveProjects/OpenSpecy/Code/Github/inst/shiny/data/user_base_tbl.rds")


OpenSpecy::human_ts()
OpenSpecy::

#User information saving ----
read_user_base <- function() {
    user_base_tbl <<- read_rds(path = "data/user_base_tbl.rds")
}

update_and_write_user_base <- function(folder, assign_input) {
    user_base_tbl <<- assign_input
    write_rds(user_base_tbl, path = "data/user_base_tbl.rds")
}


