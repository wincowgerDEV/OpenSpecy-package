test_that("check that opus files are read", {
     expect_silent(data <- read_opus(read_extdata("ftir_ps.0")))
     expect_true(is_OpenSpecy(data))
     expect_true(is_OpenSpecy(data2))
     expect_true(ncol(data$spectra) == 1)
     expect_true(ncol(data2$spectra) == 1)
     expect_true(length(data$wavenumber) == 2126)
     expect_true(length(data2$wavenumber) == 2126)
     expect_identical(names(data$metadata), c("x",
                                              "y",               
                                              "unique_id",
                                              "sample_id",
                                              "date_time_sm",
                                              "date_time_rf",
                                              "sample_name",
                                              "instr_name_range",
                                              "resolution_wn",
                                              "result_spc",
                                              "beamspl",
                                              "laser_wn",
                                              "spc_in_file",
                                              "zero_filling",    
                                              "temp_scanner_sm",
                                              "temp_scanner_rf", 
                                              "hum_rel_sm",
                                              "hum_rel_rf",      
                                              "hum_abs_sm",
                                              "hum_abs_rf",      
                                              "file_id"))
     expect_identical(names(data2$metadata), c("x",
                                              "y",               
                                              "unique_id",
                                              "sample_id",
                                              "date_time_sm",
                                              "date_time_rf",
                                              "sample_name",
                                              "instr_name_range",
                                              "resolution_wn",
                                              "result_spc",
                                              "beamspl",
                                              "laser_wn",
                                              "spc_in_file",
                                              "zero_filling",    
                                              "temp_scanner_sm",
                                              "temp_scanner_rf", 
                                              "hum_rel_sm",
                                              "hum_rel_rf",      
                                              "hum_abs_sm",
                                              "hum_abs_rf",      
                                              "file_id"))
})
