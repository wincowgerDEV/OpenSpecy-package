 data("raman_hdpe")
 raman_wavelength <- raman_hdpe
 raman_wavelength$wavenumber <- (-1*(raman_wavelength$wavenumber/10^7-1/530))^(-1)
 
 test_that("adj_wave() handles input errors and inputs correctly", {
     adj_wave(1:1000) |> expect_error()
     adj_wave(raman_wavelength, laser = 530) |> expect_silent()
     adj_wave(raman_wavelength$wavenumber, laser = 530) |> expect_silent()
     #identical with different structured inputs. 
     expect_identical(adj_wave(raman_wavelength, laser = 530)$wavenumber, 
                      adj_wave(raman_wavelength$wavenumber, laser = 530)) 
     #expected values.
     expect_identical(round(adj_wave(raman_wavelength, laser = 530)$wavenumber)[1:2], c(301,305))
     #minimal shifts in wavenumbers
     expect_true(all(abs(round(adj_wave(raman_wavelength, laser = 530)$wavenumber) - round(raman_hdpe$wavenumber))<=1))
 })
 
