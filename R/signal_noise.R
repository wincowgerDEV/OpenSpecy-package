
vapply(map$spectra, function(x) {signal_noise(wavenumber = map$wavenumber, intensity = x)}, FUN.VALUE = numeric(1))
intensity = map$spectra[[1]]

signal_noise <- function(wavenumber, intensity, remove_min = 2200, remove_max = 2420, include_min = 1900, include_max = 2700, return = "signal_times_noise") {
    include_values <- wavenumber >= include_min & wavenumber <= include_max
    remove_values <- wavenumber >= remove_min & wavenumber <= remove_max 
    signal = mean(intensity[!remove_values], na.rm = T)
    noise = sd(intensity[!remove_values & include_values], na.rm = T)
    if(return == "signal"){
        return(signal)
    }
    if(return == "noise"){
        return(noise)
    }
    if(return == "signal_times_noise"){
        return(abs(signal*noise))
    }
    if(return == "signal_to_noise"){
        return(abs(signal/noise))
    }
}
