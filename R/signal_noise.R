


signal_noise <- function(x, wavenumber, remove_min = 2200, remove_max = 2420, include_min = 1900, include_max = 2700, return = "signal_times_noise") {
    include_values <- wavenumber >= include_min & wavenumber <= include_max
    remove_values <- wavenumber >= remove_min & wavenumber <= remove_max 
    signal = mean(x[!remove_values], na.rm = T)
    noise = sd(x[!remove_values & include_values], na.rm = T)
    if(return == "signal"){
        return(signal)
    }
    if(return == "noise"){
        return(noise)
    }
    if(return == "signal_times_noise"){
        return(signal*noise)
    }
    if(return == "signal_to_noise"){
        return(signal/noise)
    }
}