#library(ggplot2)
#library(dplyr)
#library(readr)
#library(data.table)
#library(stringr)
#library(dtplyr)
#library(stringdist)
#library(signal)
#library(zoo)
#library(pals)
#library(tidyr)
library(OpenSpecy)
library(dplyr)



minmax <- function(x) {
    (x - min(x))/(max(x)-min(x))
}

header.true <- function(df) {
    names(df) <- as.character(unlist(df[2,]))
    df[-c(1:2),]
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

#MatchFunctionMany
MatchSpectraMany <- function(Data, LibraryDF2, MetaDataTable){
    Data %>%
        dplyr::inner_join(LibraryDF2, by = "Wavelength") %>%
        dplyr::group_by(name) %>%
        dplyr::mutate(Absorbance = Absorbance - min(Absorbance)) %>%
        dplyr::mutate(Absorbance = minmax(Absorbance)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(SampleName, name) %>%
        dplyr::summarise(rsq = cor(Intensity, Absorbance), count = n()) %>%
        dplyr::ungroup() %>%
        dplyr::inner_join(select(MetaDataTable, -rsq), by = "SampleName") %>%
        as_tibble()
} 

RandomlyAugmentDataBaseline <- function(Data) {
    order <- sample(1:5, 1)
    valuestosample <- c(-100:-1, 1:100)
    if(order == 1) Data*sample(valuestosample, 1)+sample(valuestosample, 1)
    else if(order == 2) sample(valuestosample, 1)*Data^2 + sample(valuestosample, 1)*Data +sample(valuestosample, 1)
    else if(order == 3) sample(valuestosample, 1)*Data^3 + sample(valuestosample, 1)* Data^2 + sample(valuestosample, 1)*Data+sample(valuestosample, 1) 
    else if(order == 4) sample(valuestosample, 1)* Data^4 + sample(valuestosample, 1)* Data^3 + sample(valuestosample, 1)* Data^2 + sample(valuestosample, 1)*Data + sample(valuestosample, 1)
    else sample(valuestosample, 1)* Data^5 +sample(valuestosample, 1)*Data^4 + sample(valuestosample, 1)*Data^3 + sample(valuestosample, 1)*Data^2 + sample(valuestosample, 1)*Data+sample(valuestosample, 1)
    
}

augment_full <- function(Data){
    Data %>%
        select(-group) %>%
        dplyr::group_by(sample_name) %>%
        dplyr::arrange(wavenumber) %>%
        dplyr::mutate(intensity_og = minmax(intensity)) %>%
        dplyr::mutate(intensity_aug1 = minmax(intensity_og  + rnorm(length(wavenumber), mean = 0, sd = 1/sample(c(30:300), 1)))) %>%
        dplyr::mutate(intensity_aug2 = minmax(intensity_aug1  + minmax(RandomlyAugmentDataBaseline(wavenumber)))) %>%
        ungroup()
}

# Fetch current spectral library from https://osf.io/x7dpz/
get_lib()

# Load library into global environment
spec_lib <- load_lib()

#subset 100 of each library
raman_subset <- unique(spec_lib$raman$library$sample_name)[sample(1:length(unique(spec_lib$raman$library$sample_name)), 100, replace = F)]
ftir_subset <- unique(spec_lib$ftir$library$sample_name)[sample(1:length(unique(spec_lib$ftir$library$sample_name)), 100, replace = F)]

augmented_raman <- augment_full(spec_lib$raman$library %>%
                                    filter(sample_name %in% raman_subset)) %>%
                    mutate(intensity = intensity_aug2)

augmented_ftir <- augment_full(spec_lib$ftir$library %>%
                                   filter(sample_name %in% ftir_subset))%>%
                    mutate(intensity = intensity_aug2)


#Might consider adding this in at some point. 
# Adjust spectral intensity
#raman_adj <- raman_hdpe %>%
#    adjust_intensity()

# Smooth and background-correct spectrum
raman_proc <- augmented_raman %>% 
    group_by(sample_name) %>%
    smooth_intensity(p = 3) #%>% 
    subtract_background(degree = 8) %>%
    mutate()

ftir_proc <- augmented_ftir %>% 
    group_by(sample_name) %>%
    smooth_intensity() %>% 
    subtract_background()

spectrum = raman_subset[100]

#Check that augmentation and cleaning performed as expected. 
ggplot() +
    geom_line(data = augmented_raman %>%
                   filter(sample_name == spectrum), 
               aes(x = wavenumber, y = intensity)
                   ) + 
    geom_line(data = augmented_raman %>% 
                    filter(sample_name == spectrum) %>%
                    smooth_intensity(p = 3) %>% 
                    subtract_background(degree = 8),
                aes(x = wavenumber, y = intensity))


#Check that cleaned match is to something reasonable. 
augmented_raman %>% 
    filter(sample_name == spectrum) %>%
    smooth_intensity(p = 3) %>% 
    subtract_background(degree = 8) %>%
    match_spectrum(library = spec_lib, which = "raman")

spectrum

raman_identities <- c()
for(spectrum in raman_subset){
    
    identity <- unlist(spec_lib$raman$metadata[sample_name == spectrum, "spectrum_identity"])
    
    topmatch <- augmented_raman %>% 
        filter(sample_name == spectrum) %>%
        smooth_intensity(p = 3) %>% 
        subtract_background(degree = 8) %>%
        match_spectrum(library = spec_lib, which = "raman", top_n = 1) %>%
        select(spectrum_identity) %>%
        unlist()
    
    raman_identities <- c(raman_identities, identity == topmatch)
    
}

sum(raman_identities)/length(raman_identities)

#Test make sure that the augmentation, cleaning worked. 

raman_proc %>%
    filter(sample_name == unique(raman_proc$sample_name)[1]) %>%
    ggplot() +
    geom_point(aes(x = wavenumber, y = intensity)) + 
    geom_line(data = augmented_raman %>%
                  filter(sample_name == unique(raman_proc$sample_name)[1]),
              aes(x = wavenumber, y = intensity_og))

# Match spectrum with library and retrieve meta data
match_spectrum(raman_proc, library = spec_lib, which = "raman")


augmented_raman %>%
    dplyr::filter(sample_name == unique(augmented_raman$sample_name)[2]) %>%
    ggplot() +
    geom_line(aes(x = wavenumber, y = intensity_og)) +
    geom_line(aes(x = wavenumber, y = intensity_aug1)) +
    geom_line(aes(x = wavenumber, y = intensity_aug2))

saveRDS(augmented_ftir, "augmented_ftir.rds")
saveRDS(augmented_raman, "augmented_raman.rds")

#Jackknife
raman_subset <- unique(Raman_metadata$sample_name)[sample(1:length(unique(Raman_metadata$sample_name)), 100, replace = F)]
ftir_subset <- unique(FTIR_metadata$sample_name)[sample(1:length(unique(FTIR_metadata$sample_name)), 100, replace = F)]


