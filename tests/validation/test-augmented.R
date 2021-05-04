#Libraries ----
library(dplyr)

#Functions ----
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

#Augment the library data ----
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
#    adj_intens()

# Smooth and background-correct spectrum
raman_proc <- augmented_raman %>%
    group_by(sample_name) %>%
    smooth_intens(p = 3) #%>%
    subtr_bg(degree = 8) %>%
    mutate()

ftir_proc <- augmented_ftir %>%
    group_by(sample_name) %>%
    smooth_intens() %>%
    subtr_bg()

spectrum = raman_subset[100]

#Check that augmentation and cleaning performed as expected.
ggplot() +
    geom_line(data = augmented_raman %>%
                   filter(sample_name == spectrum),
               aes(x = wavenumber, y = intensity)
                   ) +
    geom_line(data = augmented_raman %>%
                    filter(sample_name == spectrum) %>%
                    smooth_intens(p = 3) %>%
                    subtr_bg(degree = 8),
                aes(x = wavenumber, y = intensity))


#Check that cleaned match is to something reasonable.
augmented_raman %>%
    filter(sample_name == spectrum) %>%
    smooth_intens(p = 3) %>%
    subtr_bg(degree = 8) %>%
    match_spec(library = spec_lib, which = "raman")

spectrum


#Match back to the library using the Open Specy functions to see if we can get back to the correct identity for the top match.
raman_identities <- c()
for(spectrum in raman_subset){

    identity <- unlist(spec_lib$raman$metadata[sample_name == spectrum, "spectrum_identity"])

    topmatch <- augmented_raman %>%
        filter(sample_name == spectrum) %>%
        smooth_intens(p = 3) %>%
        subtr_bg(degree = 8) %>%
        match_spec(library = spec_lib, which = "raman", top_n = 1) %>%
        select(spectrum_identity) %>%
        unlist()

    raman_identities <- c(raman_identities, identity == topmatch)

}

#Needs to be above 80%
sum(raman_identities)/length(raman_identities)


#FTIR validation
ftir_identities <- c()
for(spectrum in ftir_subset){

    identity <- unlist(spec_lib$ftir$metadata[sample_name == spectrum, "spectrum_identity"])

    topmatch <- augmented_ftir %>%
        filter(sample_name == spectrum) %>%
        smooth_intens(p = 3) %>%
        subtr_bg(degree = 8) %>%
        match_spec(library = spec_lib, which = "ftir", top_n = 1) %>%
        select(spectrum_identity) %>%
        unlist()

    ftir_identities <- c(ftir_identities, identity == topmatch)

}

#Needs to be above 80%
sum(ftir_identities)/length(ftir_identities)
