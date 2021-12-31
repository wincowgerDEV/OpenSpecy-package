#Libraries ----
library(googleLanguageR)
library(dplyr)
library(data.table)

#Functions ----
not_all_na <- function(x) any(!is.na(x))


#Dataset ----
words <- c("About",
           "Welcome", 
           "Join the hundreds of researchers from around
           the world who are part of the Open Specy community by
           analyzing, sharing, processing, and identifying
           their Raman and IR spectra. These services are free and open source thanks to our partners:", 
           "Quick Video Tutorial", 
           "Instructions",
           "In Brief: To use the tool upload a csv, asp, jdx, spc, or spa file to the upload file tab.
                                  If csv, one column should be named 'wavenumber' (in units of 1/cm) and another named 'intensity'.
                                  You can smooth your data using an SG filter, baseline correct your data using the polynomial order of iModPolyFit, and restrict the wavelength range for the match.
                                  The result will be compared to an internal Raman or FTIR spectra library. The strongest 1000 matches along with your
                                  uploaded or processed data will be presented in an interactive plot and table. For more details click the button below
                                  or watch the detailed instructional video.",
           "Detailed Standard Operating Procedure",
           "Download Open Data",
           "Reference spectra was sourced from open access resources
           online, peer reviewed publications, and corporate donations. In the future
           spectra that is uploaded to the tool will be incorporated to the reference
           library to make it even better.",
           "Raman Reference Library",
           "FTIR Reference Library",
           "Raman Reference Library Metadata",
           "FTIR Reference Library Metadata",
           "Contribute Spectra",
           "To share spectra upload a file to the upload file tab. If you selected Share a copy of your spectra will be sent to the Community
           Data Warehouse on Open Science Framework. To add additional metadata, 
           fill in the avaliable metadata fields and click -Share Data-. The 
           spectra file that you uploaded along with your responses will be copied 
           to the a -With Metadata- subfolder at the link below. All shared data holds 
           a Creative Commons Attribution License 4.0.",
           "Community Data Warehouse",
           "Tool Validation", 
           "All parameters in this tool are tested to validate that 
           the tool is functioning as best as possible and determine the best default 
           parameters to use. Our current validation proceedure includes correcting 
           duplicated entries in the reference libraries, checking for spectra in 
           metadata that isn't in the spectral library, and ensuring the the default 
           parameters provide over 80% accuracy in the first match.",
           "Detailed Validation Procedure",
           "Updates, Feature Requests, and Bug Reports",
           "We keep track of all updates using version control on our code. Features can be requested and bug reported on GitHub.",
           "Stay up to date!",
           "Follow us on Twitter @OpenSpecy. E-mail wincowger@gmail.com to be added to the mailing list.",
           "Citation",
           "Useful Links",
           "Free FTIR Software: siMPle microplastic IR spectral identification software",
           "Free Spectroscopy Learning Academy from ThermoFisher",
           "Free Optical Microscopy Learning Resource from Florida State University",
           "Free desktop application for spectral analysis and links to reference databases.",
           "Terms And Conditions",
           "Privacy Policy",
           "Upload File",
           "Upload, View, and Share Spectra",
           "Choose .csv (preferred), .asp, .jdx, .spc, .spa, or .0 File",
           "Share Your Data?",
           "Share Help",
           "If you like, we share your uploaded spectra and settings with the spectroscopy community.
           By default, all data will be licensed under Creative Commons Attribution-NonCommercial 4.0 International (CC BY-NC 4.0).
           Uploaded spectra will appear here: https://osf.io/rjg3c",
           "Upload Help",
           "Upload Raman or FTIR spectrum files as a csv, jdx, spc, or spa. A csv file is preferred. If a csv, the file must contain one column labeled 'wavenumber' in units of (1/cm) and another column labeled 'intensity' in absorbance units.
            If jdx, spc, spa, or 0 the file should be a single absorbance spectrum with wavenumber in (1/cm). These files will not always work perfectly because they are tricky to read so double check them in another software.
            Hit the 'Test Data' button to download a sample Raman spectrum.",
           "Intensity Adjustment",
           "None",
           "Transmittance",
           "Reflectance",
           "Intensity Correction Help",
           "If the uploaded spectrum is not in absorbance units, 
           use this input to specify the units to convert from.Open Specy can 
           adjust reflectance or transmittance spectra to Absorbance units using 
           this drop down in the upload file tab. All of the preceding tabs 
           assume that the data is in absorbance units so you should make the 
           correction before continuing if needed. The transmittance adjustment 
           uses the log10(1/T) calculation which does not correct for system 
           and particle characteristics. The reflectance adjustment uses the 
           Kubelka-Munk equation (1-R)2/(2*R). We assume that the reflectance 
           is formatted as a percent from 1-100 and first correct the intensity by dividing by 100
           so that it fits the form expected by the equation.
           If none is selected, Open Specy assumes that the uploaded data is 
           an absorbance spectrum.",
           "Sample File",
           "Sample Data Help",
           "This is a sample spectrum that can be uploaded to the tool for testing it out and understanding how the csv files should be formatted.",
           "Metadata Input",
           "Metadata Help",
           "We share any uploaded spectra and metadata with the spectroscopy community if you fill out the metadata here and select share.
           Uploaded spectra and metadata will appear here: https://osf.io/rjg3c",
           "Share Metadata",
           "Upload some data to get started...",
           "Preprocess Spectrum",
           "Smooth, Baseline Correct, and Download Processed Spectra",
           "Download (recommended)",
           "Download Help",
           "Some users may wish to save a copy of their processed spectrum. This button downloads the processed spectrum as a csv file.",
           "Smoothing",
           "This smoother can enhance the signal to noise ratio of the data and uses a Savitzky-Golay filter with 12 running data points and the polynomial specified.",
           "Smoothing Polynomial",
           "Baseline Correction",
           "Baseline Correction Help",
           "This baseline correction routine has two options for baseline correction, 1) the polynomial imodpolyfit procedure to itteratively find the baseline of the spectrum using a polynomial fit to the entire region of the spectra. 2) manual lines can be drawn using the line tool on the plot and the correct button will use the lines to subtract the baseline.",
           "Baseline Correction Polynomial",
           "Technique",
           "Polynomial",
           "Manual",
           "Correct With Trace",
           "Reset",
           "Range Selection",
           "Spectral Range Help",
           "Restricting the spectral range can remove regions of spectrum where no peaks exist and improve matching",
           "Maximum Spectral Range",
           "Minimum Spectral Range",
           "Identify Spectrum",
           "Identify Spectrum Using the Reference Library",
           "Raman",
           "FTIR",
           "Spectra",
           "Type",
           "Spectrum Type Help",
           "This selection will determine whether the FTIR or Raman matching library is used. Choose the spectrum type that was uploaded.",
           "Analysis",
           "Processed",
           "Uploaded",
           "Spectrum to Analyze Help",
           "This selection will determine whether the uploaded (not processed) spectrum or the spectrum processed using the processing tab is used in the spectrum match.",
           "Region",
           "Full",
           "Peaks",
           "Region to Match Help",
           "This selection will determine whether the library you are matching to consists of the full spectrum or only spectrum peaks.",
           "Partner With Us",
           "Help us reach our goal of revolutionizing spectroscopy.",
           "Donate Cash",
           "Buy From Swag Store",
           "Contribute time",
           "Revolutionizing",
           "Thriving",
           "Maintaining",
           "Supporting",
           "Saving",
           "A paid team that is pushing Open Specy closer to the ultimate goal of 100% accurate spectral identification and deep spectral diagnostics with a single click",
           "A single paid staff person working to update and build the community and the tool",
           "Maintenance costs and minor ad-hoc updates and bug fixes",
           "Keeping the app online and essential maintenance",
           "Long term storage only",
           "Sharing Metadata",
           "Thank you for sharing your data!",
           "Your data will soon be available at https://osf.io/stmv4/",
           "Something went wrong :-(",
           "Data type not supported!",
           "Uploaded data type is not currently supported; please
                      check tooltips and 'About' tab for details.",
           "Sharing Spectrum to Community Library",
           "Reading Spectrum",
           "If you uploaded a text/csv file, make sure that the 
           columns are numeric and named 'wavenumber' and 
           'intensity'.",
           "absorbance intensity [-]",
           "wavenumber [cm<sup>-1</sup>]",
           "Analyzing Spectrum",
           "Finding Match",
           "Making Plot",
           "Material",
           "Pearson's r",
           "Organization",
           "Selectable Matches",
           "Donations",
           "Operation Costs",
           "Selection Metadata"
)

#Before we rerun this we need to make sure that the documents don't get overwritten. version control should help this. Hopefully they aren't too large. 
#Authentication and API pull ----
gl_auth("D:/google_api.json")
langs <- gl_translate_languages()

df <- data.frame(matrix(ncol=length(langs$language), 
                        nrow=length(words), 
                        dimnames=list(NULL, 
                        langs$language)))
df[,"en"] <- words
for(language in langs$language[langs$language != "en"]) {
  df[,language] <- gl_translate(
                    words,
                    target = language,
                    format = c("text"),
                    source = "en",
                    model = "nmt")$translatedText
}

df_2 <- df %>%
  select(where(not_all_na)) %>%
  select(en, everything())

for(column in 2:ncol(df_2)){
  fwrite(df_2[,c(1,column)], paste0("D:/openspecy_languages/translation_", names(df_2)[column], ".csv"), row.names = F)
}
fread_encoding <- function(x) fread(x, encoding = "UTF-8")
file <- fread(list.files("D:/openspecy_languages", ".csv", recursive = T, full.names = T)[2], encoding = "UTF-8")

files <- list.files("D:/openspecy_languages", ".csv", recursive = T, full.names = T)
language_key <- gsub(".csv", "", gsub(".{1,}_", "", files))
json_file <- fromJSON("D:/input.json")

list_files <- lapply(files, fread_encoding)
df_json <- data.frame(matrix(ncol=length(language_key), 
                        nrow=length(list_files[[1]]$en), 
                        dimnames=list(NULL, 
                                      language_key)))
df_json <- df_json %>%
  mutate(en = list_files[[1]]$en) %>%
  select(en, everything())

#file <- list_files[[1]][["af"]]

for(language in 1:length(language_key)) {
  df_json[,language_key[language]] <- list_files[[language]][[language_key[language]]]
}

json_file$languages <- c("en", language_key)
json_file$translation <- df_json

write_json(json_file, "D:/json_test.json")
