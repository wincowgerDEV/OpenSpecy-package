

get_lib("medoid_derivative")
lib <- load_lib("medoid_derivative")
#tiny_map <- read_extdata("CA_tiny_map.zip") |> read_any()
wd = "C:\\Users\\winco\\OneDrive\\Documents\\EWG"
setwd(wd)
filename <- "spike2xtime.h5"
dataset <- read_h5(filename)

for(region in c("Region1", "Region2", "Region3")){
  print(region)
  write_spec(filter_spec(dataset, dataset$metadata$region == region), file = gsub("\\.h5", paste0(region, ".rds"), filename) )
}

files <- list.files(path = wd, pattern = "Region.*.rds", full.names = TRUE)

files <- files[!grepl("particles_", files)]

for(file in files){
  print(file)
  automate_particle_analysis(
                  x = file,
                  library = lib,
                  output_dir = wd,
                  material_col = "material_class",
                  spectral_smooth = TRUE,
                  sn_threshold_min = 10^6,
                  sn_threshold_max = Inf,
                  cor_threshold = 0.7,
                  area_threshold = 1,
                  label_unknown = TRUE,
                  remove_materials = NULL,
                  remove_unknown = FALSE,
                  pixel_length = 1,
                  metric = "tot_sig",
                  particle_id_strategy = "collapse",
                  collapse_function = mean,
                  outputs = c("details",  "summary", "processed", "particle_image", "particle_heatmap","particle_heatmap_thresholded","cor_heatmap"),
                  origins = list(x = 0, y = 0)) 
}

#threshold 10^6
Blank_Count_PE = mean(c(1,1,0))
Spike_Count_PE = mean(c(11,49,34))
Spike2x_Count_PE = mean(c(15,10,2))
Drop_Count_PE = mean(c(24,53,18))
Blank_Count_ALL = mean(c(228,568,179))
Spike_Count_ALL = mean(c(169,210,230))
Drop_Count_ALL = mean(c(115,226,117))
Spike2x_Count_ALL = mean(c(163,44,39))

#threshold 2*10^6
Blank_Count_PE = mean(c(0,0,2))
Spike_Count_PE = mean(c(12,2,0))
Drop_Count_PE = mean(c(3,9,3))
Blank_Count_ALL = mean(c(101,193,144))
Spike_Count_ALL = mean(c(107,76,96))
Drop_Count_ALL = mean(c(42,145,75))

#threshold 10^7
Blank_Count_PE = mean(c(0,0,0))
Spike_Count_PE = mean(c(0,0,0))
Drop_Count_PE = mean(c(0,0,0))
Blank_Count_ALL = mean(c(62,37,77))
#Spike_Count_ALL = mean(c(169,210,230))
Drop_Count_ALL = mean(c(11,28,33))

                
test$samples[[1]]$particle_image_png |> replayPlot()
test$samples[[1]]$particle_heatmap_png |> replayPlot()
test$samples[[1]]$particle_heatmap_thresholded_jpg
test$samples[[1]]$cor_heatmap_png

img <- test$samples[[1]]$particle_image_png

png("particle_image.png", width = 1200, height = 1200, res = 150)
replayPlot(img)
dev.off()
