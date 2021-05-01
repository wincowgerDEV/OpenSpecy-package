library(mongolite)

database <- mongo(url = readLines("inst/shiny/.db_url"))

alldata <- database$find('{}')

#To test the loggit, remove the .dburl file momentarily and update the location of the library in conf