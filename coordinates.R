library(ggmap)
provinces <- read.csv('city_province_cleaned.txt', header = F)
colnames(provinces) <- c('city', 'province')
provinces <- unique(provinces$province)
coordinates <- geocode(as.character(provinces))
coordinates$province <- as.character(provinces)

write.csv(coordinates, "coordinates.csv")

