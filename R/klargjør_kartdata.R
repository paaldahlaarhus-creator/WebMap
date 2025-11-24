library(sf)
library(qs)
library(rmapshaper)
library(here)

# Filstier til rå GeoJSON-data
sti_kommuner_geojson <- here("www", "assets", "kommuner.geojson")
sti_fylker_geojson   <- here("www", "assets", "fylker.geojson")

# Leser inn GeoJSON-filene
kommunedata <- st_read(sti_kommuner_geojson,   quiet = TRUE)
fylkesdata  <- st_read(sti_fylker_geojson, quiet = TRUE)

# Transformer til WGS84 (leaflet krever dette koordinatsystemet)
kommunedata <- st_transform(kommunedata, 4326)
fylkesdata  <- st_transform(fylkesdata, 4326)

# 5. Save as fast .qs files
qsave(kommunedata, here("data", "kommuner.qs"))
qsave(fylkesdata,  here("data", "fylker.qs"))

cat("Kartdata forenklet og lagret som data/*.qs\n")