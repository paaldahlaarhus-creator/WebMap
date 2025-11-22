library(sf)
library(qs)
library(rmapshaper)
library(here)

# 1. Paths
f_kom <- here("www", "assets", "kommuner.geojson")
f_fylke <- here("www", "assets", "fylker.geojson")

# 2. Read GeoJSON
kommuner <- st_read(f_kom, quiet = TRUE)
fylker   <- st_read(f_fylke, quiet = TRUE)

# 3. Simplify geometries (makes things FAST)
kommuner <- ms_simplify(kommuner, keep = 0.1, keep_shapes = TRUE)
fylker   <- ms_simplify(fylker, keep = 0.1, keep_shapes = TRUE)

# 4. Transform to WGS84 (leaflet needs this)
kommuner <- st_transform(kommuner, 4326)
fylker   <- st_transform(fylker, 4326)

# 5. Save as fast .qs files
qs::qsave(kommuner, here("data", "kommuner.qs"))
qs::qsave(fylker,   here("data", "fylker.qs"))

cat("✓ Data preprocessed and saved in data/*.qs\n")

