reticulate::use_virtualenv('rgee')
library(rgee)
ee$Initialize(project = "prject-id-here")
library(reticulate)
import('eemont')
library(sf)
library(furrr)
library(terra)
library(tidyverse)
library(here)

bbox <- st_bbox(c(xmin = -63.87677,
                  ymin = -13.26560,
                  xmax = -63.16315,
                  ymax = -12.50203),
                crs = 4326)

bbox_ee <- sf_as_ee(st_as_sfc(bbox))
tiles <- readRDS(here('data/derived/tiles_768_sf.rds'))

canopy_ee <- ee$Image('projects/sat-io/open-datasets/CTREES/AMAZON-CANOPY-TREE-HT')$
  divide(2.5)

canopy_height <- function(tile){
  # Get bounding box and calculate dimensions
  tile_bbox <- st_bbox(tile)
  width_m <- diff(tile_bbox[c("xmin", "xmax")])
  height_m <- diff(tile_bbox[c("ymin", "ymax")])

  # Calculate pixel dimensions (10m per pixel)
  width_px <- round(width_m / 8.983153e-05)
  height_px <- round(height_m / 8.983153e-05)

  # Create dimensions string
  dimensions_str <- paste0(width_px, "x", height_px)

  canopy_ee$
    getThumbURL(list(region = sf_as_ee(tile),
                     dimensions = dimensions_str,
                     format = 'jpg',
                     min = 0,
                     max = 35,
                     palette = c("#000000", "#1a0033", "#330066", "#004d66",
                                 "#006666", "#009966", "#33cc66", "#66ff33", "#ccff00")))
}


canopy_url <- map(tiles$geometry, canopy_height)

dirs <-  list.files(here('data/derived/tile_768/'), full.names = TRUE) |>
  gtools::mixedsort()

plan(multisession)
walk2(
  .x = canopy_url,
  .y = dirs,
  ~ download.file(.x, file.path(.y, "canopy_chip.jpg"), quiet = TRUE),
  .progress = TRUE
)
plan(sequential)


band_names <- c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B8A', 'B9', 'B11', 'B12', 'EVI', 'MCARI', 'NDMI', 'NDREI', 'NBR')

s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  filterBounds(rgee::sf_as_ee(cl$geometry))$
  filter(ee$Filter$calendarRange(2023, 2023, 'year'))$
  filter(ee$Filter$calendarRange(6, 9, 'month'))$
  filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 50))$
  preprocess(cloudDist = 1500, buffer = 750)$
  select(c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B8A', 'B9', 'B11', 'B12'))$
  spectralIndices(c('EVI', 'MCARI','NDMI', 'NDREI', 'NBR'))

s2_composite <- s2$reduce(ee$Reducer$percentile(list(25L)))$
  rename(band_names)


grids <- grid |>
  mutate(reg = map(x, ~rgee::sf_as_ee(.x)),
         url = map(reg, ~s2_composite$
                     getDownloadURL(dict(
                       region = .x,
                       scale =  10,
                       format = 'GEO_TIFF',
                       filePerBand = FALSE
                     ))
         ))


dl <- function(x, y){
  options(timeout = max(900, getOption("timeout")))
  download.file(x, paste0('~/Downloads/itenez_', y, '_stack.tif'))
}

plan(multisession)
future_imap(grids$url, dl, .progress = TRUE)
plan(sequential)


s2_dat <- list.files(here('data/derived'), pattern = 'itenez_.*_stack.tif', full.names = TRUE) |> # remove harmonized
  sprc() |>
  # crop(ext(extent_sf)) |>
  mosaic() |>
  setNames(band_names)

writeRaster(s2_dat, filename = here('data/derived/itenez_s2_stack.tif'), overwrite = TRUE)
