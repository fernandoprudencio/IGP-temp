#' @title
#' Plot average temperature map of july, august, september, ovtober and november
#'
#' @description
#' this script plot average temperature map of july, august, september, october
#'   and november from climatology of average and maximum monthly data
#'
#' @author Fernando Prudencio

rm(list = ls())

#' INSTALL PACKAGES
pkg <- c(
  "tidyverse", "raster", "ncdf4", "sf", "lattice", "extrafont",
  "cptcity", "latticeExtra", "rasterVis", "maptools", "grid"
)

sapply(
  pkg,
  function(x) {
    is.there <- x %in% rownames(installed.packages())
    if (is.there == FALSE) {
      install.packages(x)
    }
  }
)

#' LOAD PACKAGE
library(tidyverse)
library(raster)
library(ncdf4)
library(sf)
library(lattice)
library(extrafont)
library(cptcity)
library(latticeExtra)
library(rasterVis)
library(maptools)
library(grid)

#' CONSTANTS
k.dep <- c(
  "Piura", "Cajamarca", "La Libertad", "Ancash", "Loreto", "Huancavelica",
  "Amazonas", "Madre de Dios", "Cusco", "Apurimac", "Puno", "Huanuco", "Pasco",
  "Junin"
)
k.months <- c(7:11)
k.data <- "PISCO" # ERA5 or PISCO data
k.type <- "MEAN" # MEAN or MAX monthly original data
k.cond <- "normal" # dry or normal conditions
k.per.str <- 1981

if (k.data == "ERA5") {
  k.per.end <- 2019
}
if (k.data == "PISCO") {
  k.per.end <- 2016
}

#' LOAD RASTER DATA OF GRIDDED TEMPERATURE
ref <- raster("data/raster/pisco/climatology/pisco_mnthlyMEANtemp_climDRY_1981-2016.nc")

input <- sprintf(
  "data/raster/%s/climatology/%s_mnthly%stemp_clim%s_%s-%s.nc",
  tolower(k.data), tolower(k.data), k.type, toupper(str_sub(k.cond, 1, 4)),
  k.per.str, k.per.end
)

grd.avr <- brick(input)[[k.months]] %>%
  mean(na.rm = T) %>% crop(ref) %>% resample(ref)

#' LOAD VECTOR DATA TO PLOT WHIT RASTER OF TEMPERATURE
#'   load world countries limits
#'     load sf data
sf.world <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "world_countries", quiet = T, as_tibble = T
)
#'     load sp data
sp.world <- as(st_geometry(sf.world), Class = "Spatial")

#'   load Peru departaments points
#'     load sf data
sf.peru <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "peru_departaments", quiet = T, as_tibble = T
) %>%
  st_centroid(of_largest_polygon = FALSE) %>%
  filter(Departamen %in% k.dep) %>%
  mutate(Departamen = as.character(Departamen))
#'     load sp data
sp.peru <- as(st_geometry(sf.peru), Class = "Spatial")

#'   load Peru departaments polygon
#'     load sf data
sf.dep <- st_read(
  dsn = "data/vector/limits.gpkg",
  layer = "peru_departaments", quiet = T, as_tibble = T
)
#'     load sp data
sp.dep <- as(st_geometry(sf.dep), Class = "Spatial")

#' SAVE PLOT
png(
  sprintf(
    "exports/%s_%s_conditions_%s-%s.png",
    tolower(k.data), k.cond, k.per.str, k.per.end
  ),
  res = 1000, height = 28, width = 20, units = "cm"
)

levelplot(grd.avr,
  # col.regions = rev(cb_palette),
  main = list(
    sprintf(
      "Average maximum temperature (jul - nov) of %s data\nfor %s conditions from %s to %s",
      k.data, k.cond, k.per.str, k.per.end
    ),
    side = 1, line = 0.5, fontfamily = "Source Sans Pro"
  ),
  scales = list(
    x = list(limits = c(-81.8, -68.2)),
    y = list(limits = c(-18.7, .4))
  ),
  col.regions = cpt(
    pal = "grass_bcyr", n = 100, colorRampPalette = FALSE
  ),
  margin = F,
  pretty = T,
  maxpixels = 15e6,
  at = seq(0, 40, 2),
  colorkey = list(
    at = seq(0, 40, 2),
    space = "right", # location of legend
    labels = list(at = seq(0, 40, 8), cex = 1.1),
    font = list(family = "Source Sans Pro")
  ),
  xlab = NULL,
  ylab = NULL,
  par.settings = list(
    axis.text = list(fontfamily = "Source Sans Pro", cex = 1.2),
    axis.text = list(fontfamily = "Source Sans Pro", cex = 1.2),
    par.xlab.text = list(fontfamily = "Source Sans Pro"),
    par.ylab.text = list(fontfamily = "Source Sans Pro"),
    par.main.text = list(fontfamily = "Source Sans Pro"),
    par.sub.text = list(fontfamily = "Source Sans Pro")
  )
) +
  latticeExtra::layer(
    sp.lines(sp.world, col = "black", lwd = 2),
    #sp.lines(sp.dep, col = "black", lwd = .8),
    sp.points(sp.peru, pch = 20, cex = 1, col = "black"),
    sp.text(coordinates(sp.peru), txt = sf.peru$Departamen, pos = 1, cex = 1.2)
  )

grid::grid.text(
  "[°C]",
  x = unit(.935, "npc"),
  y = unit(.93, "npc"),
  rot = 0,
  gp = gpar(
    fontsize = 18,
    fontface = "bold",
    fontfamily = "Source Sans Pro",
    col = "black"
  )
)

#' CLOSE THE SAVED OF PLOT
dev.off()