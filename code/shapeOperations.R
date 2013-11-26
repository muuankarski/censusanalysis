
library(rgdal)
library(ggplot2)
library(rgeos)

shapefile <- readOGR(dsn = "/home/aurelius/workspace/data/shapefiles/russia/gadm/RUS_adm_simple/", 
                     layer = "RUS_adm2")
shape_karelia <- shapefile[shapefile$NAME_1 == "Karelia", ]
shape_moscow <- shapefile[shapefile$NAME_1 == "Moskva", ]
shape_nizhni <- shapefile[shapefile$NAME_1 == "Nizhegorod", ]

## Karelia
shape_karelia$id <- rownames(shape_karelia@data)
map.points <- fortify(shape_karelia, region = "id")
map.df <- merge(map.points, shape_karelia, by = "id")
save(map.df, file="data/shape/karelia.RData")

## Nizhni
shape_nizhni$id <- rownames(shape_nizhni@data)
map.points <- fortify(shape_nizhni, region = "id")
map.df <- merge(map.points, shape_nizhni, by = "id")
save(map.df, file="data/shape/nizhni.RData")

## Moscow oblast
shape_moscow$id <- rownames(shape_moscow@data)
map.points <- fortify(shape_moscow, region = "id")
map.df <- merge(map.points, shape_moscow, by = "id")
save(map.df, file="data/shape/moscowOb.RData")
