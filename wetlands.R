occs_all1[species == "Eriophorum vaginatum", ]
occs_all1[grepl("Eriophorum", occs_all1$species), ]

library(PreSPickR)
library(data.table)


if(Sys.info()[4] == "D01RI1700308") {
  wd <- ""
}else if(Sys.info()[4] == "S-JRCIPRAP320P") {
  wd <- ""
}else if(Sys.info()[4] %in% c("jeodpp-terminal-jd001-03", "jeodpp-terminal-03")) {
  if(!dir.exists("/eos/jeodpp/home/users/rotllxa/wetlands")) 
    dir.create("/eos/jeodpp/home/users/rotllxa/wetlands")
  wd <- "/eos/jeodpp/home/users/rotllxa/wetlands"
  gbif_creds <- "/home/rotllxa/Documents/"
}else{
  wd <- ""
  gbif_creds <- "C:/Users/rotllxa/Documents/"
}

setwd(wd)



sp_list <- c("Eriophorum vaginatum", "Sphagnum fuscum", "Sphagnum medium", "Sphagnum rubellum")



num_eu_occs_df <- c()
count <- 1
countr <- c("BE", "EL", "LT", "PT", "BG", "ES", "LU", "RO", "CZ", "FR", "HU", "SI", "DK", "HR", "MT", "SK", "DE", "IT", "NL", "FI", "EE", "CY", "AT", "SE", "IE", "LV", "PL")
countr <- sort(countr)

for(sp in sp_list){
  sp_key <- as.data.frame(name_backbone(name = sp))$usageKey
  num_eu_occs <- 0
  for(c in countr){
    num_occs <- occ_count(taxonKey = sp_key,
                          country = c,
                          from = 2000,
                          to = 2021)
    num_eu_occs <- num_eu_occs + num_occs
  }
  num_eu_occs_df <- rbind(num_eu_occs_df, data.frame(sp, sp_key, num_eu_occs))
  print(paste0(sp, " - sp ", count, ": ", num_eu_occs))
  count <- count + 1
}

write.csv(num_eu_occs_df, "Number_occs_sp_EU.csv", row.names = FALSE)
num_eu_occs_df <- fread("Number_occs_sp_EU.csv", header = TRUE)
num_eu_occs_df



t0 <- Sys.time()
GetBIF(credentials = paste0(gbif_creds, "/gbif_credentials.RData"),
       taxon_list = sp_list,
       download_format = "SIMPLE_CSV",
       download_years = c(2000, 2021),
       download_coords = c(-12.69141, 42.71485, 33.4901, 71.9218), #order: xmin, xmax, ymin, ymax
       #download_coords_accuracy = c(0, 50),  # Remember that 0-50m is the default!! 
       download_coords_accuracy = c(0, 250),  #
       rm_dupl = FALSE,
       cols2keep = c("species", "decimalLatitude", "decimalLongitude", #"elevation",
                     "gbifID",
                     "coordinateUncertaintyInMeters",
                     "countryCode", "year", 
                     #"institutionCode",	"collectionCode",
                     #"ownerInstitutionCode",
                     "datasetKey"
       ),
       out_name = paste0("sp_records_", format(Sys.Date(), "%Y%m%d", "_2.csv")))

Sys.time() - t0



## if GetBIF didn't manage to create/write out the data frame with presences:
taxon_dir <- getwd()
#taxons <- taxons$sp
data1 <- Prep_BIF(taxon_dir = paste0(taxon_dir, "/"),
                  taxons = sp_list,
                  cols2keep = c("species", "decimalLatitude", "decimalLongitude", #"elevation",
                                "gbifID",
                                "coordinateUncertaintyInMeters",
                                "countryCode", "year", 
                                #"institutionCode",	"collectionCode",
                                #"ownerInstitutionCode",
                                "datasetKey"
                  ),
                  #cols2keep = "all",
                  rm_dupl = TRUE)

data1
head(data1)
unique(data1$species)
table(data1$species)

sum(num_eu_occs_df$num_eu_occs)
sum(table(data1$species))

data_sp_year <- data1[, .SD, .SDcols = c("species", "year")] %>% group_by(species) %>% table
data_sp_year
sum(apply(data_sp_year, 1, sum))


write.csv(data1, file = paste0("sp_records_", format(Sys.Date(), "%Y%m%d"), ".csv"),
          quote = FALSE, row.names = FALSE)
#data1 <- fread(paste0("sp_records_", format(Sys.Date(), "%Y%m%d"), ".csv"))
data1 <- fread(paste0("sp_records_20220127", ".csv"))

#



occs_all_shp <- SpatialPointsDataFrame(coords = data1[, c("decimalLongitude", "decimalLatitude")],
                                       #data = data.frame(sp = rep(1, nrow(occs_i))),
                                       data = data1[, .SD, .SDcols = "species"],
                                       proj4string = CRS("+init=EPSG:4326"))

occs_all_shp

dev.off()
plot(occs_all_shp[occs_all_shp$species == "Eriophorum vaginatum", ])

library(rworldmap)
wrld_map <- getMap()
wrld_map

library(raster)
library(rgdal)
wrld_map <- spTransform(wrld_map, crs(occs_all_shp))


pdf("Eriophorum_vaginatum_occs.pdf")

plot(occs_all_shp[occs_all_shp$species == "Eriophorum vaginatum", ])
plot(wrld_map, add = TRUE)

dev.off()


plot(occs_all_shp)


pdf("all_sp_occs.pdf")
par(mar = c(6, 4, 4, 4))
cols <- c("black", "blue", "green", "red")
plot(occs_all_shp[occs_all_shp$species %in% sp_list[1], ], col = cols[1])
for(sp in 2:length(sp_list)){
  print(sp_list[sp])
  print(cols[sp])
  plot(occs_all_shp[occs_all_shp$species %in% sp_list[sp], ], col = cols[sp], add = TRUE)
  
}
plot(wrld_map, add = TRUE)
par(xpd = TRUE)
legend("bottom", sp_list, fill = cols, ncol = 2, inset = -0.2)

dev.off()


pdf("all_sp_occs_separatedMaps.pdf")
par(mfrow = c(2, 2))
par(mar = c(6, 4, 4, 4))
cols <- c("purple", "blue", "green", "red")
for(sp in 1:length(sp_list)){
  par(xpd = FALSE)
  print(sp_list[sp])
  print(cols[sp])
  plot(occs_all_shp[occs_all_shp$species %in% sp_list[sp], ], col = cols[sp])
  plot(wrld_map, add = TRUE)
  par(xpd = TRUE)
  legend("bottom", sp_list[sp], fill = cols[sp], ncol = 1, inset = -0.2)
}

dev.off()



## ggplot maps ####

data1

library(sf)
library(ggplot2)
library(ggExtra)
library(viridis)  
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
# The “viridis” and “magma” scales do better - they cover a wide perceptual range in brightness in brightness and blue-yellow, 
# and do not rely as much on red-green contrast
library(ggforce)
library(ggpubr)
library(patchwork)
library(giscoR)
library(dplyr)

data1_sf <- st_as_sf(data1, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
data1_sf

wrld_map <- st_as_sf(wrld_map)
names(wrld_map)
unique(wrld_map$continent)
unique(wrld_map$REGION)
sort(unique(wrld_map$ADMIN))

sf::sf_use_s2(FALSE) # https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data

#eur_map <- wrld_map[wrld_map$GLOCAF %in% "Europe" | wrld_map$ADMIN == "Russia", ]
#eur_map <- wrld_map[wrld_map$GLOCAF %in% "Europe", ]
#eur_map


eur_map <- wrld_map[wrld_map$continent %in% "Eurasia", ]

eur_map <- st_crop(eur_map, xmin = -10.5, xmax = 50, ymin = 33, ymax = 72)


ggplot(data1_sf) + 
  geom_sf() + 
  coord_sf()

ggplot(eur_map) + 
  geom_sf() + 
  coord_sf()



## Gisco maps
# https://ropengov.github.io/giscoR/

eur_gisco <- gisco_get_countries(region = "Europe")
eur_gisco
plot(eur_gisco[, 2])

fr_gisco <-
  gisco_get_countries(
    #country = c("Morocco", "Argelia", "Libia", "Tunisia", "Egypt"),
    country = c("France"),
    resolution = "20",
    epsg = "4326",
    year = "2020"
  )
ggplot(fr_gisco[, 1]) +
  geom_sf(color = "grey80") 

coast <- gisco_get_coastallines(
  resolution = "20",
  epsg = "4326",
  year = "2016"
)

coast <- gisco_get_coastallines(
  resolution = "20",
  epsg = "3035",
  year = "2016"
)

ggplot(eur_gisco) +
  geom_sf(color = "grey80") +
  geom_sf(data = fr_gisco, fill = "grey30", color = "white") +
  coord_sf(
    xlim = c(-13, 37),
    ylim = c(40, 60)
  )

 
eu2016 <- c("UK", gisco_countrycode[gisco_countrycode$eu, ]$CNTR_CODE)

nuts0 <- gisco_get_nuts(
  year = "2016",
  epsg = "3035",  # LAEA
  resolution = "3",
  nuts_level = "0",
  country = eu2016
)

ggplot(nuts0) +
  geom_sf()

ggplot(coast) +
  geom_sf() +
  coord_sf(   # WGS84
    xlim = c(-10.5, 50),
    ylim = c(33, 72)
  )

ggplot(coast) +
  geom_sf() +
  coord_sf(     # LAEA
    xlim = c(2600000, 7000000),
    ylim = c(1500000, 5400000)
  )



ggplot(eur_gisco) +
  geom_sf() +
  coord_sf(   # WGS84
    xlim = c(-10.5, 50),
    ylim = c(33, 72)
  )

 
eur_gisco <- st_crop(eur_gisco, xmin = -10.5, xmax = 50, ymin = 33, ymax = 72)
  



## All 4 species together

p <- ggplot() +
  geom_sf(data = eur_gisco) +
  geom_point(
    data = data1, 
    #data = data1[data1$species == "Eriophorum vaginatum", ], 
    aes(x = decimalLongitude, y = decimalLatitude, 
        color = species),
    size = 0.1
  ) +
  
  theme_light() +
  scale_color_viridis(option = "viridis", discrete = TRUE) +
  labs(title = "GBIF occurrences 2000-2021") + #, x = "TY [°C]", y = "Txxx") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size = 16)) +
  guides(color = guide_legend("Species", override.aes = list(size = 2)))

# https://jtr13.github.io/cc21fall2/tutorial-for-scatter-plot-with-marginal-distribution.html
p1 <- ggMarginal(p,
                 aes(colour = species),
                 type = "density", 
                 #type = "histogram", 
                 #type = "densigram", 
                 groupColour = TRUE, groupFill = TRUE)
p1



## 4 species separatedly

p2 <- ggplot() +
  geom_sf(data = eur_gisco) +
  geom_point(
    data = data1, 
    aes(x = decimalLongitude, y = decimalLatitude, 
        color = species),
    size = 0.01
  ) +
  #facet_zoom(x = decimalLongitude < 2)+
  #facet_zoom(x = species == "Eriophorum vaginatum ")+
  facet_wrap(~ species, ncol = 2) + 
  theme_light() +
  scale_color_viridis(option = "viridis", discrete = TRUE) +
  labs(title = "GBIF occurrences 2000-2021") + #, x = "TY [°C]", y = "Txxx") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size = 16)) +
  guides(color = guide_legend("Species", override.aes = list(size = 2)))



list(p1, p2) %>%         # https://stackoverflow.com/questions/72442442/properly-size-multiple-ggextraplot-objects-after-calling-ggmarginal-in-r
  wrap_plots(nrow = 1, widths = c(2, 1.5))

# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html

#dev.new()
#p2


ggsave("GBIF_occurrences_4sps.png")#, width = 20, height = 20, units = "cm")




## Eriophorum vaginatum

p3 <- ggplot() +
  geom_sf(data = eur_gisco) +
  geom_point(
    #data = data1, 
    data = data1[data1$species == "Eriophorum vaginatum", ], 
    aes(x = decimalLongitude, y = decimalLatitude, 
        color = species),
    size = 0.1
  ) +
  theme_light() +
  scale_color_viridis(option = "viridis", discrete = TRUE) +
  labs(title = "GBIF occurrences 2000-2021") + #, x = "TY [°C]", y = "Txxx") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size = 16)) +
  guides(color = guide_legend("Species", override.aes = list(size = 2)))


p4 <- ggMarginal(p3,
                 aes(colour = species),
                 type = "density", groupColour = TRUE, groupFill = TRUE)
p4

ggsave(file = paste0("GBIF_occurrences_", "eri_vag", ".png"), p4)#, width = 20, height = 20, units = "cm")

