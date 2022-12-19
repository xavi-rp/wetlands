
if(Sys.info()[4] == "D01RI1700308") {
  wd <- ""
}else if(Sys.info()[4] == "S-JRCIPRAP320P") {
  wd <- ""
}else if(Sys.info()[4] %in% c("jeodpp-terminal-jd001-03", "jeodpp-terminal-03", "jeodpp-terminal-dev-12" )) {
  if(!dir.exists("/eos/jeodpp/home/users/rotllxa/wetlands")) 
    dir.create("/eos/jeodpp/home/users/rotllxa/wetlands")
  wd <- "/eos/jeodpp/home/users/rotllxa/wetlands"
  gbif_creds <- "/home/rotllxa/Documents/"
}else if(Sys.info()[4] %in% c("L2100739RI")) {
  if(!dir.exists("C:/Users/rotllxa/D5_FFGRCC_peatlands_data")) 
    dir.create("C:/Users/rotllxa/D5_FFGRCC_peatlands_data")
  wd <- "C:/Users/rotllxa/D5_FFGRCC_peatlands_data"
  gbif_creds <- "C:/Users/rotllxa"
}else{
  wd <- ""
  gbif_creds <- ""
}

setwd(wd)





library(data.table)
#library(PreSPickR)
library(ggplot2)
library(viridis)
library(raster)
library(sf)



## loading data from the survey ####

## https://www.nature.com/articles/s41467-021-25619-y
data_survey <- fread(paste0(wd, "/sepla/data_fen-rewetting_denaturated0.1.csv"), header = TRUE)

data_survey
View(data_survey)
nrow(data_survey)
ncol(data_survey)
names(data_survey)



## Checking data set ####

## survey point data and species 
data_survey_sps <- data_survey[, 1:552] 
#data_survey_kk <- data_survey[, c(1:12, 553:560)] # don't know what's "Maximum", "Minimum", "Amplitude"

data_survey_sps[, 1:12]
sort(unique(data_survey_sps[, 1:12]$Year))
sort(unique(data_survey_sps[, 1:12]$YearRestoration))

sum(is.na(data_survey_sps$shannon))
View(data_survey_sps[is.na(data_survey_sps$shannon), 1:12]) # not all 111 NAs in "shannon" are rewetted or natural

sort(unique(data_survey_sps$DrainStatus)) # "natural"  "rewetted"
sort(unique(data_survey_sps$YearRestoration)) # year it was restored, if it was
sort(unique(data_survey_sps$Year)) # year of the survey: 1994, 1997, 2002-2019

summary(data_survey_sps$shannon)
#    Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    NA's 
# 0.0079  1.1274   1.6339   1.5827  2.0770    3.8311     111 


sort(unique(data_survey_sps$Cov_peat))
sort(unique(data_survey_sps$Cov_moss))

sort(unique(data_survey_sps$MgtStatus.simple)) # "grazing/mowing" "no use"  

sort(unique(data_survey$pH_water)) # 
summary(data_survey$pH_water) #
# Min.   1st Qu.  Median    Mean   3rd Qu.    Max.    NA's 
# 5.400   6.380   6.520    6.482   6.670     7.030     616 

sum(!is.na(data_survey$pH_water))  # only 57 points with reported water pH
                                   # Not sure if water pH is proportional to soil pH??
# "Bogs and poor fens are acidic and dominated by peat mosses (Sphagnum), 
#  while rich fens are basic and dominated by true mosses" 
#  (https://www.sciencedirect.com/topics/agricultural-and-biological-sciences/peatlands)


summary(data_survey$BulkDensity) #



## Getting "natural" survey points

data_survey_sps_natural <- data_survey_sps[DrainStatus == "natural", ]
data_survey_sps_natural[, 1:12]

data_survey_sps_rewetted <- data_survey_sps[DrainStatus == "rewetted", ]
data_survey_sps_rewetted[, 1:12]



summary(data_survey_sps_natural$shannon)
summary(data_survey_sps_rewetted$shannon)

mean(data_survey_sps_natural$shannon, na.rm = TRUE)   # 1.738807   Natural, more diverse
mean(data_survey_sps_rewetted$shannon, na.rm = TRUE)  # 1.464567

sd(data_survey_sps_natural$shannon, na.rm = TRUE)   # 0.6432756
sd(data_survey_sps_rewetted$shannon, na.rm = TRUE)  # 0.6723645



## Getting species (natural)

View(data_survey_sps_natural)
nrow(data_survey_sps_natural)
ncol(data_survey_sps_natural)
str(data_survey_sps_natural)

apply(data_survey_sps_natural, 2, unique)

## number of occs per species
data_survey_sps_natural_ocs <- apply(data_survey_sps_natural[, 13:ncol(data_survey_sps_natural)], 2, function(x) sum(x != 0, na.rm = TRUE))
head(sort(data_survey_sps_natural_ocs, decreasing = TRUE), 20)



## Getting species (rewetted)
## number of occs per species
data_survey_sps_rewetted_ocs <- apply(data_survey_sps_rewetted[, 13:ncol(data_survey_sps_rewetted)], 2, function(x) sum(x != 0, na.rm = TRUE))
head(sort(data_survey_sps_rewetted_ocs, decreasing = TRUE), 20)



## Species appearing only in natural
length(data_survey_sps_natural_ocs[data_survey_sps_natural_ocs != 0]) 
length(data_survey_sps_natural_ocs[data_survey_sps_natural_ocs == 0]) 

sps_natural <- data_survey_sps_natural_ocs[data_survey_sps_natural_ocs != 0]
sps_natural <- names(sps_natural)

sps_rewetted <- data_survey_sps_rewetted_ocs[data_survey_sps_rewetted_ocs != 0]
sps_rewetted <- names(sps_rewetted)


sps_natural[sps_natural %in% sps_rewetted] # 239 sps from Natural also present in Rewetted
sps_natural_only <- sps_natural[!sps_natural %in% sps_rewetted] # 175 sps are only in Natural
sps_natural_only


## Species likely more strict from peatlands (not found in rewetted)
sps_natural_only_occs <- data_survey_sps_natural_ocs[names(data_survey_sps_natural_ocs) %in% sps_natural_only]
sps_natural_only_occs

sps_natural_only_occs <- data.table(species = names(sps_natural_only_occs), num_points = sps_natural_only_occs)
sps_natural_only_occs

sps_natural_only_occs <- sps_natural_only_occs[order(-rank(num_points))]
sps_natural_only_occs

write.csv(sps_natural_only_occs, file = "sps_natural_only_occs.csv", row.names = FALSE)
sps_natural_only_occs <- fread("sps_natural_only_occs.csv", header = TRUE)

## Hamatocaulis vernicosus is the accepted name
sps_natural_only_occs$species <- gsub("Drepanocladus.vernicosus", "Hamatocaulis.vernicosus", sps_natural_only_occs$species)


summary(sps_natural_only_occs$num_points)
#   Min.   1st Qu.  Median    Mean    3rd Qu.    Max. 
#    1.0     1.0     2.0        4.2     5.0    27.0 

quantile(sps_natural_only_occs$num_points, seq(0, 1, 0.1))
#  0%   10%  20%  30%  40%  50%  60%  70%  80%  90%    100% 
# 1.0   1.0  1.0  1.0  1.0  2.0  3.0  4.0  6.2  11.0   27.0 

sort(unique(sps_natural_only_occs$num_points))


## occurrences of all Sphagnus species appearing only in natural peatlands (not-rewetted)
sps_natural_only_occs[grepl("Sphag", species)]





## Number of GBIF occurrences for the sps found only in "natural" peatlands  ####

sps_natural_only_occs <- fread("sps_natural_only_occs.csv", header = TRUE)


countr <- c("BE", "EL", "LT", "PT", "BG", "ES", "LU", "RO", "CZ", "FR", "HU", "SI", "DK", "HR", "MT", "SK", "DE", "IT", "NL", "FI", "EE", "CY", "AT", "SE", "IE", "LV", "PL")
countr <- sort(countr)
length(countr)


num_eu_occs_df <- c()
count <- 1
#sp <- sps_natural_only_occs$species[1]

for(sp in sps_natural_only_occs$species){
  sp <- gsub("\\.", " ", sp)
  sp_key <- as.data.frame(name_backbone(name = sp))$usageKey
  num_eu_occs <- 0
  if(!is.null(sp_key)){
    for(c in countr){
      num_occs <- occ_count(taxonKey = sp_key,
                            country = c,
                            from = 1990,
                            to = 2022)
      num_eu_occs <- num_eu_occs + num_occs
    }
  }else{
    sp_key <- NA
    num_eu_occs <- NA
  }
  num_eu_occs_df <- rbind(num_eu_occs_df, data.frame(sp, sp_key, num_eu_occs))
  print(paste0(sp, " - sp ", count, "/", length(sps_natural_only_occs$species), ": ", num_eu_occs))
  count <- count + 1
}


num_eu_occs_df

write.csv(num_eu_occs_df, "Number_occs_GBIF_EU27.csv", row.names = FALSE)
num_eu_occs_df <- fread("Number_occs_GBIF_EU27.csv", header = TRUE)



num_eu_occs_df_1 <- na.omit(num_eu_occs_df)
num_eu_occs_df_1$sp <- factor(num_eu_occs_df_1$sp, levels = num_eu_occs_df$sp)


png("num_occs_GBIF_EU27.png", width = 15, height = 20, units = "cm", res = 150)
num_eu_occs_df_1 %>%
  ggplot(aes(x = reorder(sp, desc(sp)), y = num_eu_occs)) + 
  geom_bar(stat = "identity", fill = viridis(length(num_eu_occs_df_1$sp))) +
  ggtitle("GBIF occurrences (natural) peatland plants") +
  labs(x = "Species", y = "Number of Occurrences GBIF (1990-2022)") +
  #theme(plot.title = element_text(color="red", size=14, face="bold.italic")) +
  theme(plot.title = element_text(hjust = 0.3, size = 12, face = "bold"),
        axis.text = element_text(size = 4),
        axis.title = element_text(size = 8)) +
  coord_flip()
dev.off()



## Downloading data form GBIF ####

#sps_natural_only_occs <- fread("sps_natural_only_occs.csv", header = TRUE)
num_eu_occs_df <- fread("Number_occs_GBIF_EU27.csv", header = TRUE)
num_eu_occs_df <- na.omit(num_eu_occs_df)
taxons <- num_eu_occs_df$sp
taxons <- gsub("\\.", " ", taxons)
taxons <- gsub("_t", "", taxons)


GetBIF(credentials = paste0(gbif_creds, "/gbif_credentials.RData"),
       taxon_list = taxons,
       #taxon_list = s,
       download_format = "SIMPLE_CSV",
       download_years = c(1990, 2022),
       download_coords = c(-13, 48, 35, 72), #order: xmin, xmax, ymin, ymax
       download_coords_accuracy = c(0, 250),
       rm_dupl = TRUE,
       cols2keep = c("species", "decimalLatitude", "decimalLongitude", #"elevation",
                     "gbifID",
                     "coordinateUncertaintyInMeters",
                     "countryCode", "year", 
                     #"institutionCode",	"collectionCode",
                     #"ownerInstitutionCode",
                     "datasetKey"),
       out_name = paste0("sp_records_", format(Sys.Date(), "%Y%m%d")))



## if GetBIF didn't manage to create/write out the data frame with presences:
taxon_dir <- getwd()
#taxons <- taxons$sp

data1 <- Prep_BIF(taxon_dir = paste0(taxon_dir, "/"),
                  taxons = taxons,
                  cols2keep = c("species", "decimalLatitude", "decimalLongitude", #"elevation",
                                "gbifID",
                                "coordinateUncertaintyInMeters",
                                "countryCode", 
                                "eventDate", "day", "month",
                                "year", 
                                #"institutionCode",	"collectionCode",
                                #"ownerInstitutionCode",
                                "datasetKey"
                  ),
                  #cols2keep = "all",
                  rm_dupl = TRUE)

head(data1)
nrow(data1)
unique(data1$species)
sort(unique(data1$year))

if(length(unique(data1$species)) != length(unique(data1$sp2))){
  data1_kk <- data1
  print("Check the error in 'sp2'!!!")
  data.table(unique(data1$species), unique(data1$sp2))
  
  dt2fix_sp2 <- data1[, .SD, .SDcols = c("species", "sp2")]
  dt2fix_sp2 <- dt2fix_sp2[!duplicated(dt2fix_sp2$species), ]
  length(unique(dt2fix_sp2$species))
  length(unique(dt2fix_sp2$sp2))
  
  dt2fix_sp2 <- dt2fix_sp2[duplicated(dt2fix_sp2$sp2), ]
  setkeyv(dt2fix_sp2, "sp2")
  
  dt2fix_sp2
  
  for(s in unique(dt2fix_sp2$sp2)){
    dt2fix_sp2_1  <- unique(data1[sp2 %in% s]$species)
    for(s1 in (1:length(dt2fix_sp2_1))){
      #data1[species %in% dt2fix_sp2_1[s1]]$sp2 <- gsub('.{7}$', " ", data1[species %in% dt2fix_sp2_1[s1]]$sp2)
    }
    
  }

  
  
} 


head(sort(table(data1$species), decreasing = TRUE), 10)
sp_more_occs_10 <- names(head(sort(table(data1$species), decreasing = TRUE), 10))


data_sp_year <- data1[, .SD, .SDcols = c("species", "year")] %>% group_by(species) %>% table
data_sp_year
sort(apply(data_sp_year, 2, sum))  # in 1990s there are less occurrences (aggregated species)


## Saving data set
print(paste0("Saving GBIF data as ", "/sp_records_20220922", ".csv"))
write.csv(data1, file = paste0("sp_records_20220922", ".csv"),
          quote = FALSE, row.names = FALSE)


data <- fread(paste0("sp_records_20220922", ".csv"), header = TRUE)
data


## Citing information
load("download_info_Epipactis palustris.RData", verbose = TRUE)
citation_02





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

#data1_sf <- st_as_sf(data1, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
#data1_sf


## Plotting the 4 species most reported in the survey
sps_4 <- sps_natural_only_occs$species[5:8]
sps_4 <- gsub("\\.", " ", sps_4)
sps_4 <- gsub("_t", "", sps_4)
sps_4

data1_sps <- data1[species %in% sps_4, ]
data1_sps

table(data1_sps$species)


## Gisco maps
# https://ropengov.github.io/giscoR/

eur_gisco <- gisco_get_countries(region = "Europe")
eur_gisco

eur_gisco <- st_crop(eur_gisco, xmin = -10.5, xmax = 50, ymin = 33, ymax = 72)


## All 4 species together

p <- ggplot() +
  geom_sf(data = eur_gisco) +
  geom_point(
    data = data1_sps, 
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
    data = data1_sps, 
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



ggsave("GBIF_occurrences_4sps.png")#, width = 20, height = 20, units = "cm")






## Peatlands map ####
## Tanneberger et al., 2017. DOI: 10.19189/MaP.2016.OMB.264

library(raster)
#library(terra)
library(rasterVis)
library(viridis)
library(ggplot2)
library(ggpubr)
library(tidyverse)


#peatl_map <- rast(paste0("sepla/peatlands_map/tanneberger/", "EMB_peatl_int150m_WGS84.tif")) 
peatl_map <- raster(paste0("sepla/peatlands_map/tanneberger/", "EMB_peatl_int150m_WGS84.tif")) 
peatl_map
str(peatl_map)

peatl_map_pts <- rasterToPoints(peatl_map, spatial = TRUE)
head(peatl_map_pts)

peatl_map_df <- data.frame(peatl_map_pts)
head(peatl_map_df)
nrow(peatl_map_df)


peatl_map_df_1 <- peatl_map_df %>% mutate(across(c(x, y), round, digits = 4))
head(peatl_map_df_1)


jpeg("/eos/jeodpp/home/users/rotllxa/wetlands/peatl_map.jpg")
ggplot() +
  geom_raster(data = peatl_map_df_1, aes(x, y, fill = OID))

dev.off()














## Number of GBIF occurrences for the genus Sphagnum  ####

countr <- c("BE", "EL", "LT", "PT", "BG", "ES", "LU", "RO", "CZ", "FR", "HU", "SI", "DK", "HR", "MT", "SK", "DE", "IT", "NL", "FI", "EE", "CY", "AT", "SE", "IE", "LV", "PL")
countr <- sort(countr)
length(countr)


num_eu_occs_df <- c()
count <- 1
#sp <- sps_natural_only_occs$species[1]

for(sp in "Sphagnum"){
  sp <- gsub("\\.", " ", sp)
  sp_key <- as.data.frame(name_backbone(name = sp))$usageKey
  num_eu_occs <- 0
  if(!is.null(sp_key)){
    for(c in countr){
      num_occs <- occ_count(taxonKey = sp_key,
                            country = c,
                            georeferenced = TRUE,
                            from = 1990,
                            to = 2022)
      num_eu_occs <- num_eu_occs + num_occs
    }
  }else{
    sp_key <- NA
    num_eu_occs <- NA
  }
  num_eu_occs_df <- rbind(num_eu_occs_df, data.frame(sp, sp_key, num_eu_occs))
  print(paste0(sp, " - sp ", count, "/", length(sps_natural_only_occs$species), ": ", num_eu_occs))
  count <- count + 1
}


num_eu_occs_df








## GBIF occurrences for peatlands plants ####

# The data set has been downloaded from by Carolina Puerta-Pinero on 28/11/2022


gbif_data_dir <- "/eos/jeodpp/home/users/puercar/Peatlands/"

if(!dir.exists(paste0(getwd(), "/gbif_occs_carolina/"))) dir.create(paste0(getwd(), "/gbif_occs_carolina/"))

#unzip(paste0(gbif_data_dir, "/0177592-220831081235567.zip"), exdir = paste0(getwd(), "/gbif_occs_carolina/")) 
# This extraction needs to be done manually, as the process in R gets truncated at 4GB

gbif_data_all <- fread(paste0(getwd(), "/gbif_occs_carolina/0177592-220831081235567.csv"), header = TRUE)

nrow(gbif_data_all)   # 58481926
names(gbif_data_all)

length(unique(gbif_data_all$species))   # 25674
sort(unique(gbif_data_all$year))        # 1980-2022
sort(unique(gbif_data_all$countryCode)) # several errors (e.g. US)
sum(is.na(gbif_data_all$countryCode)) #  0
range(gbif_data_all$coordinateUncertaintyInMeters) #  0.01 350.00


gbif_data_all_clean <- gbif_data_all[, .SD, .SDcols = c("family", "genus", "species", 
                                                        "decimalLongitude", "decimalLatitude",
                                                        "gbifID", 
                                                        "countryCode", 
                                                        "coordinateUncertaintyInMeters",
                                                        "year")]

gbif_data_all_clean <- gbif_data_all_clean[year %in% c(2000:2022), ]
nrow(gbif_data_all_clean)  # 48192359
gbif_data_all_clean <- gbif_data_all_clean[coordinateUncertaintyInMeters %in% c(0:150), ]
nrow(gbif_data_all_clean)  # 38582406

setnames(gbif_data_all_clean, c("decimalLongitude", "decimalLatitude"), c("x", "y"))
gbif_data_all_clean
sort(unique(gbif_data_all_clean$countryCode))

write.csv(gbif_data_all_clean, file = "gbif_data_all_clean.csv", quote = FALSE, row.names = FALSE)

#gbif_data_all <- gbif_data_all_clean
gc()


### Extracting occs in peatlands ####

## Peatlands map (Tanneberger)
peatl_map <- raster(paste0("sepla/peatlands_map/tanneberger/", "EMB_peatl_int150m_WGS84.tif")) 

##cat_coords <-  c(3500000, 3800000, 1900000, 2300000)   # Catalonia (LAEA, m) (xmin, xmax, ymin, ymax)
#cat_coords <-  c(-0.5, 3.5, 42, 44)   # North Catalonia (WGS84) (xmin, xmax, ymin, ymax)
#peatl_map_cat <- crop(peatl_map, extent(cat_coords))
#peatl_map_cat
#plot(peatl_map_cat)



gbif_data_all_sf <- st_as_sf(as.data.frame(gbif_data_all_clean), coords = c("x", "y"), crs = 4326)#, agr = "constant")
gbif_data_all_sf

t0 <- Sys.time()
occs_peatl_map <- as.data.table(extract(peatl_map,
                                        #peatl_map_cat,
                                        gbif_data_all_sf, 
                                        sp = TRUE))
occs_peatl_map  
Sys.time() - t0


write.csv(occs_peatl_map, "occs_peatl_map.csv", row.names = FALSE, quote = FALSE)


unique(occs_peatl_map$EMB_peatl_int150m_WGS84)
sum(occs_peatl_map$EMB_peatl_int150m_WGS84, na.rm = TRUE)  # 3672825

occs_peatl_map <- occs_peatl_map[EMB_peatl_int150m_WGS84 == 1, ]
setkeyv(occs_peatl_map, "species")
occs_peatl_map  

sum(occs_peatl_map$species == "")   # 190884 with no species name

occs_peatl_map <- occs_peatl_map[!occs_peatl_map$species == "", ]
nrow(occs_peatl_map)  # 3481941

sort(unique(occs_peatl_map$year))
table(occs_peatl_map$year)
table(occs_peatl_map$species)


# Ranking of more common species
occs_peatl_map_species_rank <- as.data.table(table(occs_peatl_map$species))[order(N, decreasing = TRUE)]
occs_peatl_map_species_rank
head(occs_peatl_map_species_rank, 30)
View(occs_peatl_map_species_rank)



# Checking by country
table(occs_peatl_map[species == "Festuca rubra", countryCode])
table(occs_peatl_map[species == "Festuca rubra", countryCode], occs_peatl_map[species == "Festuca rubra", year])

table(occs_peatl_map[species == "Phragmites australis", countryCode])
table(occs_peatl_map[species == "Phragmites australis", countryCode], occs_peatl_map[species == "Phragmites australis", year])


# Entire list of species
occs_peatl_map_species <- occs_peatl_map[!duplicated(species), .SD, .SDcols = c("family", "genus", "species")]
setkeyv(occs_peatl_map_species, "species")
occs_peatl_map_species
nrow(occs_peatl_map_species)                 # 8111 species
sort(unique(occs_peatl_map_species$family))  # 409 families


# Sphagnum 
sphagnum_occs <- occs_peatl_map[grepl("Sphag", species), ]

length(unique(sphagnum_occs$species))  # 56 species
sphagnum_occs                          # 53311 occurrences of all species

sort(table(sphagnum_occs$species), decreasing = TRUE)  # species rarity (occs/species)

table(sphagnum_occs$species, sphagnum_occs$countryCode) # occs/species/country



## comparing with species found only in natural peatlands reported in the survey
sps_natural_only_occs  # from the survey (175)

occs_peatl_map_species$species   # from GBIF + peatlands map


sps_natural_only_occs_species <- sps_natural_only_occs$species
sps_natural_only_occs_species <- gsub("\\.", " ", sps_natural_only_occs_species)
sps_natural_only_occs_species <- gsub(" agg ", "", sps_natural_only_occs_species)
sps_natural_only_occs_species <- gsub("_t", "", sps_natural_only_occs_species)
sps_natural_only_occs_species <- gsub("_h", "", sps_natural_only_occs_species)


occs_peatl_map_species_species <- occs_peatl_map_species$species  # 8111 species
length(occs_peatl_map_species_species)

occs_peatl_map_species_species[occs_peatl_map_species_species %in% sps_natural_only_occs_species] # 135 species (out of 175 from the survey)








## Tanneberger vs CorineLC ####

## CLC at 100m
clc_100 <- raster::stack("/eos/jeodpp/data/base/Landcover/EUROPE/CorineLandCover/CLC2018/VER20-b2/Data/GeoTIFF/100m/clc2018_Version_20_b2.tif")
clc_100

## Peatlands map
peatl_map <- raster::raster(paste0("sepla/peatlands_map/tanneberger/", "EMB_peatl_int150m_WGS84.tif")) 
peatl_map

## Peatlands map centroids
# rasterToPoints calculates the centroid. See example: https://www.rdocumentation.org/packages/raster/versions/3.5-15/topics/rasterToPoints
peatl_map_pts <- rasterToPoints(peatl_map, fun = function(x){x == 1}, spatial = TRUE)
peatl_map_pts
head(peatl_map_pts)

# To LAEA
peatl_map_pts_laea <- spTransform(peatl_map_pts, CRS("+init=EPSG:3035"))
peatl_map_pts_laea


## Extracting values
peatl_map_CLC <- as.data.table(raster::extract(clc_100,
                                       peatl_map_pts_laea, 
                                       sp = TRUE))
peatl_map_CLC
unique(peatl_map_CLC$OID)
sort(unique(peatl_map_CLC$layer))
sum(is.na(peatl_map_CLC$layer))
sum(!is.na(peatl_map_CLC$layer))

sort(table(peatl_map_CLC$layer))

peatl_map_CLC_clean <- peatl_map_CLC[!is.na(layer), ]
peatl_map_CLC_clean

peatl_map_CLC_clean <- peatl_map_CLC_clean[layer != 999, ]
peatl_map_CLC_clean

sort(table(peatl_map_CLC_clean$layer))

sort(unique(peatl_map_CLC_clean$layer))


## barplot
peatl_map_CLC_clean$layer <- as.factor(peatl_map_CLC_clean$layer)

png("PeatlandsMap_CLC_allClasses.png", width = 20, height = 10, units = "cm", res = 150)
ggplot(peatl_map_CLC_clean, 
       aes(x = layer, fill = layer)) + 
  geom_bar(fill = viridis(length(unique(peatl_map_CLC_clean$layer)))) +
  #geom_histogram() +
  #coord_flip()
  theme(axis.text.x = element_text(angle = 90,
                                   #, size = 5
                                   vjust=-0.5
                                   )) +
  labs(#title = "MAIN TITLE", 
       x = "CLC class"#, 
       #y = "Y-AXIS TITLE"
       )
dev.off()


## barplot for 

