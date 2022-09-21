
if(Sys.info()[4] == "D01RI1700308") {
  wd <- ""
}else if(Sys.info()[4] == "S-JRCIPRAP320P") {
  wd <- ""
}else if(Sys.info()[4] %in% c("jeodpp-terminal-jd001-03", "jeodpp-terminal-03")) {
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
library(PreSPickR)
library(ggplot2)
library(viridis)




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


summary(sps_natural_only_occs$num_points)
#   Min.   1st Qu.  Median    Mean    3rd Qu.    Max. 
#    1.0     1.0     2.0        4.2     5.0    27.0 

quantile(sps_natural_only_occs$num_points, seq(0, 1, 0.1))
#  0%   10%  20%  30%  40%  50%  60%  70%  80%  90%    100% 
# 1.0   1.0  1.0  1.0  1.0  2.0  3.0  4.0  6.2  11.0   27.0 

sort(unique(sps_natural_only_occs$num_points))


## occurrences of all Sphagnus species
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











