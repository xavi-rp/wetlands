
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



## loading data from the survey ####
data_survey <- fread(paste0(wd, "/sepla/data_fen-rewetting_denaturated0.1.csv"), header = TRUE)

data_survey
nrow(data_survey)
ncol(data_survey)
names(data_survey)

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








