#############################
#                           #
#   MEDIAN RENT DATA PREP   #         
#                           #
#############################

library(sf)
library(dplyr)
library(stringr)
options(stringsAsFactors = FALSE, scipen = 9999)
setwd("Z:\\Work\\Coop\\Fall_2019\\median-rent\\data")

prep_rent_data <- function(file_name = "FY2018_50_County_rev.csv") {
  
  # https://www.huduser.gov/portal/datasets/50per.html#2018
  rent_data <- read.csv(paste0("raw\\", file_name))
  
  # clean up rent_data
  colnames(rent_data) <- tolower(colnames(rent_data))
  to_keep <- c("rent50_0", "rent50_1", "rent50_2", "rent50_3", "rent50_4", "countyname", "pop2010", "state_alpha")
  rent_data <- rent_data[, colnames(rent_data) %in% to_keep]
  colnames(rent_data)[colnames(rent_data)=="state_alpha"] <- "state"

  
  # restrict data to western states
  rent_data <- rent_data[rent_data$state %in% c("CA", "OR", "WA", "MT", "ID", "WY", "CO", "NV", "TX", "OK", "AZ", "NM", "UT"),]
  rent_data$countyname <- str_remove(rent_data$countyname, " County")
  colnames(rent_data)[colnames(rent_data)=="countyname"] <- "name"
  
  county_data <- readRDS("county\\counties_western_4326.RDS")
  st_geometry(county_data) <- NULL
  
  # give rent data county ID which matches object ID in county
  rent_data <- left_join(rent_data, county_data[, c("CountyID", "NAME", "Code")], by = c("name" = "NAME", "state" = "Code"))
  
  # make sure that all the rent data got a CountyID
  no_id <- nrow(rent_data[is.na(rent_data$CountyID), ])
  if (no_id > 0) {
    stop(paste(no_id, "counties did not recieve an ID", paste(rent_data[is.na(rent_data$CountyID), "name"], collapse = ", ")))
  } else {
    print("All counties successfully assigned an ID")
  }

  write.csv(rent_data, paste0("rent\\", file_name), row.names = FALSE)
 
}

prep_county_data <- function(file_name = "UNITEDSTATES_County_2018_ESRI_R2019.shp") {
  
  county_data <- st_read(paste0("raw\\", file_name))
  
  # clean up county data
  to_drop <- c("STATE_FIPS", "CNTY_FIPS", "FIPS")
  county_data <- county_data[, !(colnames(county_data) %in% to_drop)]
  colnames(county_data)[colnames(county_data)=="OBJECTID"] <- "CountyID"
  
  # fix dona ana county because special symbol messes up name
  county_data[county_data$NAME == "Doña Ana", "NAME"] <- "Dona Ana"
  
  # add state_abbreviations
  abbrs <- read.csv(paste0("raw\\state_abbr_map.csv"))
  county_data <- left_join(county_data, abbrs[, c("State", "Code")], by = c("STATE_NAME" = "State"))
  
  county_data <- st_transform(county_data, 4326)
  
  st_write(county_data, "county\\counties_western_4326.shp")
  saveRDS(county_data, "county\\counties_western_4326.RDS")
  
}




