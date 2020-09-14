########################################
############## PROCESSING ################
########################################

library(nomisr)
library(tidyverse)

##################


# x <- as.data.frame(nomis_data_info())
# y <- nomis_data_info("NM_162_1")
# 
# a <- nomis_get_metadata("NM_162_1")
# b <- nomis_get_metadata("NM_162_1", concept = "GEOGRAPHY", type = "type")
# b01 <- nomis_get_metadata("NM_162_1", concept = "GEOGRAPHY", type = "TYPE480")
# 
# b1 <- nomis_get_metadata("NM_162_1", concept = "GEOGRAPHY")
# c <- nomis_get_metadata("NM_162_1", concept = "MEASURE")
# d <- nomis_get_metadata("NM_162_1", concept = "FREQ")
# e <- nomis_get_metadata("NM_162_1", concept = "MEASURES")
# f <- nomis_get_metadata("NM_162_1", concept = "AGE")

####################################

################# Import and process old CC ##########################

old_raw <- nomis_get_data("NM_162_1",
                          date = "2019-07",
                          #LSOAs and Scottish datazones
                          geography = c("TYPE298", "TYPE232"),
                          MEASURE = 1,
                          age = 0,
                          sex = 0,
                          measures = 20100)

old <- old_raw %>% rename(area = "GEOGRAPHY_CODE",
                          claimant_count_old = "OBS_VALUE")

old <- old %>% select(area,
                      claimant_count_old)

########################## Import and process new CC ###################################

new_raw <- nomis_get_data("NM_162_1",
                          date = "2020-07",
                          geography = c("TYPE298", "TYPE232"),
                          MEASURE = 1,
                          age = 0,
                          sex = 0,
                          measures = 20100)

new <- new_raw %>% rename(area = "GEOGRAPHY_CODE",
                          claimant_count_new = "OBS_VALUE")

new <- new %>% select(area,
                      claimant_count_new)

##############################################

# y1 <- nomis_data_info("NM_2010_1")
# b1 <- nomis_get_metadata("NM_2010_1", concept = "GEOGRAPHY", type = "type")
# e1 <- nomis_get_metadata("NM_2010_1", concept = "MEASURES")
# f1 <- nomis_get_metadata("NM_2010_1", concept = "C_AGE")
# c1 <- nomis_get_metadata("NM_2010_1", concept = "MEASURE")
# h1 <- nomis_get_metadata("NM_2010_1", concept = "GENDER")

############################ Import and process England and Wales populations ########################



pop_eng_wales <- nomis_get_data("NM_2010_1",
                                time =  "latest",
                                geography = c("TYPE298"),
                                measures = 20100,
                                c_age = 203,
                                gender = 0)

pop_eng_wales <- pop_eng_wales %>% rename(area = "GEOGRAPHY_CODE",
                                          population = "OBS_VALUE")

pop_eng_wales <- pop_eng_wales %>% select(area, population)

######################### Import and process Scotland populations   #####################

pop_scot <- read_csv(file = "IZ population estimates.csv")

pop_scot <- pop_scot %>% filter(pop_scot$Sex == "All")
pop_scot <- pop_scot %>% filter(pop_scot$Year == "2018")

pop_scot <- pop_scot %>% rename(area = "IntZone")

pop_scot <- pop_scot %>% select(area, population)


############################# Join populations together ######################################

population <- rbind(pop_eng_wales, pop_scot)


######################## Create one dataset ###############################################

cc <- merge(old, new, by = "area")

cc <- merge(cc, population, by = "area")

########################### Calculate rates ########################################

cc$old_rate <- (cc$claimant_count_old/cc$population)*100

cc$new_rate <- (cc$claimant_count_new/cc$population)*100


cc$`Claimant count rate ppt change (July 2017 - July 2020)` <- cc$new_rate - cc$old_rate

cc$old_rate <- round(cc$old_rate,1)
cc$new_rate <- round(cc$new_rate,1)
cc$`Claimant count rate ppt change (July 2017 - July 2020)` <- round(cc$`Claimant count rate ppt change (July 2017 - July 2020)`,1)

#make nice names for the columns
cc <- cc %>% rename("msoa11cd" = area, 
                    "Claimant count (July 2017)" = claimant_count_old, 
                    "Claimant count (July 2020)" = claimant_count_new, 
                    "Population 2018" = population, 
                    "Claimant count rate % (July 2017)" = old_rate, 
                    "Claimant count rate % (July 2020)" = new_rate)

#format the data
cc$`Claimant count (July 2017)` <- format(cc$`Claimant count (July 2017)`, big.mark = ",")
cc$`Claimant count (July 2020)` <- format(cc$`Claimant count (July 2020)`, big.mark = ",")
cc$`Population 2018` <- format(cc$`Population 2018`, big.mark = ",")

######################################
############## MAP ###################
#####################################

library(rgdal)
library(rgeos)
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggiraph))
suppressPackageStartupMessages(library(geojsonio))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))

lsoabounds <- geojson_sf("https://opendata.arcgis.com/datasets/f213065139e3441195803b4155e71e00_0.geojson")
lsoabounds <- rmapshaper::ms_simplify(lsoabounds, keep = 0.05)
