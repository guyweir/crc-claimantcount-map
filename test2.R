########################################
############## PROCESSING ################
########################################

library(nomisr)
library(tidyverse)
library(reactable)
library(crosstalk)

##################


# x <- as.data.frame(nomis_data_info())
# y <- nomis_data_info("NM_162_1")
# 
# a <- nomis_get_metadata("NM_162_1")
# b <- nomis_get_metadata("NM_162_1", concept = "GEOGRAPHY", type = "type")
# b01 <- nomis_get_metadata("NM_162_1", concept = "GEOGRAPHY", type = "TYPE298")
# 
# b1 <- nomis_get_metadata("NM_162_1", concept = "GEOGRAPHY")
# c <- nomis_get_metadata("NM_162_1", concept = "MEASURE")
# d <- nomis_get_metadata("NM_162_1", concept = "FREQ")
# e <- nomis_get_metadata("NM_162_1", concept = "MEASURES")
# f <- nomis_get_metadata("NM_162_1", concept = "AGE")

####################################

################# Import and process old CC ##########################

old_raw <- nomis_get_data("NM_162_1",
                          date = "2019-08",
                          #LSOAs and Scottish datazones
                          geography = c("TYPE298", "TYPE231"),
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
                          date = "2020-08",
                          geography = c("TYPE298", "TYPE231"),
                          MEASURE = 1,
                          age = 0,
                          sex = 0,
                          measures = 20100)

new <- new_raw %>% rename(area = "GEOGRAPHY_CODE",
                          claimant_count_new = "OBS_VALUE")

new <- new %>% select(area,
                      claimant_count_new)

pop_eng_wales <- nomis_get_data("NM_2010_1",
                                time =  "latest",
                                geography = c("TYPE298"),
                                measures = 20100,
                                c_age = 203,
                                gender = 0)

pop_eng_wales <- pop_eng_wales %>% rename(area = "GEOGRAPHY_CODE",
                                          population = "OBS_VALUE")

pop_eng_wales <- pop_eng_wales %>% select(area, population)

# ######################### Import and process Scotland populations   #####################
# 
# pop_scot <- read_csv("population-estimates-current-geographic-boundaries.csv", skip = 8)
# pop_scot$lsoa11cd <- str_remove_all(pop_scot$`http://purl.org/linked-data/sdmx/2009/dimension#refArea`,"http://statistics.gov.scot/id/statistical-geography/")
# pop_scot <- pop_scot[,c(4,2,3)]
# pop_scot <- rename(pop_scot,"population" = "Count", "area" = "lsoa11cd")

############################# Join populations together ######################################

# population <- bind_rows(pop_eng_wales, pop_scot)
population <- pop_eng_wales

######################## Create one dataset ###############################################

cc <- merge(old, new, by = "area")

cc <- merge(cc, population, by = "area", all.x = T)

########################### Calculate rates ########################################

cc$old_rate <- (cc$claimant_count_old/cc$population)*100

cc$new_rate <- (cc$claimant_count_new/cc$population)*100


cc$`Claimant count rate ppt change (August 2019 - August 2020)` <- cc$new_rate - cc$old_rate

cc$old_rate <- round(cc$old_rate,1)
cc$new_rate <- round(cc$new_rate,1)
cc$`Claimant count rate ppt change (August 2019 - August 2020)` <- round(cc$`Claimant count rate ppt change (August 2019 - August 2020)`,1)

cc$"Change decile (1 = low)" <- ntile(cc$claimant_count_old,10)
cc$"Change decile (1 = low)" <- as.factor(cc$"Change decile (1 = low)")

#make nice names for the columns
cc <- cc %>% rename("lsoa11cd" = area, 
                    "Claimant count (August 2019)" = claimant_count_old, 
                    "Claimant count (August 2020)" = claimant_count_new, 
                    "Population 2018" = population, 
                    "Claimant count rate % (August 2019)" = old_rate, 
                    "Claimant count rate % (August 2020)" = new_rate
)

#format the data
cc$`Claimant count (August 2019)` <- format(cc$`Claimant count (August 2019)`, big.mark = ",")
cc$`Claimant count (August 2020)` <- format(cc$`Claimant count (August 2020)`, big.mark = ",")
cc$`Population 2018` <- format(cc$`Population 2018`, big.mark = ",")

#lookups
# scotlook <- read_csv("Datazone2011lookup.csv")
# scotlook <- scotlook %>% select(DZ2011_Code,DZ2011_Name, LA_Name)
# scotlook <- scotlook[!duplicated(scotlook),]
# scotlook <- scotlook %>% rename("LSOA11CD" = DZ2011_Code,"LSOA11NM" = DZ2011_Name,"LAD17NM" = LA_Name )
# scotlook$RGN11NM <- "Scotland"

EWlook <- read_csv("os lsoa msoa lookup.csv")
EWScotlook <- EWlook %>% select(LSOA11CD, LSOA11NM,LAD17NM, RGN11NM)
EWScotlook <- EWScotlook[!duplicated(EWScotlook),]
# EWScotlook <- bind_rows(EWlook, scotlook)
EWScotlook <- rename(EWScotlook,"Local Authority" = LAD17NM, "Region/Country" = RGN11NM, "Neighbourhood name" = LSOA11NM)

cc <- merge(cc, EWScotlook, by.x ="lsoa11cd", by.y = "LSOA11CD" )
cc <- select(cc, -"Reference Area")

###########################################
################### TABLE ################
###########################################



table <- cc # %>% select(-lsoa11cd)
table <- table[,c(1,9,10,11,2,3,5,6,7,8)]

rm(cc,EWScotlook, EWlook, new, old, pop_eng_wales, population)

#table2 <- SharedData$new(table)


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

lsoa.centroids <- geojson_sf("https://opendata.arcgis.com/datasets/b7c49538f0464f748dd7137247bbc41c_0.geojson")
#add in the IZ centroids
#dzcentroids <- read_sf("http://sedsh127.sedsh.gov.uk/arcgis/rest/services/ScotGov/StatisticalUnits/MapServer/4/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryMultipoint&inSR=&spatialRel=esriSpatialRelWithin&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson")


# dzcentroids <- dzcentroids %>% rename(objectid = OBJECTID, lsoa11cd = DataZone, lsoa11nm = Name)
# dzcentroids <- dzcentroids[,c(1,2,3,9)]
#dzcentroids <- as(dzcentroids, "Spatial")

#EWS.centroids <- raster::union(lsoa.centroids, dzcentroids)

#EWS.centroids <- rbind(dzcentroids, lsoa.centroids)
EWS.centroids <- lsoa.centroids
rm(lsoa.centroids)
#merge in the cc data
#EWS.centroids <- st_as_sf(lsoa.centroids)

EWS.centroids.df <- merge(EWS.centroids,table, by.x = "lsoa11cd",by.y = "lsoa11cd", all.x = T)
EWS.centroids.df <- EWS.centroids.df[,c(colnames(table),"geometry")]

EWS.centroids.df$ccradius <- (EWS.centroids.df$`Claimant count rate ppt change (August 2019 - August 2020)`)* 0.2 + 2
EWS.centroids.df$"Change decile (1 = low)" <- as.factor(EWS.centroids.df$"Change decile (1 = low)")
EWS.centroids.df <- EWS.centroids.df[,c(1:10,12,13, 11)] #stick geometry on the end of the file
#EWS.centroids.df <- EWS.centroids.df[c(1:14,16,15)]

factpal <- colorFactor("RdBu",levels = levels(EWS.centroids.df$`Change decile (1 = low)`[!is.na(EWS.centroids.df$`Change decile (1 = low)`)]), 
                       ordered = TRUE, reverse = T )


labels <- sprintf("<strong>%s</strong><br/>%g ppt claimant count change<sup></sup>",
                  EWS.centroids.df$`Neighbourhood name`, round(EWS.centroids.df$`Claimant count rate ppt change (August 2019 - August 2020)`,1)) %>% 
  lapply(htmltools::HTML)





#function for adding circle sizes to the legend
addLegendCustom <- function(title,map, colors, labels, sizes, opacity = 0.5, position){
  
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:",
                           sizes, "px", "; position: relative; left: ",max(sizes)-(sizes/2)-12,"px")
  
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes,";position:relative; left: ",max(sizes)-(sizes),"px","; bottom: ",
                           10,"px",";margin-top: 12px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, title = title,
                   labels = labelAdditions, opacity = opacity, position = position))
}

#add title to page
library(htmltools)

#page element title
title <- tags$div(HTML("Claimant count percentage point change,<br> August 2019 to August 2020, Great Britain</br>"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 22px; text-align: center"
)

#page element data sources
sources <- tags$div(HTML("Claimant Count, ONS<br> Analysis: WPI Economics on behalf of CRC"), 
                    style = "font-family: Open Sans;color: #2A2A2A;font-style: italic; font-size: 12px; text-align: left"
)

#remove NA
#EWS.centroids.df <- EWS.centroids.df[!is.na(EWS.centroids.df$`Change decile (1 = low)`),]
#EWS.centroids.df <- st_as_sf(EWS.centroids.df)
#EWS.centroids.df <- as_Spatial(EWS.centroids.df)

#reorder to match table

EWS.centroids.dfXT <- SharedData$new(EWS.centroids.df)

#map element
#map element
m2 <- leaflet(EWS.centroids.dfXT, height = "580px", options = list(padding = 100)) %>% setView(-3.5,53.2, 5.5) %>% 
  setMapWidgetStyle(list(background = "white")) %>% addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 1) ) %>% 
  
  
  addCircleMarkers( group = "circlegw",
                    radius = ~`ccradius`,
                    stroke = F,
                    color = ~factpal(`Change decile (1 = low)`), opacity = 0.85, fillOpacity = 0.85,
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))   %>% 
  
  addLegendCustom(colors = c("grey", "grey", "grey"),
                  labels = c("-3ppts","+5ppts","+15ppts"),
                  
                  sizes = c(1.4,3,5)*2, position = "bottomright", title = "CC change 2019-2020<br> &nbsp" ) %>%
  
  addLegend(pal = factpal, values = EWS.centroids.df$`Change decile (1 = low)`,
            labels = levels(EWS.centroids.df$`Change decile (1 = low)`), position = "bottomright", title = "CC 2017 deciles <br>(1 = low)") %>%
  
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton() 
m2



sd <- EWS.centroids.df %>% as.data.frame() %>% select(-geometry, -ccradius) 
sd_df <- SharedData$new(sd)


##########the reactable table



tbl <- reactable(sd_df, selection = "multiple",
                 onClick = "select",
                 rowStyle = list(cursor = "pointer"),
                 minRows = 10,filterable = F,searchable = F, wrap = T , defaultPageSize = 15, striped = T, highlight = T,
                 defaultSorted = list("Claimant count rate ppt change (August 2019 - August 2020)" = "desc"),
                 columns = list(`Change decile (1 = low)` = colDef(filterable = T),
                                `Local Authority` = colDef(filterable = T),
                                `Region/Country` = colDef(filterable = T)),
                 #`COVID-19 deaths per 100,000` = colDef(aggregate = "mean",format = colFormat(digits = 0)),
                 #`COVID-19 deaths age adjusted per 100,000` = colDef(aggregate = "mean",format = colFormat(digits = 0))),
                 theme = reactableTheme(
                   stripedColor = "#faf8f1",
                   highlightColor = "#e5dec4",
                   cellPadding = "6px 10px",
                   style = list(fontFamily = "Arial", fontSize = "12px"),
                   #searchInputStyle = list(width = "100%", fontWeight = "400"),
                   headerStyle = list(color = "white",background = "#2A2A2A",
                                      "&:hover[aria-sort]" = list(background = "#8c8c8c "),
                                      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "#8c8c8c"),
                                      borderColor = "#555"
                   )
                 )) 
tbl

combo <- htmltools::tagList(m2, tbl,sources) #I think this makes a combined html object
browsable(combo)
