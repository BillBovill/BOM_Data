library(tidyverse)

getwd()
BOM_Data <- read_csv("Data/Raw_Data/BOM_data.csv")
BOM_Stations <- read_csv("Data/Raw_Data/BOM_stations.csv")

dim(BOM_Data)
glimpse(BOM_Data)

dim(BOM_Stations)
glimpse(BOM_Stations)

