library(tidyverse)

getwd()
BOM_Data <- read_csv("Data/Raw_Data/BOM_data.csv")
BOM_Stations <- read_csv("Data/Raw_Data/BOM_stations.csv")

dim(BOM_Data)
glimpse(BOM_Data)


dim(BOM_Stations)
glimpse(BOM_Stations)
colnames(BOM_Stations)

#Question 1
#For each station, how many days have a minimum temperature,
#a maximum temperature and a rainfall measurement recorded?

separate(BOM_Data, Temp_min_max, into = c("T_Min", "T_Max"), sep = "/") -> BOM_Data 

BOM_Data %>% 
  mutate(T_Min = as.numeric(T_Min)) %>% 
  mutate(T_Max = as.numeric(T_Max)) %>% 
  mutate(Rainfall = as.numeric(Rainfall))-> BOM_Data

summary(BOM_Data)

BOM_Data %>% 
  group_by(Station_number) %>%
  summarise(MinDays = sum(!is.na(T_Min)),
            MaxDays = sum(!is.na(T_Max)),
            RainyDays = sum(!is.na(Rainfall)))
  
#Question 2
#Which month saw the lowest average daily temperature difference?

BOM_Data %>%
  mutate(Temp_Diff = T_Max - T_Min) -> BOM_Data %>% 
  group_by(Month) %>% 
  

#Question 3
#Which state saw the lowest average daily temperature difference?

#Question 4
#Does the westmost (lowest longitude) or eastmost (highest longitude)
#weather station in our dataset have a higher average solar exposure?