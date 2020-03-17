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

#converting various columns to numeric to force NA's
BOM_Data %>%
  mutate(T_Min = as.numeric(T_Min)) %>% 
  mutate(T_Max = as.numeric(T_Max)) %>% 
  mutate(Rainfall = as.numeric(Rainfall)) %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure)) -> BOM_Data

#This gives which stations which have data for each column individually
BOM_Data %>% 
  group_by(Station_number) %>%
  summarise(MinDays = sum(!is.na(T_Min)),
            MaxDays = sum(!is.na(T_Max)),
            RainyDays = sum(!is.na(Rainfall)))

#This gives which stations have all three measurements
#Uncomment out the summarise function will create a table with all three
#But comment out the last summarise if above line is done!
BOM_Data %>% 
  group_by(Station_number) %>%
  filter(!is.na(T_Min)) %>% 
  filter(!is.na(T_Max)) %>% 
  filter(!is.na(Rainfall)) %>% 
  #summarise(MinDays = sum(!is.na(T_Min)),
  #          MaxDays = sum(!is.na(T_Max)),
  #          RainyDays = sum(!is.na(Rainfall)))
  summarise(n = n())
        

#Question 2
#Which month saw the lowest average daily temperature difference?

BOM_Data %>% 
  mutate(Temp_Diff = T_Max - T_Min) %>% 
  group_by(Month) %>%
  summarise(Ave_Month_Temp_Diff = mean(Temp_Diff, na.rm = TRUE)) %>% 
  arrange(Ave_Month_Temp_Diff)

#Question 3
#Which state saw the lowest average daily temperature difference?

#gather and spread to get this into a decent format to work with
BOM_Stations %>% 
  gather(key = key, value = value, 2:21) %>% 
  rename(Station_number = key) %>% 
  spread(BOM_Stations, key = info, value = value) -> BOM_Stations

glimpse(BOM_Stations)
glimpse(BOM_Data)

#converting to same format between both objects
BOM_Stations %>%
  mutate(Station_number = as.numeric(Station_number)) %>% 
  mutate(elev = as.numeric(elev)) %>%
  mutate(end = as.numeric(end)) %>%
  mutate(lat = as.numeric(lat)) %>% 
  mutate(lon = as.numeric(lon)) %>% 
  mutate(name = as.character(name)) %>% 
  mutate(start = as.numeric(start)) %>%
  mutate(state = as.character(state)) -> BOM_Stations

#joining both objects
full_join(BOM_Data, BOM_Stations) -> BOM_Combined

#finally answering the question!
BOM_Combined %>%
  mutate(Temp_Diff = T_Max - T_Min) %>% 
  group_by(state) %>%
  summarise(Ave_Temp_Diff = mean(Temp_Diff, na.rm = TRUE)) %>% 
  arrange(Ave_Temp_Diff)

#Question 4
#Does the westmost (lowest longitude) or eastmost (highest longitude)
#weather station in our dataset have a higher average solar exposure?

#This answers question, but returns NaN for station 86344
BOM_Combined %>%
  group_by(Station_number) %>%
  summarise(mean_lon = mean(lon, na.rm = TRUE),
            ave_sol_exp = mean(Solar_exposure, na.rm = TRUE)) %>% 
  arrange(mean_lon)


#NaN for station 86344 is because there is no Solar_exposure data for this station
BOM_Combined %>% 
  filter(Station_number == 86344 & Solar_exposure != NA) 
  