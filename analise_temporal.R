# ------------------------------------------------------------
# Importando as bibliotecas necessárias
# ------------------------------------------------------------
library(dplyr)
library(stringr)
library(purrr)
library(ggplot2)
library(glue)
library(forcats)
library(tidyr)

# Carregando a base de dados
loaded_objs <- load("bfd_2022.rdata")
df <- get(loaded_objs[1]) 

# Lista das variáveis depart/arrival para as informações climaticas ou meteorológicas
# que serão verificadas quanto a NAs e contagem de aeroportos
vars_to_check <- c(
  "depart_air_temperature",   #degrees Celsius 
  "depart_dew_point",         #degrees Celsius
  "depart_relative_humidity", #Percentage of relative humidity
  "depart_wind_direction", #degree, based on Wind Rose
  "depart_wind_speed",        #in knots,
  "depart_sky_coverage", #cat
  "depart_pressure", # Atmospheric pressure         
  "depart_visibility", #in miles
  "depart_apparent_temperature", #in degrees 
  "depart_wind_speed_scale",  #cat
  "depart_wind_direction_cat", #cat
  
  "arrival_air_temperature",  
  "arrival_dew_point",
  "arrival_relative_humidity",
  "arrival_wind_direction",
  "arrival_wind_speed",       
  "arrival_sky_coverage",
  "arrival_pressure",         
  "arrival_visibility",
  "arrival_apparent_temperature",
  "arrival_wind_speed_scale", 
  "arrival_wind_direction_cat"
)
df_clean <- df %>%
  drop_na(all_of(vars_to_check))
