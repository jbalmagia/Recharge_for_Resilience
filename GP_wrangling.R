#Data Wrangling and Analysis for Model 

#Required packages 
library(tidyverse)
library(tmap)
library(sf)
library(spatstat)
library(maptools)
library(sp)
library(raster)
library(gstat)

########################################################################################
#Create a SAGBI index with no root-zone residence time consideration. 
#For when we don't want to consider on-farm recharge and adverse effects of recharge on crops isn't necessary

sagbi <- read_csv("SAGBI_no_rzrt.csv")

#Taking out Root Zone Residence Time category of SAGBI score (weighted as 27.5%) and proportionally redistributing that percentage across the remaining 4 categories.

#Figure out new weight of each category: 

#Deep Percolation (27.5%)
deep = ((27.5 * .275) + 27.5)/100
#Topographic limitations (20%)
topo = ((20 *.275) + 20)/100
#Chemical limitations (20%)
chem = ((20 * .275) + 20)/100 
#Surface conditions (5%)
surf = ((5 *.275) + 5)/100 

#Create new df with modified SAGBI score 

sagbi_no_rzrt <- sagbi %>% 
  rename(dp = deep_perc) %>% 
  mutate(sagbi_nocrop = dp*deep + toxicity*chem + surf_cond*surf + topo_rest*topo)

#Export new df to .txt to use in ArcMap. NOTE this code gives a specific path, will need to change when using a different computer with a different path
write.table(sagbi_no_rzrt, "C:\\Users\\jbalmagia\\Documents\\Bren_GP\\Data\\_Madera/sagbi_no_rzrt", sep="\t")


#############################################################################################
#GW Quality Interpolation
#This is to create raster surfaces from point data for all gw quality parameters of interest
####


#Prepping gw quality data for interpolation.

#read in data for all contaminants/chemistry 
as <- read.csv("water_quality/San Joaquin River_AS.csv")
cr6 <- read.csv("water_quality/San Joaquin River_CR6.csv")
do <- read.csv("water_quality/San Joaquin River_DO.csv")
fe <- read.csv("water_quality/San Joaquin River_FE.csv")
mg <- read.csv("water_quality/San Joaquin River_MG.csv")
no3 <- read.csv("water_quality/San Joaquin River_NO3N.csv")
ph <- read.csv("water_quality/San Joaquin River_PH.csv")
tcp <- read.csv("water_quality/San Joaquin River_TCPR123.csv")
tds <- read.csv("water_quality/San Joaquin River_TDS.csv")
u <- read.csv("water_quality/San Joaquin River_U.csv")

#Make sure R knows these are spatial data

as_sf <- st_as_sf(as, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
cr6_sf <- st_as_sf(cr6, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
do_sf <- st_as_sf(do, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
fe_sf <- st_as_sf(fe, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
mg_sf <- st_as_sf(mg, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
no3_sf <- st_as_sf(no3, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
ph_sf <- st_as_sf(ph, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
tcp_sf <- st_as_sf(tcp, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
tds_sf <- st_as_sf(tds, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
u_sf <- st_as_sf(u, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

#Read in Madera Sub-basin boundary
madera <- read_sf(dsn = 'Madera_Basin_Boundary', layer = "Madera_gw_basin")
st_crs(madera) = 4326

#Check out Madera boundary - did I do it right??
tm_shape(madera)+
  tm_fill()+
  tm_shape(as_sf)+
  tm_dots("RESULTS", size = 0.5)+
  tm_shape(cr6_sf)+
  tm_dots("RESULTS", size = 0.5)
#Works!!

#Make GW quality parameters each a Spatial Points data frame

as_sp <- as_Spatial(as_sf)
cr6_sp <- as_Spatial(cr6_sf)
do_sp <- as_Spatial(do_sf)
fe_sp <- as_Spatial(fe_sf)
mg_sp <- as_Spatial(mg_sf)
no3_sp <- as_Spatial(no3_sf)
ph_sp <- as_Spatial(ph_sf)
tcp_sp <- as_Spatial(tcp_sf)
tds_sp <- as_Spatial(tds_sf)
u_sp <- as_Spatial(u_sf)
