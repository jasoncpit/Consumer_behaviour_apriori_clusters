#Association rules minings... 
#Import relevant packages-------------
library(dplyr)
library(tidyverse)
library(arulesSequences)
library(sf)
library(tmap)
library(rgdal)
library(ggplot2)
library(plyr)
library(XML)
library(dplyr)
library("entropy")
library(tidyr)
library(rgeos)
library(readr)
#Import standard transaction data -> convert date-time obejet -------------
library("ISOweek")
Inhome_data = data.frame(Inhome_data)
Inhome_data$date = ISOweek2date(paste0(substr(Inhome_data$WEEK,1,4),"-W",substr(Inhome_data$WEEK,5,6),"-",Inhome_data$DAY.OF.PURCHASE))

Ouhome_data = data.frame(Ouhome_data)
Ouhome_data$date = ISOweek2date(paste0(substr(Ouhome_data$week,1,4),"-W",substr(Ouhome_data$week,5,6),"-",Ouhome_data$DAY))

# Start time of data to be considered
start_month <- "2018-01-01"

#Pre-agreating data -------------

#Aggreating product based on categories 
library(stringr)

Inhome_attr_cat = data.frame(product = Inhome_attr$PRODUCT, product_category  = Inhome_attr$Category)
Inhome_attr_cat$product_category = str_replace_all(string=Inhome_attr_cat$product_category, pattern=" ", repl="_")

Outhome_attr_cat = Outhome_attr %>% data.frame() %>% mutate(product_category = case_when(
  #No NA 
  Category == "" & 
    Total.FOTG.Sector == "Coffee"&
    Manufacturer == "No Brand Name" ~ "No_Brand_take_away_Coffee", 
  Category == "" & 
    Total.FOTG.Sector == "Coffee"&
    Manufacturer != "No Brand Name" ~ "Branded_take_away_Coffee",
  Category == "" & 
    Total.FOTG.Sector == "Tea"&
    Manufacturer != "No Brand Name" ~ "Branded_take_away_Tea", 
  Category == "" & 
    Total.FOTG.Sector == "Tea"&
    Manufacturer == "No Brand Name" ~ "No_Brand_take_away_Tea",
  Category == "" & 
    Total.FOTG.Sector == "Other Hot Drinks" ~ "Other Hot Drinks",
  TRUE ~ as.character(Category )
)) %>% select(PRODUCT,product_category)
Outhome_attr_cat$product_category = str_replace_all(string=Outhome_attr_cat$product_category, pattern=" ", repl="_")

#Aggreating stores based on Lake et al (2010)
#Inhome_store_cat = data.frame(shopcode = Inhome_store$SHOP_CODE,shop_category = Inhome_store$SHOP_DESC)
#Inhome_store_cat$shop_category = str_replace_all(string=Inhome_store_cat$shop_category, pattern=" ", repl="_")
#Outhome_store_cat =  data.frame(shopcode = Outhome_store$SHOP_CODE,shop_category = Outhome_store$SHOP_DESC)
#Outhome_store_cat$shop_category = str_replace_all(string=Outhome_store_cat$shop_category, pattern=" ", repl="_")
#0 Unknown
#1 Restaurant
#2 Pub/Bar
#3 Convenience
#4 Supermarket
#5 Takeaway Food
#6 Work Place/Education
#7 Hotels/Function Rooms/Associations
#8 Medical e.g. Pharmacy
#9 Entertainment e.g. cinema, bowling, theatre, sports venues
#10 Department Stores i.e. large retail store organised into departments offering variety of merchandise.
#11 Discount Stores
#12 Fast Food
#13 Forecourts
#14 Non-Food Stores/Novelty Items e.g. clothes/accessory shops, gift shops, stationery shops, cosmetic/toiletry shops.
#15 Food Production Services e.g. wholesalers, suppliers, distributers, caterers, cash & carry
#16 Sandwich Shop
#17 Café/Coffee Shop
#18 Specialist e.g. organic food stores, holistic food stores, fair trade stores, oriental food stores
#19 Specialist Traditional e.g. Delicatessen, Butcher, Baker, Fishmonger, Confectioners, Greengrocer
#20 Baker-Retail Freshly baked savouries/bread, pre-made sandwiches, baked sweet products & branded products. Usually a chain, takeaway only.
#21 Health and Leisure e.g. Gyms, Health Clubs, Leisure Centre
#22 market  
#23Retail Chains restaurants/caf
#24 online
#25 Ice-cream?
library(readr)
Lake_store_cat =  read_csv("~/Desktop/Inhome_store_lake.csv")
Store_cat = data.frame(shopcode = Lake_store_cat$SHOP_CODE,shop_desc = Lake_store_cat$SHOP_DESC,food_env_cat= Lake_store_cat$Food_env_cat)
Store_cat = Store_cat %>% mutate(food_env_cat_name = case_when(
  food_env_cat == 0 ~ "Unknown",
  food_env_cat == 1 ~ "Restaurant",
  food_env_cat == 2 ~ "Pub/Bar",
  food_env_cat == 3 ~ "Convenience",
  food_env_cat == 4 ~ "Supermarket",
  food_env_cat == 5 ~ "Takeaway Food",
  food_env_cat == 6 ~ "Work Place/Education",
  food_env_cat == 7 ~ "Hotels/Function Rooms/Associations",
  food_env_cat == 8 ~ "Medical",
  food_env_cat == 9 ~ "Entertainment",
  food_env_cat == 10 ~ "Department Store",
  food_env_cat == 11 ~ "Discount Stores",
  food_env_cat == 12 ~ "Fast Food",
  food_env_cat == 13 ~ "Forecourts",
  food_env_cat == 14 ~ "Non-Food Stores",
  food_env_cat == 15 ~ "Food Production Services",
  food_env_cat == 16 ~ "Sandwich Shop",
  food_env_cat == 17 ~ "Café/Coffee Shop",
  food_env_cat == 18 ~ "Specialist",
  food_env_cat == 19 ~ "Specialist Traditional",
  food_env_cat == 20 ~ "Baker-Retail",
  food_env_cat == 21 ~ "Health and Leisure",
  food_env_cat == 22 ~ "market",
  food_env_cat == 23 ~ "Retail Chains restaurants/cafe",
  food_env_cat == 24 ~ "online",
  
  TRUE ~ as.character("Others ")
))
Store_cat$food_env_cat_name = str_replace_all(string=Store_cat$food_env_cat_name, pattern=" ", repl="_")

#Create list of consumed products and shop categories by customer ID and date (formatted dates)-------------
colnames(Inhome_data)[5] = "households"
Inhome_data = merge(Inhome_data,Inhome_attr_cat,by.x= "PRODUCT.CODE", by.y = "product")
Inhome_data = merge(Inhome_data,Store_cat,by.x= "SUPERPANEL.SHOPCODE", by.y = "shopcode")
trans_sequence_product_inhome <- Inhome_data %>%
  group_by(households, date) %>%
  dplyr::summarise(
    SIZE = n(),
    ServiceLevel = paste(as.character(product_category), collapse = ';')
  )

trans_sequence_store_inhome <- Inhome_data %>%
  group_by(households, date) %>%
  dplyr::summarise(
    SIZE = n(),
    ServiceLevel = paste(as.character(food_env_cat_name), collapse = ';')
  )

Ouhome_data$household_ind = paste0(Ouhome_data$house,Ouhome_data$individ)
Ouhome_data = merge(Ouhome_data,Outhome_attr_cat,by.x= "Product", by.y = "PRODUCT")
Ouhome_data = merge(Ouhome_data,Store_cat,by.x= "shopcode", by.y = "shopcode")

trans_sequence_product_outhome <- Ouhome_data %>%
  group_by(house, date) %>%
  dplyr::summarise(
    SIZE = n(),
    ServiceLevel = paste(as.character(product_category), collapse = ';')
  )


trans_sequence_store_outhome <- Ouhome_data %>%
  group_by(house,date) %>%
  dplyr::summarise(
    SIZE = n(),
    ServiceLevel = paste(as.character(food_env_cat_name), collapse = ';')
  )


#Preparing data for association rules mining ------------
trans_sequence_inhome_all = merge(trans_sequence_store_inhome,trans_sequence_product_inhome,by =  c("households","date","SIZE"))
trans_sequence_inhome_all$all_transaction  = str_c(trans_sequence_inhome_all$ServiceLevel.x,";",trans_sequence_inhome_all$ServiceLevel.y)
trans_sequence_outhome_all = merge(trans_sequence_store_outhome,trans_sequence_product_outhome,by =  c("house","date","SIZE"))
trans_sequence_outhome_all$all_transaction  = str_c(trans_sequence_outhome_all$ServiceLevel.x,";",trans_sequence_outhome_all$ServiceLevel.y)

#trans_sequence_inhome_all = data.frame(households = Inhome_data$households,transactions = paste0(Inhome_data$product_category,",",Inhome_data$food_env_cat_name))
#Reordering 
trans_sequence_inhome_all <- trans_sequence_inhome_all[order(trans_sequence_inhome_all$households, trans_sequence_inhome_all$date),]
trans_sequence_outhome_all <- trans_sequence_outhome_all[order(trans_sequence_outhome_all$house, trans_sequence_outhome_all$date),]
colnames(trans_sequence_outhome_all)[1]  = "households"
#Standardising 
trans_sequence_inhome_all$all_transaction = str_replace_all(string=trans_sequence_inhome_all$all_transaction, pattern=";", repl=",")
trans_sequence_outhome_all$all_transaction = str_replace_all(string=trans_sequence_outhome_all$all_transaction, pattern=";", repl=",")

#Reading transactions
library(readr)
setwd("~/Desktop/Sequential data mining/Code/Transaction sequence")
trans_sequence_outhome_all <- read_csv("trans_sequence_outhome_all.csv")
c <- read_csv("trans_sequence_inhome_all.csv")

