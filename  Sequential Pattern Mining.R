#Analysing transactional details among different retail boundaries -------------
#Loading postcode_sector_shapefile
setwd("~/Desktop/Geodemorgaphics/Distribution")
postcode_sector_sp <- readOGR(dsn = ".", layer="Sectors")
postcode_sector_sp = spTransform(postcode_sector_sp, "+proj=longlat +datum=WGS84 +no_defs")
#Loading retail_centers shapfile 
setwd("~/Desktop/Sequential data mining")
st_layers("retailcentreboundaries.gpkg")
retail_centers <- st_read("retailcentreboundaries.gpkg", layer="retail_centre_boundaries")
retail_centers_sp <- as(retail_centers, "Spatial")
retail_centers_sp = spTransform(retail_centers_sp, "+proj=longlat +datum=WGS84 +no_defs")
retailcentretypology <- read_csv("retailcentretypology.csv")
#retail_centers_sp@data = merge(retail_centers_sp@data,retailcentretypology, by.x="id", by.y ="TC_ID",all.x= TRUE)

#Postcode_sector centroids 
postcode_sector_centroids = gCentroid(postcode_sector_sp, byid = TRUE)
postcode_sector_centroids <- cbind(postcode_sector_sp@data, postcode_sector_centroids)


#Ideally I want a dataframe to something like ... 

#Household Number - Postcode sector - Retail classfication (based on 1km buffer) 
postcode_sector_centriod_kantar = merge(postcode_sector_centroids,Kantar_region,by.x="name",by.y="Postcode Sector")

#I hate sf.... Letmme use sp  

#Transforming postcode centriod dataframes 
postcode_sector_centriod_sf = st_as_sf(postcode_sector_centriod_kantar,coords=c("x","y"),crs = "+proj=longlat +datum=WGS84 +no_defs" )
postcode_sector_centriod_sp = as(postcode_sector_centriod_sf, "Spatial")
postcode_sector_centriod_sp = spTransform(postcode_sector_centriod_sp, CRS="+proj=utm +zone=29N +datum=WGS84 +units=km")
#Transforming retails_centers spatial polygons
retail_centers_km = spTransform(retail_centers_sp, CRS = "+proj=utm +zone=29N +datum=WGS84 +units=km")
## Set up containers for results
n <- length(postcode_sector_centriod_sp)
nearesthighstreet <- character(n)
distToNearesthighstreet <- numeric(n)

## For each point, find name of nearest polygon (in this case, Belgian cantons)
for (i in seq_along(nearesthighstreet)) {
  print(i)
  gDists <- gDistance(postcode_sector_centriod_sp[i,], retail_centers_km, byid=TRUE)
  nearesthighstreet[i] <- retail_centers_km$id[which.min(gDists)]
  distToNearesthighstreet[i] <- min(gDists)
}

## Check that it worked
households_highstreet = cbind(data.frame(nearesthighstreet_code = nearesthighstreet, distToNearesthighstreet = distToNearesthighstreet),households = postcode_sector_centriod_sp$Household.Number)
#write.csv(households_highstreet,"households_highstreet.csv")
#Merging with retailcentretypology
#Read CSV 
#households_highstreet = read.csv("households_highstreet.csv")
households_highstreet_with_name = merge(households_highstreet,retailcentretypology,by.x ="nearesthighstreet_code",by.y="TC_ID")
#Fairly even distribution,... thats goods
#table(households_highstreet_with_name$Cluster)
households_highstreet_with_name$Cluster = as.numeric(households_highstreet_with_name$Cluster)
households_highstreet_with_retail_cluster = households_highstreet_with_name %>% 
  data.frame %>%
  mutate(retail_cluster = case_when(
  Cluster == 1 ~ "Local retail & service centres", 
  Cluster == 2 ~ "Retail, shopping & leisure parks",
  Cluster == 3 ~ "Leading comparison & leisure destinations", 
  Cluster == 4 ~ "Primary food and secondary comparison destinations", 
  Cluster == 5 ~ "Traditional high streets & market towns")
  )
households_highstreet_with_retail_cluster$retail_cluster = as.factor(households_highstreet_with_retail_cluster$retail_cluster )
#Outhome 
retail_cluster_outhome = jason_apriori_bulk(trans_sequence_outhome_all,households_highstreet_with_retail_cluster,"retail_cluster",mainDir="~/Desktop/Sequential data mining",subDir= "retail_cluster_outhome",supp = 0.01, conf = 0.5)
#Inhome 
retail_cluster_inhome = jason_apriori_bulk(trans_sequence_inhome_all,households_highstreet_with_retail_cluster,"retail_cluster",mainDir="~/Desktop/Sequential data mining",subDir= "retail_cluster_inhome",supp = 0.01, conf = 0.5)
                                                                                                                                                                                                            
#Visualisation 
formattable(retail_cluster_outhome,
            list(
              `rules` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `Local retail & service centres` =color_tile_mean(),
              `Retail, shopping & leisure parks`=color_tile_mean(),
              `Leading comparison & leisure destinations`=color_tile_mean(),
              `Primary food and secondary comparison destinations`=color_tile_mean(),
              `Traditional high streets & market towns`=color_tile_mean()
            )
)

formattable(retail_cluster_inhome,
            list(
              `rules` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `Local retail & service centres` =color_tile_mean(),
              `Retail, shopping & leisure parks`=color_tile_mean(),
              `Leading comparison & leisure destinations`=color_tile_mean(),
              `Primary food and secondary comparison destinations`=color_tile_mean(),
              `Traditional high streets & market towns`=color_tile_mean()
            )
)


#Analysing transactional details based on distance to the nearest highstreet-------------
library(mltools)
bin = 10 #for 1/3 rd, 4 for 1/4, 100 for 1/100th etc
households_highstreet_with_name[, "distance_cluster"] <- bin_data(households_highstreet_with_name$distToNearesthighstreet, bins=c(-Inf, 1, 5, 10,100), binType = "explicit")
Inhome_distance_cluster = jason_apriori_bulk(trans_sequence_inhome_all,households_highstreet_with_name,"distance_cluster",mainDir="~/Desktop/Sequential data mining",subDir= "Inhome_distance_cluster",supp = 0.01, conf = 0.5)
Outhome_distance_cluster = jason_apriori_bulk(trans_sequence_outhome_all,households_highstreet_with_name,"distance_cluster",mainDir="~/Desktop/Sequential data mining",subDir= "Outhome_distance_cluster",supp = 0.01, conf = 0.5)

#Inhome
formattable(Inhome_distance_cluster ,
            list(
              `rules` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `[-Inf, 1)`=color_tile_mean(),
              `[1, 5)`=color_tile_mean(),
              `[5, 10)`=color_tile_mean(),
              `[10, 100]`=color_tile_mean()
            )
)
#Outhome
formattable(Outhome_distance_cluster ,
            list(
              `rules` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `[-Inf, 1)`=color_tile_mean(),
              `[1, 5)`=color_tile_mean(),
              `[5, 10)`=color_tile_mean(),
              `[10, 100]`=color_tile_mean()
            )
)
#Further example --------

#sub_clusters 
households_highstreet_with_name$new_cluster= as.factor(households_highstreet_with_name$SubCluster)
jason_apriori_bulk(trans_sequence_outhome_all,households_highstreet_with_name,"new_cluster",mainDir="~/Desktop/Sequential data mining",subDir= "demoaa",supp = 0.01, conf = 0.5, colnames = c("rules","1.1","1.2","1.3","2.1","2.2","3.1","3.2","3.3","4.1","4.2","4.3","5.1","5.2","5.3","5.4"))

#Dist set to 10 intervals 
households_highstreet_with_name[, "distance_cluster10"] <- bin_data(households_highstreet_with_name$distToNearesthighstreet, bins=c(-Inf, 1,3,5,10,30,50,100), binType = "explicit")
households_highstreet_with_name$distance_cluster10= as.factor(households_highstreet_with_name$distance_cluster10)
jason_apriori_bulk(trans_sequence_inhome_all,households_highstreet_with_name,"distance_cluster10",mainDir="~/Desktop/Sequential data mining",subDir= "inhome_dist_8",supp = 0.01, conf = 0.5, colnames = c("rules","dist_0_1","dist_1_3","dist_3_5","dist_5_10","dist_10_30","dist_30_50","dist_50_100"))
jason_apriori_bulk(trans_sequence_outhome_all,households_highstreet_with_name,"distance_cluster10",mainDir="~/Desktop/Sequential data mining",subDir= "outhome_dist_8",supp = 0.01, conf = 0.5, colnames = c("rules","dist_0_1","dist_1_3","dist_3_5","dist_5_10","dist_10_30","dist_30_50","dist_50_100"))


#Analysing transactional details among different geodemographics group - MSOA -------------
library(readr)
geodemographics_postcode_sector <- read_csv("geodemographics_postcode_sector.csv")
geodemographics_postcode_sector  <- data.frame(geodemographics_postcode_sector)
geodemographics_postcode_sector$supergroup_mode =  as.factor(geodemographics_postcode_sector$supergroup_mode)
geodemographics_postcode_sector$subgroup_mode =  as.factor(geodemographics_postcode_sector$subgroup_mode)

#Outhome 
geodemo_supergroup_mode_outhome = jason_apriori_bulk(trans_sequence_outhome_all,
                                            geodemographics_postcode_sector,
                                            "supergroup_mode",
                                            mainDir="~/Desktop/Sequential data mining",subDir= "geodemo_supergroup_mode_outhome",
                                            supp = 0.01, conf = 0.5)
#Inhome 
geodemo_supergroup_mode_inhome = jason_apriori_bulk(trans_sequence_inhome_all,
                                                     geodemographics_postcode_sector,
                                                     "supergroup_mode",
                                                     mainDir="~/Desktop/Sequential data mining",subDir= "geodemo_supergroup_mode_inhome",
                                                     supp = 0.01, conf = 0.3)

#Outhome 
geodemo_supergroup_mode_outhome = jason_apriori_bulk(trans_sequence_outhome_all,
                                                     geodemographics_postcode_sector,
                                                     "subgroup_mode",
                                                     mainDir="~/Desktop/Sequential data mining",subDir= "geodemo_subgroup_mode_outhome",
                                                     supp = 0.01, conf = 0.5)
#Inhome 
geodemo_supergroup_mode_inhome = jason_apriori_bulk(trans_sequence_inhome_all,
                                                    geodemographics_postcode_sector,
                                                    "subgroup_mode",
                                                    mainDir="~/Desktop/Sequential data mining",subDir= "geodemo_subgroup_mode_inhome",
                                                    supp = 0.01, conf = 0.5)

#Analysing transactional details based on SES and urban/rurality ---------- 

library(readr)
library("arulesViz")
rural_urban_msoa <- read_csv("~/Desktop/Sequential data mining/Rural_Urban_Classification_(2011)_of_Middle_Layer_Super_Output_Areas_in_England_and_Wales.csv")
Kantar_region_msoa <- read_csv("~/Desktop/Sequential data mining/Kantar_region_msoa.csv")
Kantar_rural_urban = merge(Kantar_region_msoa,rural_urban_msoa,by.x ="msoa_mode",by.y = "MSOA11CD" )
Kantar_rural_urban$RUC11 = as.factor(Kantar_rural_urban$RUC11)
colnames(Kantar_rural_urban)[4] = "households"
#Outhome 
rural_urban_outhome = jason_apriori_bulk(trans_sequence_outhome_all,Kantar_rural_urban,"RUC11",mainDir="~/Desktop/Sequential data mining",subDir= "rural_urban_outhome",supp = 0.05, conf = 0.5)                                                                                                                                                                                                  
rural_urban_outhome_rules = jason_apriori_bulk(trans_sequence_outhome_all,Kantar_rural_urban,"RUC11",mainDir="~/Desktop/Sequential data mining",subDir= "rural_urban_outhome",supp = 0.05, conf = 0.5,df_output = FALSE)                                                                                                                                                                                                  
s <- rural_urban_outhome_rules[1,itemFrequency(rural_urban_outhome_rules[[1]])>0.05]

d_affinity = dissimilarity(rural_urban_outhome_rules[[1]],method = "affinity", 
                           args = list(transactions = s))
hc <- hclust(d_affinity, method = "ward.D2")
plot(hc, labels = FALSE, main = "Dendrogram for Transactions (Affinity)")

formattable(rural_urban_outhome ,
            list(
              `rules` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `Rural town and fringe`=color_tile_mean(),
              `Rural town and fringe in a sparse setting`=color_tile_mean(),
              `Rural village and dispersed`=color_tile_mean(),
              `Rural village and dispersed in a sparse settings`=color_tile_mean(),
              `Urban city and town` = color_tile_mean(),
              `Urban city and town in a sparse setting`=color_tile_mean(),
              `Urban major conurbation`=color_tile_mean(),
              `Urban minor conurbation` = color_tile_mean()
              
            )
)

#Inhome 
rural_urban_inhome = jason_apriori_bulk(trans_sequence_inhome_all,Kantar_rural_urban,"RUC11",mainDir="~/Desktop/Sequential data mining",subDir= "rural_urban_inhome",supp = 0.05, conf = 0.5)

formattable(rural_urban_inhome ,
            list(
              `rules` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `Rural town and fringe`=color_tile_mean(),
              `Rural town and fringe in a sparse setting`=color_tile_mean(),
              `Rural village and dispersed`=color_tile_mean(),
              `Rural village and dispersed in a sparse settings`=color_tile_mean(),
              `Urban city and town` = color_tile_mean(),
              `Urban city and town in a sparse setting`=color_tile_mean(),
              `Urban major conurbation`=color_tile_mean(),
              `Urban minor conurbation` = color_tile_mean()
              
            )
)
#Households lifestage ------ 
library(dplyr)
colnames(Inhome_panel)[1] = "households"
Inhome_panel$LIFESTGE = as.numeric(Inhome_panel$LIFESTGE)
Inhome_panel_new = Inhome_panel %>% select(c("households","LIFESTGE"))%>% 
  as.data.frame %>%
  mutate(LIFESTGE_group = case_when(
  LIFESTGE == 1 ~ "Young Family 0-4 Years",
  LIFESTGE == 2 ~ "Middle Family 5-9 Years",
  LIFESTGE == 3 ~ "Family 10+ Years",
  LIFESTGE == 4 ~ "Older Dependents",
  LIFESTGE == 5 ~ "Empty Nesters",
  LIFESTGE == 6 ~ "Retired",
  LIFESTGE == 7 ~ "Presence of Children")
)
Inhome_panel_new[,"LIFESTGE_group"] = as.factor(Inhome_panel_new$LIFESTGE_group)
#Outhome 
trans_sequence_inhome_all <- read_csv("trans_sequence_inhome_all.csv")

LIFESTGE_outhome = jason_apriori_bulk(trans_sequence_outhome_all,Inhome_panel_new,"LIFESTGE_group",mainDir="~/Desktop/Sequential data mining",subDir= "LIFESTGE_outhome",supp = 0.02, conf = 0.5)
formattable(LIFESTGE_outhome ,
            list(
              `rules` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `Empty Nesters`=color_tile_mean(),
              `Family 10+ Years`=color_tile_mean(),
              `Middle Family 5-9 Years`=color_tile_mean(),
              `Older Dependents`=color_tile_mean(),
              `Presence of Children` = color_tile_mean(),
              `Retired`=color_tile_mean(),
              `Young Family 0-4 Years`=color_tile_mean()
              
            )
)

#Inhome 
LIFESTGE_inhome = jason_apriori_bulk(trans_sequence_inhome_all,Inhome_panel_new,"LIFESTGE_group",mainDir="~/Desktop/Sequential data mining",subDir= "LIFESTGE_inhome",supp = 0.02, conf = 0.5)
#Classification of Workplace Zones (COWZ)
formattable(LIFESTGE_inhome ,
            list(
              `rules` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `Empty Nesters`=color_tile_mean(),
              `Family 10+ Years`=color_tile_mean(),
              `Middle Family 5-9 Years`=color_tile_mean(),
              `Older Dependents`=color_tile_mean(),
              `Presence of Children` = color_tile_mean(),
              `Retired`=color_tile_mean(),
              `Young Family 0-4 Years`=color_tile_mean()
              
            )
)
#IMD------ 
Kantar_region$IMD
library(mltools)
Kantar_region_msoa= data.frame(Kantar_region_msoa)
Kantar_region_msoa[, "IMD_decile"] <- bin_data(Kantar_region_msoa$IMD, bins=5, binType = "quantile")
colnames(Kantar_region_msoa)[3] = "households"
#Outhome 
IMD_rules_outhome = jason_apriori_bulk(trans_sequence_outhome_all,Kantar_region_msoa,"IMD_decile",mainDir="~/Desktop/Sequential data mining",subDir= "5IMD_decile_outhome",supp = 0.01, conf = 0.5
                                       )
#Inhome 
IMD_rules_inhome =  jason_apriori_bulk(trans_sequence_inhome_all,Kantar_region,"IMD_decile",mainDir="~/Desktop/Sequential data mining",subDir= "5IMD_decile_inhome",supp = 0.01, conf = 0.5)
#Inhome
formattable(IMD_rules_inhome ,
            list(
              `rules` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `IMD_1`=color_tile_mean(),
              `IMD_2`=color_tile_mean(),
              `IMD_3`=color_tile_mean(),
              `IMD_4`=color_tile_mean(),
              `IMD_5`=color_tile_mean(),
              `IMD_6`=color_tile_mean(),
              `IMD_7`=color_tile_mean(),
              `IMD_8`=color_tile_mean(),
              `IMD_9`=color_tile_mean(),
              `IMD_10`=color_tile_mean()

            )
)
#Outhome 
formattable(IMD_rules_outhome ,
            list(
              `rules` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              `IMD_1`=color_tile_mean(),
              `IMD_2`=color_tile_mean(),
              `IMD_3`=color_tile_mean(),
              `IMD_4`=color_tile_mean(),
              `IMD_5`=color_tile_mean(),
              `IMD_6`=color_tile_mean(),
              `IMD_7`=color_tile_mean(),
              `IMD_8`=color_tile_mean(),
              `IMD_9`=color_tile_mean(),
              `IMD_10`=color_tile_mean()
              
            )
)
#Internet User Classification-------------


#Some thoughts ---------
Kantar_region$msoa_mode = factor(Kantar_region$msoa_mode)
all_levels = levels(Kantar_region[Kantar_region$`Postcode district` == "London",]$msoa_mode)
your_length <- length(all_levels)
msoa_name <- rep(NA, your_length)
for(i in 1:length(all_levels)){
  msoa_name[i] = all_levels[i]
}
msoa_name= paste(c("rules",msoa_name))
MSOA_rules =  jason_apriori_bulk(trans_sequence_inhome_all,Kantar_region[Kantar_region$`Postcode district` == "London",],"msoa_mode",mainDir="~/Desktop/Sequential data mining",subDir= "5IMD_decile_inhome",supp = 0.01, conf = 0.3, 
                                 colnames = msoa_name)









#Unused code -------

#Prototyping.... 
#Analysing association rules under different retail structures 
#Subset households from different clusters 
cluster1 = households_highstreet_with_name[households_highstreet_with_name$Cluster == 1,]
cluster2 = households_highstreet_with_name[households_highstreet_with_name$Cluster == 2,]
cluster3 = households_highstreet_with_name[households_highstreet_with_name$Cluster == 3,]
cluster4 = households_highstreet_with_name[households_highstreet_with_name$Cluster == 4,]
cluster5 = households_highstreet_with_name[households_highstreet_with_name$Cluster == 5,]

trans_sequence_inhome_all$all_transaction = str_replace_all(string=trans_sequence_inhome_all$all_transaction, pattern=";", repl=",")

cluster1_inhome_transactions =data.frame(itemList = trans_sequence_inhome_all[trans_sequence_inhome_all$households %in% cluster1$households,"all_transaction"])
cluster2_inhome_transactions =data.frame(itemList = trans_sequence_inhome_all[trans_sequence_inhome_all$households %in% cluster2$households,"all_transaction"])
cluster3_inhome_transactions =data.frame(itemList = trans_sequence_inhome_all[trans_sequence_inhome_all$households %in% cluster3$households,"all_transaction"])
cluster4_inhome_transactions =data.frame(itemList = trans_sequence_inhome_all[trans_sequence_inhome_all$households %in% cluster4$households,"all_transaction"])
cluster5_inhome_transactions =data.frame(itemList = trans_sequence_inhome_all[trans_sequence_inhome_all$households %in% cluster5$households,"all_transaction"])

trans_sequence_outhome_all$all_transaction = str_replace_all(string=trans_sequence_outhome_all$all_transaction, pattern=";", repl=",")
cluster1_outhome_transactions =data.frame(itemList = trans_sequence_outhome_all[trans_sequence_outhome_all$house %in% cluster1$households,"all_transaction"])
cluster2_outhome_transactions =data.frame(itemList = trans_sequence_outhome_all[trans_sequence_outhome_all$house %in% cluster2$households,"all_transaction"])
cluster3_outhome_transactions =data.frame(itemList = trans_sequence_outhome_all[trans_sequence_outhome_all$house %in% cluster3$households,"all_transaction"])
cluster4_outhome_transactions =data.frame(itemList = trans_sequence_outhome_all[trans_sequence_outhome_all$house %in% cluster4$households,"all_transaction"])
cluster5_outhome_transactions =data.frame(itemList = trans_sequence_outhome_all[trans_sequence_outhome_all$house %in% cluster5$households,"all_transaction"])


#Bulk-savings-transaction-details 
inhome_transaction_list <- list(cluster1_inhome_transactions = cluster1_inhome_transactions, 
                                cluster2_inhome_transactions = cluster2_inhome_transactions,
                                cluster3_inhome_transactions = cluster3_inhome_transactions,
                                cluster4_inhome_transactions =cluster4_inhome_transactions,
                                cluster5_inhome_transactions= cluster5_inhome_transactions)
for(i in names(inhome_transaction_list)){
  write.csv(inhome_transaction_list[[i]], paste0(i,".csv"),row.names = FALSE,quote = FALSE)
}


outhome_transaction_list <- list(cluster1_outhome_transactions = cluster1_outhome_transactions, 
                                 cluster2_outhome_transactions = cluster2_outhome_transactions,
                                 cluster3_outhome_transactions = cluster3_outhome_transactions,
                                 cluster4_outhome_transactions =cluster4_outhome_transactions,
                                 cluster5_outhome_transactions= cluster5_outhome_transactions)
for(i in names(outhome_transaction_list)){
  write.csv(outhome_transaction_list[[i]], paste0(i,".csv"),row.names = FALSE,quote = FALSE)
}

#Bulk readiing transaction_details-------- 
library(arules)
setwd("~/Desktop/Sequential data mining/Inhome_transactions")
inhome_transaction_list <-
  list.files(pattern = "*.csv") %>% 
  map(~read.transactions(file=., rm.duplicates= TRUE, format="basket",sep=","))
setwd("~/Desktop/Sequential data mining/Outhome_transactions")
Outhome_transaction_list <-
  list.files(pattern = "*.csv") %>% 
  map(~read.transactions(file=., rm.duplicates= TRUE, format="basket",sep=","))

#Determning support and confidence threshoulds-------
no_dataframe <- 5 # or whatever length you want
Inhome_rules <- vector(mode = "list", length = no_dataframe)
Outhome_rules <- vector(mode = "list", length = no_dataframe)

for(i in 1:no_dataframe){
  Inhome_rules[[i]] <- apriori(inhome_transaction_list[[i]],parameter =list(supp = 0.01, conf = 0.5, target = "rules"))
  Outhome_rules[[i]] <- apriori(Outhome_transaction_list[[i]],parameter =list(supp = 0.01, conf = 0.5, target = "rules"))
}


#Redundant rules filtering 
#i.e. school -> coffee, coffee -> school 
#i.e. subseting problems 
#Standardising rules for visualisation


association_rules_2_df = function(rule){
  rule_filter = rule[!is.redundant(rule)]
  rule_filter_df = data.frame(
    lhs = labels(lhs(rule_filter)),
    rhs = labels(rhs(rule_filter)), 
    cluster_confidence_lift = paste0(formattable(rule_filter@quality$confidence,digits =2,format ="f"),
                                     "/",
                                     formattable(rule_filter@quality$lift,digits =2,format ="f")))
  return(rule_filter_df)
}
Inhome_rules_df <- vector(mode = "list", length = no_dataframe)
Outhome_rules_df <- vector(mode = "list", length = no_dataframe)
for(i in 1:no_dataframe){
  Inhome_rules_df[[i]] <- association_rules_2_df(Inhome_rules[[i]])
  Outhome_rules_df[[i]] <- association_rules_2_df(Outhome_rules[[i]])
}


#Visualisation --------
#Ideal output 
#Clusters/Rules - cluster 1 - cluster 2 - cluster 3..... 
#Supermarkets => Core-sparklings - 0.4(0.53) - 0.5(0.1) - 0.4(0.5)..... a

library("formattable")
library(purrr)
#Inhome
Inhome_all_rules = Inhome_rules_df %>% reduce(full_join, by =  c("lhs","rhs"))
Inhome_all_rules$rules = paste0(Inhome_all_rules$lhs,"=>",Inhome_all_rules$rhs)
colnames(Inhome_all_rules) = c("lhs","rhs","cluster1", "cluster2","cluster3", "cluster4","cluster5","rules")
#Outhome
Outhome_all_rules = Outhome_rules_df %>% reduce(full_join, by =  c("lhs","rhs"))
Outhome_all_rules$rules = paste0(Outhome_all_rules$lhs,"=>",Outhome_all_rules$rhs)
colnames(Outhome_all_rules) = c("lhs","rhs","cluster1", "cluster2","cluster3", "cluster4","cluster5","rules")


# Define color_tile_mean function
color_tile_mean <- function (...) {
  #Define_colors
  customGreen0 = "#DeF7E9"
  customGreen = "#71CA97"
  formatter("span", style = function(x) {
    style(display = "block",
          padding = "0 4px", 
          `border-radius` = "4px", 
          `background-color` = csscolor(gradient(as.numeric(substr(x,1,3)),customGreen0, customGreen)))
  })}
#Inhome
formattable(Inhome_all_rules %>% select(-c("lhs","rhs")) %>%
              relocate(rules, .before = cluster1) %>%
              arrange(.,desc(as.numeric(substr(cluster1,1,3)))) %>% 
              mutate_all(~str_remove(.,"NA")) ,
            list(
              `rules` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              cluster1=color_tile_mean(),
              cluster2=color_tile_mean(),
              cluster3=color_tile_mean(),
              cluster4=color_tile_mean(),
              cluster5=color_tile_mean()
            )
)
#Outhome
formattable(Outhome_all_rules %>% select(-c("lhs","rhs")) %>%
              relocate(rules, .before = cluster1) %>%
              arrange(.,desc(as.numeric(substr(cluster1,1,3)))) %>% 
              mutate_all(~str_remove(.,"NA")) ,
            list(
              `rules` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
              cluster1=color_tile_mean(),
              cluster2=color_tile_mean(),
              cluster3=color_tile_mean(),
              cluster4=color_tile_mean(),
              cluster5=color_tile_mean()
            )
)
#Sensitivity analysis------
#Cross-validation
#Comparing distribution between different groups -> see whether there is a significatn differnce between groups 
#Associative Classification
library("arulesCBA")

#Demo-------
##  First project data into a planar coordinate system (here UTM zone 32)
utmStr <- "+proj=utm +zone=%d +datum=NAD83 +units=m +no_defs +ellps=GRS80"
crs <- CRS(sprintf(utmStr, 42))
postcode_sector_centriod_sp <- spTransform(postcode_sector_centriod_sp, crs)
retail_centers_sp <- spTransform(retail_centers_sp, crs)

# convert points into sf object
postcode_sector_centriod_sf = st_as_sf(postcode_sector_centriod_kantar,coords=c("x","y"))

# convert sp to sf
retail_centers_sf = st_as_sf(retail_centers_sp)

#equaling crs
st_crs(postcode_sector_centriod_sf) = st_crs(retail_centers_sp)

#Transform to metric coordinate system:
postcode_sector_centriod_km = st_transform(postcode_sector_centriod_sf, "+proj=utm +zone=42N +datum=WGS84 +units=km")
retail_centers_km = st_transform(retail_centers_sf, "+proj=utm +zone=42N +datum=WGS84 +units=km")
#Create buffer
postcode_sector_centriod_buffer = st_buffer(postcode_sector_centriod_km,3)
intersection = st_intersection(postcode_sector_centriod_buffer, retail_centers_km)

#Convert to sp a
intersection_sp <- as(intersection, "Spatial")
kantar_region_retail_boundaries = data.frame(households = intersection_sp@data$Household.Number,name = intersection_sp$name,classfication = intersection_sp$Classification)

#Analying transactional sequence based on retail center categories 
town_centers = trans_sequence_outhome_all[trans_sequence_outhome_all$sequenceID %iß% kantar_region_retail_boundaries[kantar_region_retail_boundaries$classfication =="Town Centre",]$households,]
local_centers = trans_sequence_outhome_all[trans_sequence_outhome_all$sequenceID %in% kantar_region_retail_boundaries[kantar_region_retail_boundaries$classfication =="Local Centre",]$households,]
Market_Town= trans_sequence_outhome_all[trans_sequence_outhome_all$sequenceID %in% kantar_region_retail_boundaries[kantar_region_retail_boundaries$classfication =="Market Town",]$households,]

write.table(town_centers, "town_centers.txt", sep=" ", row.names = FALSE, col.names = FALSE, quote = FALSE)
tc_trans_matrix <- read_baskets("town_centers.txt", info = c("sequenceID","eventID","SIZE"))
tc_trans_matrix_s <- cspade(tc_trans_matrix, parameter = list(support = 0.25), control = list(verbose = TRUE,tidLists = TRUE))
sequence = as(tc_trans_matrix_s, "data.frame")
tc_rules <- ruleInduction(tc_trans_matrix_s, 
                          confidence = 0.5, 
                          control = list(verbose = FALSE))

tc_rules_cleaned <- tc_rules[!is.redundant(tc_rules)]
tc_rules_df <- as(tc_rules_cleaned, "data.frame") %>% 
  as_tibble() %>% 
  separate(col = rule, into = c('lhs', 'rhs'), sep = " => ", remove = F)

write.table(Market_Town, "Market_Town.txt", sep=" ", row.names = FALSE, col.names = FALSE, quote = FALSE)
MT_trans_matrix <- read_baskets("Market_Town.txt", info = c("sequenceID","eventID","SIZE"))
MT_trans_matrix_s <- cspade(MT_trans_matrix, parameter = list(support = 0.25), control = list(verbose = TRUE,tidLists = TRUE))
sequence2 = as(MT_trans_matrix_s, "data.frame")
MT_rules <- ruleInduction(MT_trans_matrix_s, 
                          confidence = 0.5, 
                          control = list(verbose = FALSE))

MT_rules_cleaned <- MT_rules[!is.redundant(MT_rules)]
MT_rules_df <- as(MT_rules_cleaned, "data.frame") %>% 
  as_tibble() %>% 
  separate(col = rule, into = c('lhs', 'rhs'), sep = " => ", remove = F)



write.table(trans_sequence_outhome_all[1:500,], "mytxtout.txt", sep=" ", row.names = FALSE, col.names = FALSE, quote = FALSE)
trans_matrix <- read_baskets("mytxtout.txt", info = c("sequenceID","eventID","SIZE"))
inspect(trans_matrix)

# Get frequent sequences and corresponding support values
s1 <- cspade(trans_matrix, parameter = list(support = 0.5), control = list(verbose = TRUE,tidLists = TRUE))
as(s1, "data.frame")
summary(s1)

rules <- ruleInduction(s1, 
                       confidence = 0.5, 
                       control = list(verbose = FALSE))

rules_cleaned <- rules[!is.redundant(rules)]
rules_df <- as(rules_cleaned, "data.frame") %>% 
  as_tibble() %>% 
  separate(col = rule, into = c('lhs', 'rhs'), sep = " => ", remove = F)

#Visualisations
library(ggtext) #Making adding some flair to plots
library(tidygraph)  ## Creating a Graph Structure
library(tidytext)
rules_df %>%  
  arrange(desc(support)) %>% 
  head() %>% 
  #Remove All Rules that Involve Google
  #Keep only Rule, Confidence, and Lift - 1
  transmute(rule, confidence, support) %>% 
  #Pivot Lift and confidence into a single column
  pivot_longer(cols = c('confidence','support'),
               names_to = "metric", 
               values_to = "value") %>% 
  group_by(metric) %>% 
  #Keep only the Top 3 Rules for Each Metric
  top_n(10, value) %>% 
  ungroup() %>% 
  # Reorder so that order is independent for each metrics
  ggplot(aes(x = tidytext::reorder_within(rule, value, metric),
             y = value,
             fill = rule)) + 
  geom_col() + 
  geom_label(aes(label = value %>% scales::percent()), 
             hjust = 0) +
  scale_fill_discrete(guide = F) + 
  tidytext::scale_x_reordered() + 
  scale_y_continuous(label = scales::percent, 
                     limits = c(0, 1),
                     expand = expansion(mult = c(0, .1))) + 
  labs(x = "Rule", 
       y = "", 
       title = "Top Rules by Confidence and Lift",
       caption = "**Confidence** is the probability RHS occurs 
         given LHS occurs <br>
         **Lift** is the increased liklihood of seeing LHS & RHS together vs. independent") +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  coord_flip() +
  theme_minimal() +
  theme(
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = 'plot',
    strip.text = element_textbox(
      size = 12,
      color = "white", fill = "#5D729D", box.color = "#4A618C",
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    )
  )

library("arulesViz")

plot(rules, method = "grouped matrix", engine = "interactive")



#Creating sequential pattern mining -------- 
#Combining sequence 
trans_sequence_inhome_all = rbind(trans_sequence_store_inhome,trans_sequence_product_inhome)
trans_sequence_outhome_all = rbind(trans_sequence_store_outhome,trans_sequence_product_outhome)
#Reordering sequence
trans_sequence_inhome_all <- trans_sequence_inhome_all[order(trans_sequence_inhome_all$households, trans_sequence_inhome_all$date),]
trans_sequence_outhome_all <- trans_sequence_outhome_all[order(trans_sequence_outhome_all$house, trans_sequence_outhome_all$date),]
#Make eventID
trans_sequence_inhome_all = trans_sequence_inhome_all %>% group_by(households) %>% mutate(eventID = 100 + row_number(households))
trans_sequence_outhome_all = trans_sequence_outhome_all %>% group_by(house) %>% mutate(eventID = 100 + row_number(house))
#Convert SequenceID and EventID into factor + reordering 
trans_sequence_inhome_all = trans_sequence_inhome_all[,c(1,5,3,4)]
names(trans_sequence_inhome_all) = c("sequenceID", "eventID", "SIZE", "items")
trans_sequence_inhome_all <- data.frame(lapply(trans_sequence_inhome_all, as.factor))
trans_sequence_inhome_all <- trans_sequence_inhome_all[order(trans_sequence_inhome_all$sequenceID, trans_sequence_inhome_all$eventID),]

trans_sequence_outhome_all = trans_sequence_outhome_all[,c(1,5,3,4)]
names(trans_sequence_outhome_all) = c("sequenceID", "eventID", "SIZE", "items")
trans_sequence_outhome_all <- data.frame(lapply(trans_sequence_outhome_all, as.factor))
trans_sequence_outhome_all <- trans_sequence_outhome_all[order(trans_sequence_outhome_all$sequenceID, trans_sequence_outhome_all$eventID),]


#Un-used code------

#Writing rules
trans_sequence_inhome_all_demo = data.frame(trans_sequence_inhome_all[1:50000,"all_transaction"])
colnames(trans_sequence_inhome_all_demo) <- c("itemList")
trans_sequence_inhome_all_demo = str_replace_all(string=trans_sequence_inhome_all_demo$itemList, pattern=";", repl=",")

trans_sequence_outhome_all_demo = data.frame(trans_sequence_outhome_all[1:50000,"all_transaction"])
colnames(trans_sequence_outhome_all_demo) <- c("itemList")
trans_sequence_outhome_all_demo = str_replace_all(string=trans_sequence_outhome_all_demo$itemList, pattern=";", repl=",")

write.csv(trans_sequence_inhome_all_demo,"Inhome_ItemList.csv",  row.names = FALSE,quote = FALSE)
write.csv(trans_sequence_outhome_all_demo,"Outhome_ItemList.csv",  row.names = FALSE,quote = FALSE)

#Small test 

#Association rules
library(arules)
Inhome_basket = read.transactions(file="Inhome_ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",");
Outhome_basket = read.transactions(file="Outhome_ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",")
inspect(Outhome_basket)
itemLabels(Outhome_basket)
#Rule 
if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:tm, unload=TRUE)
}
basket_rules <- apriori(Outhome_basket,parameter =list(supp = 0.01, conf = 0.5, target = "rules"))
library(arulesViz)
subrules2 <- head(basket_rules, n = 10, by = "lift")
plot(subrules2, method = "paracoord")


#Demo-------
Demo_sequence= trans_sequence_outhome_all[trans_sequence_outhome_all$sequenceID %in% Kantar_region[Kantar_region$`UK region` == "Greater London",]$`Household Number`,]

write.table(Demo_sequence, "Demo_sequence.txt", sep=" ", row.names = FALSE, col.names = FALSE, quote = FALSE)
tc_trans_matrix <- read_baskets("Demo_sequence.txt", info = c("sequenceID","eventID","SIZE"))
tc_trans_matrix_s0 <- cspade(tc_trans_matrix, parameter = list(support = 0.25), control = list(verbose = TRUE,tidLists = TRUE))
sequence = as(tc_trans_matrix_s0, "data.frame")
tc_trans_matrix_s1 <- cspade(tc_trans_matrix, parameter = list(support = 0.25,maxgap=1), control = list(verbose = TRUE,tidLists = TRUE))
sequence2 = as(tc_trans_matrix_s1, "data.frame")
tc_rules <- ruleInduction(tc_trans_matrix_s0, 
                          confidence = 0.5, 
                          control = list(verbose = FALSE))

tc_rules_cleaned <- tc_rules[!is.redundant(tc_rules)]
tc_rules_df1 <- as(tc_rules_cleaned, "data.frame") %>% 
  as_tibble() %>% 
  separate(col = rule, into = c('lhs', 'rhs'), sep = " => ", remove = F)

tc_rules <- ruleInduction(tc_trans_matrix_s1, 
                          confidence = 0.5, 
                          control = list(verbose = FALSE))

tc_rules_cleaned <- tc_rules[!is.redundant(tc_rules)]
tc_rules_df2 <- as(tc_rules_cleaned, "data.frame") %>% 
  as_tibble() %>% 
  separate(col = rule, into = c('lhs', 'rhs'), sep = " => ", remove = F)


*+66666666666666666666666666666666666666666666666666666666666666666666664777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777§+6666666666666666666666666666666666666666666****33333333333333/47