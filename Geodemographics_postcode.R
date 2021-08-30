#Geodemorgaphics classification at postcode sector level 

#Loadiing required dataset ------
#Postcode to output area lookup
NSPCL_NOV19_UK_LU <- read_csv("~/Desktop/NSPCL_NOV19_UK_LU.csv")
#Output area classfication
Geodemoragphics_oa <- read_excel("~/Desktop/Geodemorgaphics/2011 OAC Clusters and Names Excel v2.xlsx", 
                                 sheet = "2011 OAC Clusters")
#Preprocess-----
#Subset Postcode to output area lookup table 
lookup = data.frame(postcode = NSPCL_NOV19_UK_LU$pcds,OA11CD = NSPCL_NOV19_UK_LU$oa11cd)
#Substring function 
postcode_sectorsubstr  = function(postcode){
  postcode = as.character(postcode)
  postcode_sector = paste(strsplit(postcode," ")[[1]][1], substr(strsplit(postcode, " ")[[1]][2],1,1))
  return(postcode_sector)
}
#Convert postcode to postcode sector level 
postcode_sector = sapply(lookup$postcode,postcode_sectorsubstr)
lookup_new = cbind(postcode_sector,lookup)
#Because postcode Each postcode covers an average of about 15 properties.
#However, this is not a definitive number, where postcodes can hold up to 100.
#OA are the smallest of the geographies that data is published 
#at and have an average population of about 310 residents 
#Create postcode sector to geodemographics table based on mode -------
library(dplyr)
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
colnames(Kantar_region)[3]  = "postcode_sector"
colnames(Kantar_region)[2] = "households" 
colnames(Geodemoragphics_oa)[1] = "OA11CD"
lookup_new_dedup  = lookup_new %>%
  distinct(postcode_sector, OA11CD, .keep_all = FALSE) %>% 
  left_join(Geodemoragphics_oa,by = "OA11CD") %>% 
  group_by(postcode_sector) %>%
  summarise(supergroup_mode = Mode(`Supergroup Name`), 
            subgroup_mode = Mode(`Group Name`) ) %>% 
  right_join(Kantar_region,by = "postcode_sector") %>% 
  select(postcode_sector,supergroup_mode,subgroup_mode,households)

#Write table 
write.csv(lookup_new_dedup,"geodemographics_postcode_sector.csv")
