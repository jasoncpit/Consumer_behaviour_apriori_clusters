#Postcode sector to msoa 
library(readr)
postcode_sectorsubstr  = function(postcode){
  postcode = as.character(postcode)
  postcode_sector = paste(strsplit(postcode," ")[[1]][1], substr(strsplit(postcode, " ")[[1]][2],1,1))
  return(postcode_sector)
}
postcodes <- read_csv("~/Desktop/postcodes.csv")
#Interesting variables 
#Postcode 
#Msoa 
#Dis2station 
#rural_urban 
#IMD 
postcodes_yes = postcodes[postcodes$`In Use?` == "Yes",]
postcode_sector_msoa <- data.frame(postcode = postcodes_yes$Postcode,msoa_code = postcodes_yes$`MSOA Code`, dist2station = postcodes_yes$`Distance to station`,
                                   rural_urban = postcodes_yes$`Rural/urban`,IMD = postcodes_yes$`Index of Multiple Deprivation`)
postcode_sector = sapply(postcode_sector_msoa$postcode,postcode_sectorsubstr)
postcode_sector_msoa = cbind(postcode_sector,postcode_sector_msoa)
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


for(i in 1:nrow(Kantar_region)){
  print(i)
  ps = Kantar_region$`Postcode Sector`[i]
  msoa_mode = Mode(postcode_sector_msoa[postcode_sector_msoa$postcode_sector == ps,]$msoa_code)
  rural_urban = Mode(postcode_sector_msoa[postcode_sector_msoa$postcode_sector == ps,]$rural_urban)
  IMD = mean(postcode_sector_msoa[postcode_sector_msoa$postcode_sector == ps,]$IMD)
  dis2station = mean(postcode_sector_msoa[postcode_sector_msoa$postcode_sector == ps,]$dist2station)
  Kantar_region$msoa_mode[i] = msoa_mode
  Kantar_region$rural_urban[i] = rural_urban
  Kantar_region$IMD[i] = IMD
  Kantar_region$dis2station[i] = dis2station
}
write.csv(Kantar_region,"Kantar_region_msoa.csv")


#Geodemographics 
NSPCL_FEB20_UK_LU <- read_csv("~/Desktop/NSPCL_FEB20_UK_LU.csv")
lsoa_oac =  data.frame(postcode = NSPCL_FEB20_UK_LU$pcds,lsoa11cd  = NSPCL_FEB20_UK_LU$lsoa11cd,soac11nm = NSPCL_FEB20_UK_LU$soac11nm)
postcode_sector = sapply(postcode_sector_msoa$postcode,postcode_sectorsubstr)
postcode_sector_msoa = cbind(postcode_sector,postcode_sector_msoa)
