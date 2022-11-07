# generate a IMD decile to approx townsend mapping file.
#http://medweb4.bham.ac.uk/websites/key_health_data/2005/ch_08.htm
#http://medweb4.bham.ac.uk/websites/key_health_data/2005/figures/ch_08/fig_08.01.htm
library(tidyverse)
lsoaIMD = readr::read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv")
lsoaTownsend = readr::read_csv("https://s3-eu-west-1.amazonaws.com/statistics.digitalresources.jisc.ac.uk/dkan/files/Townsend_Deprivation_Scores/Scores/Scores-%202011%20UK%20LSOA.csv")

imdToTownsend = lsoaIMD %>% inner_join(lsoaTownsend, by=c("LSOA code (2011)"="GEO_CODE"))
imdToTownsend2 = imdToTownsend %>% group_by(lad = `Local Authority District name (2019)`, imd_decile = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`) %>% summarise(mean_townsend = mean(TDS))
imdToTownsend3 = imdToTownsend %>% group_by(imd_decile = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`) %>% summarise(mean_townsend = mean(TDS))

ggplot(imdToTownsend2 %>% filter(lad %>% stringr::str_detect("ristol")), aes(x=as.factor(imd_decile),y=mean_townsend))+geom_point(colour="red")+
 geom_point(data=imdToTownsend3, mapping=aes(x=as.factor(imd_decile),y=mean_townsend))

imd_to_townsend = imdToTownsend3

## code to prepare `imd_to_townsend` dataset goes here

usethis::use_data(imd_to_townsend, overwrite = TRUE)
