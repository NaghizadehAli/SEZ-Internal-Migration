rm(list = ls())
library(readxl)
library(tidyverse)
getwd()

County <- readRDS("F:/LFS/Processed data/County_ID.RDS")

County <- County%>%
  select(HHID,County_ID_23)%>%
  rename("County_ID" = "County_ID_23")

TRTo82 <- readRDS("Data/County_Division/Cleaned Data/TRTo82.RDS")
TRTo82 <- TRTo82%>%
  mutate(Key_Join = paste0(Year,C_ID))%>%
  select(Key_Join,CID_82)

D_Y   <- read_xlsx("Data/County_Division/Cleaned Data/Division_Year.xlsx") 

LFSCKJ <- tibble() # LFS County Key Join


for (year in 84:98) {
  W3 <- readRDS(paste0("F:/LFS/Processed data/",year,"/W3.RDS"))
  
  W3 <- W3%>%
    select(Year,Season,HHID,Province_ID)%>%
    left_join(County , by = "HHID")%>%
    left_join(D_Y,by = c("Year","Season"))%>%
    mutate(Key_Join = paste0(Division_Year,Province_ID,County_ID))%>%
    left_join(TRTo82,by = "Key_Join")%>%
    select(Year,Season,Province_ID,County_ID,CID_82)%>%
    distinct(Year,Season,Province_ID,County_ID,County_ID,.keep_all = T)%>%
    mutate(Seasonal_KJ = paste0(Year,Season,Province_ID,County_ID))%>%
    select(Seasonal_KJ,CID_82)
  
  LFSCKJ<- bind_rows(LFSCKJ,W3)
}


LFSCKJ <- LFSCKJ%>%
  filter(str_sub(Seasonal_KJ,7,8) != "NA")%>%
  mutate(CID_82 = case_when(
    is.na(CID_82) & str_sub(Seasonal_KJ,5,8) == "2813" ~ "0913",
    is.na(CID_82) & str_sub(Seasonal_KJ,5,8) == "2911" ~ "0911",
    is.na(CID_82) & str_sub(Seasonal_KJ,5,8) == "2912" ~ "0912",
    is.na(CID_82) & str_sub(Seasonal_KJ,5,8) == "2110" ~ "2110",
    is.na(CID_82) & str_sub(Seasonal_KJ,5,8) == "3008" ~ "2308",
    is.na(CID_82) & str_sub(Seasonal_KJ,5,8) == "3015" ~ "2305",
    !is.na(CID_82) ~ CID_82
  ))

saveRDS(LFSCKJ,"Data/County_Division/Cleaned Data/LFSCKJ.RDS")
