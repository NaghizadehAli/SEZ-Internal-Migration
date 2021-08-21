rm(list = ls())
library(readxl)
library(tidyverse)
library(writexl)
getwd()

CIDY <- excel_sheets("Data/County_Division/Cleaned Data/Final_County_ID.xlsx") # County ID Years

County <- readRDS("F:/LFS/Processed data/County_ID.RDS")

County <- County%>%
  select(HHID,County_ID_23)%>%
  rename("County_ID" = "County_ID_23")

County_ID <- tibble()

for (i in CIDY) {
  C_ID <- read_xlsx("Data/County_Division/Cleaned Data/Final_County_ID.xlsx",
                    sheet = i)
  
  C_ID <- C_ID%>%
    mutate(Year =eval(i))
  
  County_ID <- bind_rows(County_ID,C_ID)
}

County_ID <- County_ID%>%
  arrange(Year,County_ID)%>%
  select(-Ostan,-Shahrestan)%>%
  distinct(County_ID,.keep_all = T)

W3_TC <- tibble() # W3 total coutines

for (Year in 84:98) {
  
  W3 <-  readRDS(paste0("F:/LFS/Processed data/",Year,"/W3.RDS"))
  
  W3 <- W3%>%
    select(HHID,Year,Season,Province_ID)%>%
    left_join(County,by = "HHID")%>%
    mutate(County_ID = paste0(Province_ID,County_ID))%>%
    left_join(County_ID,by = "County_ID")
  
  W3_TC <- bind_rows(W3_TC,W3)
}

D_Y<- W3_TC %>%
  mutate_at(vars(Year.y),as.integer)%>%
  mutate_at(vars(Year.x,Season),as.factor)%>%
  group_by(Year.x,Season)%>%
  filter(County_ID != "2911")%>%
  summarise(Division_Year = max(Year.y,na.rm = TRUE))%>%
  rename("Year"="Year.x")%>%
  distinct(Year,Division_Year)

write_xlsx(D_Y,"Data/County_Division/Cleaned Data/Division_Year.xlsx")
