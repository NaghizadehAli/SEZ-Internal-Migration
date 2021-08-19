rm(list=ls())

library(tidyverse)
library(dplyr)
library(readxl)

################################################################################
    # Counting Number of LFS Counties in each Province During years 84:98 #
################################################################################
County_ID <- readRDS("F:/LFS/Processed data/County_ID.RDS")

County_ID <- County_ID%>%
  select(HHID,County_ID_23)%>%
  rename("Shahrestan" = "County_ID_23")

LFS_C_N <- tibble(Province_ID = c("00","01","02","03","04","05","06","07","08",
                                  "09","10","11","12","13","14","15","16","17",
                                  "18","19","20","21","22","23","24","25","26",
                                  "27","28","29","30")) # Number of LFS Counties 

for (Year in 84:98) {
  W3 <- readRDS(paste0("F:/LFS/Processed data/",Year,"/W3.RDS"))
  
  W3 <- W3%>%
    select(HHID,Season,Province_ID)%>%
    left_join(County_ID,by = "HHID")%>%
    distinct(Season,Province_ID,Shahrestan,.keep_all = T)%>%
    group_by(Season,Province_ID)%>%
    summarise(N = n())%>%
    ungroup(Season)
  
  
  S1 <- W3%>%
    filter(Season == "01")%>%
    select(-Season)
  colnames(S1)[2] <-  paste0(Year,"01")
  
  S2 <- W3%>%
    filter(Season == "02")%>%
    select(-Season)
  colnames(S2)[2] <-  paste0(Year,"02")
  
  S3 <- W3%>%
    filter(Season == "03")%>%
    select(-Season)
  colnames(S3)[2] <-  paste0(Year,"03")
  
  S4 <- W3%>%
    filter(Season == "04")%>%
    select(-Season)
  colnames(S4)[2] <-  paste0(Year,"04")
  
  LFS_C_N <- LFS_C_N%>%
    left_join(S1,by = "Province_ID")%>%
    left_join(S2,by = "Province_ID")%>%
    left_join(S3,by = "Province_ID")%>%
    left_join(S4,by = "Province_ID")
    
  
}

################################################################################
# Counting Number of  Counties in each Province According to Country division #
                               # Years 84:98  #
################################################################################
sheets <- excel_sheets("Data/County_Division/Cleaned Data/Final_County_ID.xlsx")
Division_C_N <- tibble(Province_ID = LFS_C_N$Province_ID)

for (i in sheets) {
  
  FCID <- read_xlsx("Data/County_Division/Cleaned Data/Final_County_ID.xlsx",
                               sheet = i) # Final County ID , County Division
  
  FCID <- FCID%>%
    select(County_ID,Province,County)%>%
    mutate(Province_ID = str_sub(County_ID,1,2))%>%
    group_by(Province)%>%
    dplyr::mutate(N=n())%>%
    ungroup(Province)%>%
    distinct(Province,.keep_all = T)%>%
    select(Province_ID,N)
  
  colnames(FCID)[colnames(FCID) == "N"] <- i
    
  Division_C_N <- Division_C_N%>%
    left_join(FCID,by = "Province_ID")
}


################################################################################
                  # Counting Number of  Missing Counties#
                              # Years 84:98  #
################################################################################

LFS_MC <- tibble(Province_ID = LFS_C_N$Province_ID)

for (Year in 84:98) {
  W3 <- readRDS(paste0("F:/LFS/Processed data/",Year,"/W3.RDS"))
  
  W3 <- W3%>%
    select(HHID,Season,Province_ID)%>%
    left_join(County_ID,by = "HHID")%>%
    distinct(Season,Province_ID,Shahrestan,.keep_all = T)%>%
    group_by(Season,Province_ID)%>%
    dplyr::mutate(Missing_Counties = sum(is.na(Shahrestan)))%>%
    distinct(Season,Province_ID,Missing_Counties)%>%
    ungroup(Season)
  
  S1 <- W3%>%
    filter(Season == "01")%>%
    select(-Season)
  colnames(S1)[2] <-  paste0(Year,"01")
  
  S2 <- W3%>%
    filter(Season == "02")%>%
    select(-Season)
  colnames(S2)[2] <-  paste0(Year,"02")
  
  S3 <- W3%>%
    filter(Season == "03")%>%
    select(-Season)
  colnames(S3)[2] <-  paste0(Year,"03")
  
  S4 <- W3%>%
    filter(Season == "04")%>%
    select(-Season)
  colnames(S4)[2] <-  paste0(Year,"04")
  
  LFS_MC <- LFS_MC%>%
    left_join(S1,by = "Province_ID")%>%
    left_join(S2,by = "Province_ID")%>%
    left_join(S3,by = "Province_ID")%>%
    left_join(S4,by = "Province_ID")
  
}

LFS_MC <- LFS_MC %>%
  mutate(Sum = rowSums(.[2:61],na.rm = TRUE))%>%
  filter(Sum != 0)%>%
  select(-Sum)
  
LFS_MC <- LFS_MC[, colSums(LFS_MC != 0) > 0]


#select(which(colSums(.) > 0))
#bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))





