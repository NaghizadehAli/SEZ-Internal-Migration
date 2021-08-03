rm(list = ls())
library(readxl)
library(writexl)
library(tidyverse)
library(stringi)

# Importing Data and write first step Cleaning .xlsx (F_County_ID.xlsx) 
year <- c("76","81","82","85","88","89","90","91","92","93","94","95","96","97","98")

TC <- list() # Total Counties

for (i in year) {
  
  sheets<- excel_sheets(paste0("G:/Github/SEZ-Internal-Migration/Data/County_Division/Raw_data/GEO",i,".xlsx"))[-1]
  
  EYC <- tibble() # Each Year Counties
  
  if (length(sheets) > 1) {
    for (j in sheets) {
      Counties<- read_xlsx(paste0("G:/Github/SEZ-Internal-Migration/Data/County_Division/Raw_data/GEO",i,".xlsx"),sheet = j)
      
      Counties <- Counties%>%
        filter(nchar(Address)==4)%>%
        distinct(Address,.keep_all = TRUE)%>%
        select(Address,Ostan,Shahrestan)
      
      EYC <- bind_rows(EYC,Counties)
    }
  }else{
    Counties<- read_xlsx(paste0("G:/Github/SEZ-Internal-Migration/Data/County_Division/Raw_data/GEO",i,".xlsx"))
    
    EYC <- Counties%>%
      filter(nchar(Address)==4)%>%
      distinct(Address,.keep_all = TRUE)%>%
      select(Address,Ostan,Shahrestan)
    
    
  }
  
  TC[i] <- list(EYC)
}

write_xlsx(TC,"G:/Github/SEZ-Internal-Migration/Data/County_Division/Cleaned Data/First Step Cleaning/F_County_ID.xlsx")
