rm(list = ls())
library(readxl)
library(writexl)
library(tidyverse)
library(stringi)

# load F_TO_E_Provice.xlsx and F_TO_E_County.xlsx files

FTOEP<- read_xlsx("G:/Github/SEZ-Internal-Migration/Data/County_Division/Cleaned Data/F_To_E_Province.xlsx")

FTOEC <- read_xlsx("G:/Github/SEZ-Internal-Migration/Data/County_Division/Cleaned Data/F_To_E_County.xlsx")

# Merge First step Cleaned file with FTOEP and FTOEC to Conver Persian Name to English one

c = c("76","81","82","85","88","89","90","91","92","93","94","95","96","97","98")

A <- list()
for (i in c) {
  
  GEO <- read_xlsx("G:/Github/SEZ-Internal-Migration/Data/County_Division/Cleaned Data/First Step Cleaning/F_County_ID.xlsx",sheet = i)
  
  GEO <- GEO%>%
    rename("County_ID"="Address")%>%
    left_join(FTOEP,by = "Ostan",suffix = c("",".x"))%>%
    left_join(FTOEC,by = "Shahrestan",suffix = c("",".x"))%>%
    select(County_ID,Ostan,Province,Shahrestan,County)

  A[i] <- list(GEO)
}

write_xlsx(A,"G:/Github/SEZ-Internal-Migration/Data/County_Division/Cleaned Data/Final_County_ID.xlsx")
