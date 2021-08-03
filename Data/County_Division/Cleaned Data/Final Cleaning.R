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

# Find New County in Years

B <- list()

year<- c("98","97","96","95","94","93","92","91","90","89","88","85","82","81","76")


for (i in 1:(length(year)-1)) {
  
  GEO2 <- read_xlsx("G:/Github/SEZ-Internal-Migration/Data/County_Division/Cleaned Data/Final_County_ID.xlsx",sheet = year[i])
  GEO2 <- GEO2%>%
    select(County_ID,Province,County)
  
  GEO1 <- read_xlsx("G:/Github/SEZ-Internal-Migration/Data/County_Division/Cleaned Data/Final_County_ID.xlsx",sheet = year[i+1])
  GEO1 <- GEO1%>%
    select(County_ID,Province,County)
  
  diff <- anti_join(GEO2,GEO1)
  
  B[paste0(year[i],"-",year[i+1])] <- list(diff)
  
} 

write_xlsx(B,"G:/Github/SEZ-Internal-Migration/Data/County_Division/Cleaned Data/Diff_County.xlsx")
