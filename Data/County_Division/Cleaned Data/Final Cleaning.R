rm(list = ls())
library(readxl)
library(writexl)
library(tidyverse)
library(stringi)

# load F_TO_E_Provice.xlsx and F_TO_E_County.xlsx files

FTOEP<- read_xlsx("G:/Github/SEZ-Internal-Migration/Data/County_Division/Cleaned Data/F_To_E_Province.xlsx")

FTOEC <- read_xlsx("G:/Github/SEZ-Internal-Migration/Data/County_Division/Cleaned Data/F_To_E_County.xlsx")
