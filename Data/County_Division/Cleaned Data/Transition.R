
rm(list = ls())
library(tidyverse)
library(dplyr)
library(readxl)

################################################################################
                  # Transition Data set with County ID #
################################################################################

Diff_Sheet <- excel_sheets("Data/County_Division/Cleaned Data/Diff_County.xlsx")

Transition_I <- read_xlsx("Data/County_Division/Cleaned Data/Final_County_ID.xlsx",
                        sheet = "98")
Transition_I <- Transition_I%>%
  select(County_ID)

for (i in 1:(length(Diff_Sheet)-1)) {
  
  DiFF <- read_xlsx("Data/County_Division/Cleaned Data/Diff_County.xlsx",
                    sheet = Diff_Sheet[i])
  DiFF <- DiFF%>%
    select(County_ID,Previous_C_ID)%>%
    mutate_all(as.character)
  
  Transition_I <- Transition_I%>%
    left_join(DiFF,by = "County_ID")%>%
    mutate(Previous_C_ID = ifelse(is.na(Previous_C_ID),County_ID,Previous_C_ID))
  
  colnames(Transition_I)[colnames(Transition_I) == "County_ID"] <- paste0("CID_",str_sub(Diff_Sheet[i],1,2))
  colnames(Transition_I)[colnames(Transition_I) == "Previous_C_ID"] <- "County_ID"
  
}

Transition_I <- Transition_I%>%
    mutate(Equal = case_when(
        CID_98 == CID_97 & CID_97 == CID_96 & CID_96 == CID_95 & CID_95 == CID_94 & 
        CID_94 == CID_93 & CID_93 == CID_92 & CID_92 == CID_91 & CID_91 == CID_90 & 
        CID_90 == CID_89 & CID_89 == CID_88 & CID_88 == CID_85 & CID_85 == CID_82 &
        CID_82 == County_ID ~ 1,
        TRUE ~ 0
    ))

New_County_I <- Transition_I%>%
  filter(Equal == 0)

Year <- c("98","97","96","95","94","93","92","91","90","89","88","85","82")

TRTo82_I <- tibble()
  
for (i in 1:13) {
  
  TR <- Transition_I%>%
    select(eval(i),13)%>%
    mutate(Year = Year[i])
  
  colnames(TR)[1] <- "C_ID"
  
  TRTo82_I <- bind_rows(TRTo82_I,TR)
}

TRTo82_I <- TRTo82_I%>%
  mutate(CID_82 = ifelse(is.na(CID_82) & Year == 82 ,C_ID,CID_82))

TRTo82_I <- TRTo82_I%>%
  select(Year,everything())

saveRDS(TRTo82_I,"Data/County_Division/Cleaned Data/TRTo82.RDS")
################################################################################
                  # Transition Data set with County  #
################################################################################

Diff_Sheet <- excel_sheets("Data/County_Division/Cleaned Data/Diff_County.xlsx")

Transition_C <- read_xlsx("Data/County_Division/Cleaned Data/Final_County_ID.xlsx",
                        sheet = "98")
Transition_C <- Transition_C%>%
  select(County)

for (i in 1:(length(Diff_Sheet)-1)) {
  
  DiFF <- read_xlsx("Data/County_Division/Cleaned Data/Diff_County.xlsx",
                    sheet = Diff_Sheet[i])
  DiFF <- DiFF%>%
    select(County,Previous_County)%>%
    mutate_all(as.character)
  
  Transition_C <- Transition_C%>%
    left_join(DiFF,by = "County")%>%
    mutate(Previous_County = ifelse(is.na(Previous_County),County,Previous_County))
  
  colnames(Transition_C)[colnames(Transition_C) == "County"] <- paste0("County_",County_Sheet[i])
  colnames(Transition_C)[colnames(Transition_C) == "Previous_County"] <- "County"
  
}


Transition_C <- Transition_C%>%
  mutate(Equal = case_when(
      County_98 == County_97 & County_97 == County_96 & County_96 == County_95 & County_95 == County_94 & 
      County_94 == County_93 & County_93 == County_92 & County_92 == County_91 & County_91 == County_90 & 
      County_90 == County_89 & County_89 == County_88 & County_88 == County_85 & County_85 == County_82 &
      County_82 == County ~ 1,
    TRUE ~ 0
  ))

New_County_C <- Transition_C%>%
  filter(Equal == 0)

Year <- c("98","97","96","95","94","93","92","91","90","89","88","85","82","81")

TRTo81_C <- tibble()

for (i in 1:13) {
  
  TR <- Transition_C%>%
    select(eval(i),14)%>%
    mutate(Year = Year[i])
  
  colnames(TR)[colnames(TR) == "County"] <- "County_81"
  colnames(TR)[1] <- "County"
  
  
  TRTo81_C <- bind_rows(TRTo81_C,TR)
}

A <- TRTo81_C%>%
  distinct(County,County_81)
B <- TRTo81_I%>%
  distinct(C_ID,CID_81)
