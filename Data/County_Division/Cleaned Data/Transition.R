
rm(list = ls())
library(tidyverse)
library(dplyr)
library(readxl)

################################################################################
                  # Transition Data set with County ID #
################################################################################

Diff_Sheet <- excel_sheets("Data/County_Division/Cleaned Data/Diff_County.xlsx")

Transition <- read_xlsx("Data/County_Division/Cleaned Data/Final_County_ID.xlsx",
                        sheet = "98")
Transition <- Transition%>%
  select(County_ID)

for (i in 1:(length(Diff_Sheet)-1)) {
  
  DiFF <- read_xlsx("Data/County_Division/Cleaned Data/Diff_County.xlsx",
                    sheet = Diff_Sheet[i])
  DiFF <- DiFF%>%
    select(County_ID,Previous_C_ID)%>%
    mutate_all(as.character)
  
  Transition <- Transition%>%
    left_join(DiFF,by = "County_ID")%>%
    mutate(Previous_C_ID = ifelse(is.na(Previous_C_ID),County_ID,Previous_C_ID))
  
  colnames(Transition)[colnames(Transition) == "County_ID"] <- paste0("CID_",County_Sheet[i])
  colnames(Transition)[colnames(Transition) == "Previous_C_ID"] <- "County_ID"
  
}

Transition <- Transition%>%
    mutate(Equal = case_when(
        CID_98 == CID_97 & CID_97 == CID_96 & CID_96 == CID_95 & CID_95 == CID_94 & 
        CID_94 == CID_93 & CID_93 == CID_92 & CID_92 == CID_91 & CID_91 == CID_90 & 
        CID_90 == CID_89 & CID_89 == CID_88 & CID_88 == CID_85 & CID_85 == CID_82 &
        CID_82 == County_ID ~ 1,
        TRUE ~ 0
    ))

New_County <- Transition%>%
  filter(Equal == 0)

Year <- c("98","97","96","95","94","93","92","91","90","89","88","85","82","81")

TR_to_81 <- tibble()
  
for (i in 1:13) {
  
  TR <- Transition%>%
    select(eval(i),14)%>%
    mutate(Year = Year[i])
  
  colnames(TR)[1] <- "C_ID"
  colnames(TR)[colnames(TR) == "County_ID"] <- "CID_81"
  
  TR_to_81 <- bind_rows(TR_to_81,TR)
}


