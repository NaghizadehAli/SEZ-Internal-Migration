rm(list=ls())
library(tidyverse)
library(readxl)
library(tibble)

################################################################################
                            # Create FORM3 Total Data set #
################################################################################

FORM3_Total <- tibble()

for (year in 84:98) {
  FORM3 <- readRDS(paste0("F:/LFS/Processed data/",year,"/FORM3.RDS"))
  
  FORM3[FORM3 == -1] <- NA
  
  FORM3 <- FORM3%>%
    mutate(Employment = ifelse(F3_D01 == "Yes"|F3_D02 == "Yes"|F3_D03 == "Yes"|F3_D06 == "Yes"|F3_D08 == "Yes","Employed",NA))%>%
    mutate(Employment = ifelse(is.na(Employment) & F3_D31 == "Yes" & F3_D34 == "Yes","Unemployed",Employment))%>%
    mutate(Employment = ifelse(is.na(Employment) & F3_D31 == "No"  & F3_D33 %in% c("First","Secound") & F3_D34 == "Yes","Unemployed",Employment))%>%
    mutate(Employment = ifelse(is.na(Employment),"Inactive",Employment))%>%
    mutate(Inactive = case_when(
      Employment == "Inactive" & F3_D47 == "Studying"  ~ "Student",
      Employment == "Inactive" & F3_D47 == "housewife" ~ "Housekeeper",
      Employment == "Inactive" & F3_D47 %in% c("Non-retired","Retired") ~ "Income without a job",
      Employment == "Inactive" & F3_D47 == "Others" ~ "Others"
    ))%>%
    mutate(Ex = ifelse(Employment=="Employed",F3_D14SAL*12+F3_D14MAH,NA))%>%
    mutate(Ex_T = case_when(
      Employment == "Employed" ~ F3_D15SAL*12 + F3_D15MAH,
      F3_D36 == "Working" ~ F3_D37SAL*12 + F3_D37MAH
    ))%>%
    mutate(JSD = ifelse(Employment == "Unemployed" , F3_D35SAL*12+F3_D35MAH,NA))%>%
    mutate(Employment= factor(Employment,levels = c("Employed","Unemployed","Inactive")))%>%
    mutate(Inactive = factor(Inactive,levels = c("Student","Housekeeper","Income without a job","Others")))%>%
    mutate(Incomplete_Employment = case_when(
      Employment == "Employed" & F3_D16SHHAMSA < 44 & F3_D21 == "Yes" & F3_D22 == "Yes" ~ "Yes",
      Employment == "Employed" & F3_D16SHHAMSA < 44 & (F3_D21 == "No" | F3_D22 == "No") ~ "No",
      Employment == "Employed" & F3_D16SHHAMSA >=44 ~ "No"
    ))%>%
    mutate(Incomplete_Employment = factor(Incomplete_Employment,levels = c("Yes","No")))
  
  FORM3 <- FORM3%>%
    select(Pkey,Employment,Inactive,Ex,Ex_T,JSD,F3_D08,F3_D09,F3_D11,F3_D12,
           F3_D13,F3_D16SHHAMSA,Incomplete_Employment,F3_D24,F3_D38)%>%
    dplyr::rename("Job_Status" = "F3_D11",
                  "N_Workers"  = "F3_D12",
                  "Insurance"  = "F3_D13",
                  "WHours_T"   = "F3_D16SHHAMSA",
                  "Other_JS"   = "F3_D24",
                  "UI"         = "F3_D38" )
  
  
  FORM3_Total <- bind_rows(FORM3_Total,FORM3)
}

################################################################################
                    # Create FROM2 Total Data set #
################################################################################


FORM2_Total <- tibble()

for (year in 84:98) {
  FORM2 <- readRDS(paste0("F:/LFS/Processed data/",year,"/FORM2JOZ.RDS"))
  
  FORM2[FORM2 == -1] <- NA
  
  FORM2 <- FORM2 %>%
    mutate(Migrant = case_when(
      F2_D11 %in% c("Other places Same City","NR") ~ "No",
      is.na(F2_D11) ~ NA_character_,
      TRUE~"Yes"
    ))%>%
    mutate(Provincial_Migrant = case_when(
      F2_D11 %in% c("Other City Other Province","Other Village Other Province") ~ "Yes",
      is.na(F2_D11) ~ NA_character_ ,
      TRUE ~ "No"))%>%
    select(Pkey,Relation,Gender,Age,Nationality,F2_D13,Student,
           Literacy,Degree,Marriage_Status,Migrant,Provincial_Migrant)%>%
    mutate(Migrant = factor(Migrant,levels = c("Yes","No")))%>%
    mutate(Provincial_Migrant = factor(Provincial_Migrant,levels = c("Yes","No")))
  
  FORM2_Total <- bind_rows(FORM2_Total,FORM2)
}

################################################################################
                          # Create W3 Total Data set #
################################################################################

W3_Total <- tibble()

County <- readRDS("F:/LFS/Processed data/County_ID.RDS")

County <- County%>%
  select(HHID,County_ID_23)%>%
  rename("County_ID" = "County_ID_23")

for (year in 84:98) {
  W3 <- readRDS(paste0("F:/LFS/Processed data/",year,"/W3.RDS"))
  
  W3 <- W3%>%
    select(-Shahrestan)%>%
    left_join(County,by = "HHID")%>%
    mutate(Seasonal_KJ = paste0(Year,Season,Province_ID,County_ID))%>%
    select(-HHID,-IID,-County_ID)
  
  W3_Total <- bind_rows(W3_Total,W3)
}

################################################################################
                        # Create LFS Total Data set #
################################################################################
rm(FORM2,FORM3,W3)

LFS_Total <- W3_Total%>%
  left_join(FORM2_Total,by = "Pkey")%>%
  left_join(FORM3_Total,by = "Pkey")


rm(FORM2_Total,FORM3_Total,W3_Total)


County_82 <- read_xlsx("Data/County_Division/Cleaned Data/Final_County_ID.xlsx",sheet = "82")

County_82 <- County_82%>%
  dplyr::rename("CID_82" = "County_ID")%>%
  select(CID_82,County)

LFSCKJ <- readRDS("Data/County_Division/Cleaned Data/LFSCKJ.RDS")

LFSCKJ <- LFSCKJ%>%
  left_join(County_82,by = "CID_82")


LFS_Total <- LFS_Total%>%
  mutate(Province_ID = ifelse(Province_ID=="30","23",Province_ID))%>%
  mutate_at(vars(Season,Province_ID,Shahrestan),as.factor)%>%
  left_join(LFSCKJ,by = "Seasonal_KJ")

library("rio")
export(LFS_Total,"Data/LFS_Total/LFS_Total.dta")
saveRDS(LFS_Total,"Data/LFS_Total/LFS_Total.RDS")

