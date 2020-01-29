# Working Directory:
setwd("/schaeffer-a/sch-projects/dua-data-projects/Teladoc/kmkaiser/")

## File paths:
icd10_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/ICD_VALUE_SET_LINDER.csv"
ndc_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/NDC_list.csv"
regions_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/Region_list.csv"

TD_ndc_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/kmkaiser/Value_Sets/Antibiotic_All.csv"
TD_icd10_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/kmkaiser/Value_Sets/Diagnosis_TDH Reporting_2019-12-18.csv"

## Pkgs and logs:
library.dynam(tidyverse)
library.dynam(lubridate)
#sink("./Aim1A_Summary_Tables_Log.txt")

##############
ICD_ValueSet <- read.csv(icd10_path)
TD_ICD_ValueSet <- read.csv(TD_icd10_path)
# Compare list of ARI-ICDs between Linder List and TD List
merged_icd_VS <- full_join(ICD_ValueSet, TD_ICD_ValueSet, by=c("diagnosis_cd"="encrypt_diagnosis_cd"), suffix=c(".l",".td"))
merged_icd_VS1 <- mutate(merged_icd_VS, 
                         in_TD=if_else(is.na(encrypt_diagnosis_nm),0,1),
                         in_Linder=if_else(is.na(flag),0,1))

# Overlap Table
merged_icd_VS1 %>%
  group_by(flag) %>%
  summarise(count_linder=sum(in_Linder), count_td=sum(in_TD), mean_td=mean(in_TD))

#Output Merged Data
write.csv(merged_icd_VS1, "ICD_Merged.csv")

##############
NDC_ValueSet <- read.csv(ndc_path)
TD_NDC_ValueSet <- read.csv(TD_ndc_path)
# Compare list of abx NDCs between this list and TD list, and names of drugs that are on the extract
merged_NDC_VS <- full_join(NDC_ValueSet, TD_NDC_ValueSet, by=c("NDC"="ndc_name"), suffix=c(".l",".td"))
merged_NDC_VS1 <- mutate(merged_NDC_VS, 
                         in_TD=if_else(is.na(brand_name),0,1),
                         in_Linder=if_else(is.na(is_abx),0,1))
# Overlap Table
merged_NDC_VS1 %>%
  group_by(route_desc) %>%
  summarise(count_linder=sum(in_Linder), count_td=sum(in_TD), mean_linder=mean(in_Linder))

# Most abx on Linder's list are oral drugs
##############
Region_ValueSet <- read.csv(regions_path)

# only select region codes by-state
Region_ValueSet=subset(Region_ValueSet,state.abbreviation!="",select=c(Region,state.abbreviation))
#whichever the final value set ends up being
saveRDS(merged_icd_VS1, file = "ICD_ValueSet.rds")
#saveRDS(NDC_ValueSet, file = "NDC_ValueSet.rds")
saveRDS(Region_ValueSet, file = "Region_ValueSet.rds")
