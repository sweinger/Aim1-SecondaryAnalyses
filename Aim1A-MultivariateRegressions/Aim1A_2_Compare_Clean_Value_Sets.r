# Working Directory:
setwd("/schaeffer-a/sch-projects/dua-data-projects/Teladoc/kmkaiser/")

## File paths:
icd10_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/ICD_VALUE_SET_LINDER.csv"
ndc_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/NDC_list.csv"
regions_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/Region_list.csv"

## Pkgs and logs:
library.dynam(tidyverse)
library.dynam(lubridate)
#sink("./Aim1A_Summary_Tables_Log.txt")

##############
ICD_ValueSet <- read.csv(icd10_path)
# Compare list of ARI-ICDs between Linder List and TD List,
# Create Table of Dx Codes and Descriptions to compare below

NDC_ValueSet <- read.csv(ndc_path)
# Compare list of abx NDCs between this list and TD list, and names of drugs that are on the extract
# broad spectrum vs all abx 

Region_ValueSet <- read.csv(regions_path)

# only select region codes by-state
Region_ValueSet=subset(Region_ValueSet,state.abbreviation!="",select=c(Region,state.abbreviation))
#whichever the final value set ends up being
saveRDS(ICD_ValueSet, file = "ICD_ValueSet.rds")
saveRDS(NDC_ValueSet, file = "NDC_ValueSet.rds")
saveRDS(Region_ValueSet, file = "Region_ValueSet.rds")
