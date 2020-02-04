# Working Directory:
setwd("/schaeffer-a/sch-projects/dua-data-projects/Teladoc/kmkaiser/")

## File paths:
regions_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/Region_list.csv"
TD_ndc_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/kmkaiser/Value_Sets/Antibiotic_All.csv"
fakedata_path="/schaeffer-a/sch-data-library/dua-data/TelaDoc/Original_data/Data/Sample_File-Consult Details_Fake Data_2019-11-20.csv"
ICD_pathfinal="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/kmkaiser/ICD_Merged.csv"

## Pkgs and logs:
library.dynam(tidyverse)

###########
#Read in merged ICD list - this will contain only icd10, ARI flag, and final antibiotic appropriateness flag
ICD_Merged <- read.csv(ICD_pathfinal)

#Read in clean and stacked consults data - will be done with read RDS in actual code
ConsultDetails1 <- read.csv(fakedata_path)

#Merge on ARI flag with abx appropriate/inappropriate to diagnoses
ConsultDetails2 <- inner_join(ConsultDetails1,ICD_Merged,by=c("Diagnosis_1"="diagnosis_cd"),suffix=c("consults","icd"))



#Read in antibiotic list
NDC <- read.csv(TD_ndc_path)

#extract consults with a prescription and turn them long
ConsultDetails2 %>%
  filter(!is.na(Prescription_1)) %>%
  select(Consult_ID,Prescription_1,Prescription_2,Prescription_3) %>% #add additional as needed
  gather("Prescription","Prescription_Num",2:4) %>% #add additional columns as needed
  (~ Consult_Prescriptions) #save long table with multiple rows per consult

#separate out drug name and merge on antibiotic list with tallman lettering
Consult_Prescriptions %>%
  separate(Prescription, into = c("Prescription_Name", "Prescription_Details"), sep="[0-9]") %>% #regular expression
  inner_join(NDC,by=c("Prescription_Name"="drug_name") %>% 
  (~ Consult_Prescriptions_abx)
  
#flag and keep only consults with abx
#merge back to main consult list by consult_id


  
           
