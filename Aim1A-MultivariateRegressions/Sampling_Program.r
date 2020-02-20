# Cut Sample Data to send to USC for program checking

# read in csvs - update the path parameter to reflect location of consult csvs - maybe a subfolder
ConsultDetails_all <- 
  list.files(path=, pattern = "*.csv") %>% 
  map_dfr(~read_csv(.)) %>%
  filter(Product=="GENMEDICAL")



#Read in merged ICD list - update path parameter
ICD_Merged <- read.csv()

#Merge on ARI flag with abx appropriate/inappropriate to diagnoses
#this file includes everything on both Jeff's and TD's lists, including some non-ARIs
ConsultDetails_icd <- left_join(ConsultDetails,ICD_Merged,by=c("Diagnosis_1"="diagnosis_cd"),suffix=c("consults","icd"))

#sample some cancellations
Cancellations <- 
ConsultDetails_icd %>%
  filter(Cancelled_Consult=="Y") %>%
  sample_n(50)

#sample mostly ARIs with prescription
ARI_Prescription <- 
ConsultDetails_icd %>%
  filter(!is.na(diagnosis_cd) & !is.na(Prescription_1)) %>%
  sample_n(500)

#sample mostly ARIs with no prescription
ARI_NoDrug <- 
ConsultDetails_icd %>%
  filter(!is.na(diagnosis_cd) & is.na(Prescription_1)) %>%
  sample_n(250)

#sample none of the above
OtherConsults <- 
ConsultDetails_icd %>%
  filter(is.na(diagnosis_cd) & is.na(Prescription_1)) %>%
  sample_n(200)

SampleConsults <- bind_rows(Cancellations, ARI_Prescription, ARI_NoDrug, OtherConsults)

#export - specify path
write.csv(SampleConsults)
