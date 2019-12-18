## Working Directory:
setwd("/schaeffer-a/sch-projects/dua-data-projects/Teladoc/kmkaiser/")

## File paths:
fakedata_path="/schaeffer-a/sch-data-library/dua-data/TelaDoc/Original_data/Data/Sample_File-Consult Details_Fake Data_2019-11-20.csv"
icd10_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/ICD_VALUE_SET_LINDER.csv"
ndc_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/NDC_list.csv"
regions_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/Region_list.csv"

## Pkgs and logs:
library.dynam(tidyverse)
library.dynam(lubridate)
#sink("./Aim1A_Summary_Tables_Log.txt")
##############

## Load Data Frames and Clean:
ConsultDetails <- read.csv(fakedata_path)

# Convert datetime vars and time-related derived vars
ConsultDetails1 <- mutate(ConsultDetails, 
    DOB_dt=mdy(DOB), 
    Consult_Request_dt=mdy_hm(Consult_Request_Date), 
    Consult_Start_dt=mdy_hm(Consult_Start_Date_Time), 
    Consult_End_dt=mdy_hm(Consult_End_Date_Time), 
    MHD_Completion_dt=mdy_hm(MHD_Completion_Date_Time), 
    Called_Back_tm=hms(Called_Back_Time),
    Age_At_Consult=years(as_date(Consult_Start_dt)-DOB_dt), 
    WaitTime_Mins=(Consult_Start_dt-Consult_Request_dt)/60, 
    ConsultDuration_Mins=(Consult_End_dt-Consult_Start_dt)/60, 
    Consult_Month=month(Consult_Start_dt),
    Consult_Quarter=quarter(Consult_Start_dt)
)

## Summary Tables
# Frequency of Cancellation Codes
ConsultDetails1 %>%
  group_by(Cancelled_Consult_Reason_Code, Cancelled_Consult_Reasons_Description) %>%
  count(Cancelled_Consult_Reason_Code)

# Frequency of Consult Alternative Code/Desc
ConsultDetails1 %>%
  group_by(Consult_Alternative_Code, Consult_Alternative_Description) %>%
  count(Consult_Alternative_Code)

# Freq of Consult Type by Method of Consult Request
consult_type_request <- table(ConsultDetails1$Consult_Type, ConsultDetails1$Method_of_Consult_Request) 
margin.table(consult_type_request,1)
margin.table(consult_type_request,2)
prop.table(consult_type_request,1)
prop.table(consult_type_request,2)

# Summarize Wait Time and Duration
ConsultDetails1 %>% 
  summarise(mean_wait = mean(WaitTime_Mins, na.rm = TRUE),
            min_wait = min(WaitTime_Mins, na.rm = TRUE),
            max_wait = max(WaitTime_Mins, na.rm = TRUE),
            mean_duration = mean(ConsultDuration_Mins, na.rm = TRUE),
            min_duration = min(ConsultDuration_Mins, na.rm = TRUE),
            max_duration = max(ConsultDuration_Mins, na.rm = TRUE),
)

qplot(ConsultDetails1$WaitTime_Mins,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram Wait Time"
)
# Summary Wait Time and Duration By Season

##########
ICD_ValueSet <- read.csv(icd10_path)
# Compare list of ARI-ICDs between Linder List and TD List,
# Create Table of Dx Codes and Descriptions to compare below

NDC_ValueSet <- read.csv(ndc_path)
# Compare list of abx NDCs between this list and TD list, and names of drugs that are on the extract
# broad spectrum vs all abx 

Region_ValueSet <- read.csv(regions_path)

# only select region codes by-state
Region_ValueSet=subset(Region_ValueSet,state.abbreviation!="",select=c(Region,state.abbreviation))
