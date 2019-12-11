## Working Directory:
setwd("/schaeffer-a/sch-projects/dua-data-projects/Teladoc/kmkaiser/")

## File paths:
fakedata_path="/schaeffer-a/sch-data-library/dua-data/TelaDoc/Original_data/Data/Sample_File-Consult Details_Fake Data_2019-11-20.csv"
icd10_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/ICD_VALUE_SET_LINDER.csv"
ndc_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/NDC_list.csv"
regions_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/Region_list.csv"

## Pkgs and logs:
install.packages("tidyverse")
install.packages("lubridate")
sink("./Aim1A_Summary_Tables_Log.txt")
##############

## Load Data Frames and Clean:
ConsultDetails <- read.csv(fakedata_path)

# Convert datetime vars
mutate(ConsultDetails, 
	DOB_dt=mdy(DOB), 
	Consult_Request_dt=mdy_hm(Consult_Request_Date), 
	Consult_Start_dt=mdy_hm(Consult_Start_Date_Time), 
	Consult_End_dt=mdy_hm(Consult_End_Date_Time), 
	MHD_Completion_dt=mdy_hm(MHD_Completion_Date_Time), 
	Called_Back_tm=hms(Called_Back_Time)
) 
# Create time-related derived vars
mutate(ConsultDetails, 
	Age_At_Consult=years(as_date(Consult_Start_dt)-DOB_dt), 
	WaitTime_Mins=(Consult_Start_dt-Consult_Request_dt)/60, 
	ConsultDuration_Mins=(Consult_End_dt-Consult_Start_dt)/60, 
	Consult_Month=month(Consult_Start_dt),
	Consult_Quarter=quarter(Consult_Start_dt)
)

## Summary Tables
# Frequency of Cancellation Codes - figure out counts

CancelCodes <- group_by(Cancelled_Consult_Reason_Code, Cancelled_Consult_Reasons_Description

ICD_ValueSet <- read.csv(icd10_path)
NDC_ValueSet <- read.csv(ndc_path)
Region_ValueSet <- read.csv(regions_path)

# only select region codes by-state
Region_ValueSet=subset(Region_ValueSet,state.abbreviation!="",select=c(Region,state.abbreviation))
