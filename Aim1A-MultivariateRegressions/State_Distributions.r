## Working Directory:
setwd("/schaeffer-a/sch-projects/dua-data-projects/Teladoc/kmkaiser/")

## File paths:
ori_path="/schaeffer-a/sch-data-library/dua-data/TelaDoc/Original_data/Data/usc_consults_20190220.csv"
fakedata_path="/schaeffer-a/sch-data-library/dua-data/TelaDoc/Original_data/Data/Sample_File-Consult Details_Fake Data_2019-11-20.csv"

#Old data is ARI only, prescriptions are abx only - multiple rows per consult
olddata <- read.csv(ori_path)

#roll up by consult, prescription, state
olddata_roll <- olddata %>%
  group_by(consult_key, NPI, consult_state_cd) %>%
  summarise(abx_count = sum(!is.na(ndc)))

#Visit count by state
freq_bystate <- olddata_roll %>%
  group_by(consult_state_cd) %>%
  summarise(visitcount=n(), visitcount_abx=sum(abx_count)) %>%
  mutate(prescribing_rate=visitcount_abx/visitcount) 

qplot(x=visitcount, y=prescribing_rate, data=freq_bystate, geom="point")
qplot(x=visitcount, data=freq_bystate, geom="histogram")
qplot(x=prescribing_rate, data=freq_bystate, geom="histogram")

#Provider count by state
prov_bystate <- olddata_roll %>%
  group_by(consult_state_cd) %>%
  summarise(provcount=n_distinct(NPI)) 

qplot(x=provcount, data=prov_bystate, geom="histogram")

#Intracluster correlation - subject to change, taking too long
iccbin(consult_state_cd, abx_count, data=olddata_roll, method = c("aov"))
print.iccbin
