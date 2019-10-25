# AHRQ-Teladoc Project Aim 1 R Program
The program clinician_stratification.R creates a list of providers with characteristics relevant for selecting interviewees. 

## Getting Started
These instructions will get you a copy of the project on your local system. 

### Prerequisites
* Create .csv sample of consultations on local system. Data warehouse extraction SQL should be modified to include the most recent year of data.
* Clone or download directory containing 3 subfolders: /Program, /Value_Sets, and /Output.

### Updating Directory Paths
* Update line 1 to reflect directory location:
```
setwd(“S:/dirlocation/”)
```
* Update line 2 to reflect consultation data location and file name:
```
ori_path=“S:/datalocation/consults.csv”
```
### Run Code in Linux Environment
* Rscript --save filename.R
* Example: 
```
Rscript --save /server/user/sample_code.R
```
A stable connection is required while the code is running. Time to completion depends on the size of the data and server's RAM utilizable by the user.

## Handling Output
This program creates two .csv lists that are unique by NPI and provider_durable key:
* table_sat.csv: List of providers based on consults with satisfaction scores
  * Stratified by inappropriate antibiotic prescriptions, visit counts, and mean satisfaction score (recoded numerically)
* table_all.csv: List of providers based on all consults in sample
  * Stratified by inappropriate antibiotic prescription and visit count

Research team will use table_sat.csv for inviting potential interviewees. Two lists are output because there many missing satisfaction scores, and Teladoc may choose to evaluate or compare both lists further. 

The final list will be cut based on the strata of interest (to choose providers with relatively high/low amounts of the given measures, and not medium amounts). The Teladoc analysts will merge table_sat.csv to provider contact information in order to create the potential interviewee list. 

## Post Processing to merge Provider Name and Contact Email
After generating this output, implementer must JOIN on Provider ID local provider information.

The list will be uploaded to Google Drive as a Google Sheet, where Tara and Teladoc project managers will add columns used to manage who is eligible, contacted, and selected for interviewing.

Data dictionary with original Teladoc sample and derived variables: https://docs.google.com/spreadsheets/d/17GMMgbmVEjoj1ZdDLB63BXm-zSaXOwJW8oCQATNgV_E/edit#gid=1645317766&range=A1




## Authors
* @zhongjic

### Acknowledgments
* @daniellameeker
* @tara-knight
* @kmkaiser


