#####Set up working directory
setwd("/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/")
#####Set up file paths
ori_path="/schaeffer-a/sch-data-library/dua-data/TelaDoc/Original_data/Data/usc_consults_20190220.csv"
icd10_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/ICD_VALUE_SET_LINDER.csv"
ndc_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/NDC_list.csv"
regions_path="/schaeffer-a/sch-projects/dua-data-projects/Teladoc/zhongjic/Value_Sets/Region_list.csv"
##############

test.dat=read.csv(ori_path)
ICD_10=read.csv(icd10_path)
test.dat=merge(test.dat,ICD_10,by=("diagnosis_cd"),all.x=T)

NDC=read.csv(ndc_path)
NDC_list=as.array(NDC$NDC)

regions.dat=read.csv(regions_path)
regions.dat=subset(regions.dat,state.abbreviation!="",select=c(Region,state.abbreviation))
names(regions.dat)[names(regions.dat) == 'state.abbreviation'] <- 'consult_state_cd'
#merge region and NDC
region_merge=merge(test.dat,regions.dat,by="consult_state_cd",all.x=T)

ndc_sample=region_merge$ndc
in_ndc_list=numeric(length(ndc_sample))
for (i in 1:length(ndc_sample)){
  in_ndc_list[i]=ifelse(is.element(ndc_sample[i],NDC_list),1,0)
}
region_NDC_merge=cbind(region_merge,is_abx=in_ndc_list)

#create ordinal satisfaction score
region_NDC_merge$satisfaction[region_NDC_merge$answer=="poor"]=1
region_NDC_merge$satisfaction[region_NDC_merge$answer=="good"]=2
region_NDC_merge$satisfaction[region_NDC_merge$answer=="outstanding"]=3

#subset providers, keep if there is a satisfaction score
region_NDC_merge$is_inappropriate_dx[region_NDC_merge$flag=="Abx Inappropriate" | region_NDC_merge$flag=="GB"]=1
region_NDC_merge$is_inappropriate_dx[is.na(region_NDC_merge$is_inappropriate_dx)]=0

# remove dup consult_key, prioritize is_abx=0
region_NDC_merge=region_NDC_merge[order(region_NDC_merge[,"consult_key"],-region_NDC_merge[,"is_abx"]),]
region_NDC_merge$denom=region_NDC_merge$is_inappropriate_dx
prov_sat=region_NDC_merge[!duplicated(region_NDC_merge$"consult_key"),]

names(prov_sat)[names(prov_sat) == 'is_abx'] <- 'any_abx'

inappro_rate=aggregate(prov_sat[, c("is_inappropriate_dx")], list(prov_sat$NPI), mean)
names(inappro_rate)[names(inappro_rate) == 'Group.1'] <- 'NPI'
names(inappro_rate)[names(inappro_rate) == 'x'] <- 'inapp_abx_rate'
inappro_sum=aggregate(prov_sat[, c("is_inappropriate_dx")], list(prov_sat$NPI), sum)
names(inappro_sum)[names(inappro_sum) == 'Group.1'] <- 'NPI'
names(inappro_sum)[names(inappro_sum) == 'x'] <- 'inapp_abx_sum'
sat_mean=aggregate(prov_sat[, c("satisfaction")], list(prov_sat$NPI), mean,na.rm=T)
names(sat_mean)[names(sat_mean) == 'Group.1'] <- 'NPI'
names(sat_mean)[names(sat_mean) == 'x'] <- 'sat_mean'

#merge by physician
phy_rate=merge(prov_sat,inappro_rate,by="NPI",all.x=T)
phy_rate=merge(phy_rate,inappro_sum,by="NPI",all.x=T)
phy_rate=merge(phy_rate,sat_mean,by="NPI",all.x=T)

# get inapprop quantiles
uni_phy_rate = phy_rate[!duplicated(phy_rate$NPI),]
inapprate_quantile=quantile(uni_phy_rate$inapp_abx_rate)

phy_rate$rate_strat[phy_rate$inapp_abx_rate<=as.numeric(inapprate_quantile[2])]=1
phy_rate$rate_strat[phy_rate$inapp_abx_rate<=as.numeric(inapprate_quantile[3]) & phy_rate$inapp_abx_rate>as.numeric(inapprate_quantile[2])]=2
phy_rate$rate_strat[phy_rate$inapp_abx_rate<=as.numeric(inapprate_quantile[4]) & phy_rate$inapp_abx_rate>as.numeric(inapprate_quantile[3])]=3
phy_rate$rate_strat[phy_rate$inapp_abx_rate<=as.numeric(inapprate_quantile[5]) & phy_rate$inapp_abx_rate>as.numeric(inapprate_quantile[4])]=4

# get visit quantiles
prov_freq=as.data.frame(table(phy_rate$provider_durable_key))
visit_quantile=quantile(prov_freq[,2])

# rename column for merging
colnames(prov_freq)[colnames(prov_freq)=="Var1"]="provider_durable_key"
colnames(prov_freq)[colnames(prov_freq)=="Freq"]="visit_count"
# merge visit frequency and stratify
prov_strat=merge(phy_rate,prov_freq,by="provider_durable_key",all.x=T)
# 25 50 75 100%
prov_strat$visit_strat[prov_strat$visit_count<=as.numeric(visit_quantile[2])]=1
prov_strat$visit_strat[prov_strat$visit_count<=as.numeric(visit_quantile[3]) & prov_strat$visit_count>as.numeric(visit_quantile[2])]=2
prov_strat$visit_strat[prov_strat$visit_count<=as.numeric(visit_quantile[4]) & prov_strat$visit_count>as.numeric(visit_quantile[3])]=3
prov_strat$visit_strat[prov_strat$visit_count<=as.numeric(visit_quantile[5]) & prov_strat$visit_count>as.numeric(visit_quantile[4])]=4

#create quantiles for mean satisfaction score
sat_mean_q_cut=as.numeric(quantile(prov_strat$sat_mean,na.rm=T))
prov_strat$sat_mean_q=as.numeric(cut(prov_strat$sat_mean,breaks=sat_mean_q_cut,include.lowest=T))

prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==1]=1
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==2]=2
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==3]=3
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==4]=4
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==1]=5
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==2]=6
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==3]=7
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==4]=8
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==1]=9
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==2]=10
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==3]=11
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==4]=12
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==1]=13
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==2]=14
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==3]=15
prov_strat$stratum_id[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==4]=16
#######
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==1]=17
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==2]=18
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==3]=19
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==4]=20
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==1]=21
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==2]=22
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==3]=23
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==4]=24
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==1]=25
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==2]=26
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==3]=27
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==4]=28
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==1]=29
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==2]=30
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==3]=31
prov_strat$stratum_id[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==4]=32
#######
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==1]=33
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==2]=34
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==3]=35
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==4]=36
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==1]=37
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==2]=38
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==3]=39
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==4]=40
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==1]=41
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==2]=42
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==3]=43
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==4]=44
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==1]=45
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==2]=46
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==3]=47
prov_strat$stratum_id[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==4]=48
#######
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==1]=49
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==2]=50
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==3]=51
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==4]=52
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==1]=53
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==2]=54
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==3]=55
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==4]=56
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==1]=57
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==2]=58
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==3]=59
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==4]=60
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==1]=61
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==2]=62
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==3]=63
prov_strat$stratum_id[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==4]=64

###############Labels
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==1]="inapp.q1_sat.q1_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==2]="inapp.q1_sat.q1_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==3]="inapp.q1_sat.q1_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==4]="inapp.q1_sat.q1_visit.q4"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==1]="inapp.q1_sat.q2_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==2]="inapp.q1_sat.q2_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==3]="inapp.q1_sat.q2_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==4]="inapp.q1_sat.q2_visit.q4"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==1]="inapp.q1_sat.q3_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==2]="inapp.q1_sat.q3_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==3]="inapp.q1_sat.q3_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==4]="inapp.q1_sat.q3_visit.q4"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==1]="inapp.q1_sat.q4_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==2]="inapp.q1_sat.q4_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==3]="inapp.q1_sat.q4_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==1 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==4]="inapp.q1_sat.q4_visit.q4"
#######
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==1]="inapp.q2_sat.q1_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==2]="inapp.q2_sat.q1_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==3]="inapp.q2_sat.q1_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==4]="inapp.q2_sat.q1_visit.q4"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==1]="inapp.q2_sat.q2_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==2]="inapp.q2_sat.q2_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==3]="inapp.q2_sat.q2_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==4]="inapp.q2_sat.q2_visit.q4"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==1]="inapp.q2_sat.q3_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==2]="inapp.q2_sat.q3_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==3]="inapp.q2_sat.q3_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==4]="inapp.q2_sat.q3_visit.q4"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==1]="inapp.q2_sat.q4_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==2]="inapp.q2_sat.q4_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==3]="inapp.q2_sat.q4_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==2 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==4]="inapp.q2_sat.q4_visit.q4"
#######
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==1]="inapp.q3_sat.q1_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==2]="inapp.q3_sat.q1_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==3]="inapp.q3_sat.q1_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==4]="inapp.q3_sat.q1_visit.q4"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==1]="inapp.q3_sat.q2_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==2]="inapp.q3_sat.q2_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==3]="inapp.q3_sat.q2_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==4]="inapp.q3_sat.q2_visit.q4"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==1]="inapp.q3_sat.q3_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==2]="inapp.q3_sat.q3_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==3]="inapp.q3_sat.q3_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==4]="inapp.q3_sat.q3_visit.q4"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==1]="inapp.q3_sat.q4_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==2]="inapp.q3_sat.q4_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==3]="inapp.q3_sat.q4_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==3 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==4]="inapp.q3_sat.q4_visit.q4"
#######
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==1]="inapp.q4_sat.q1_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==2]="inapp.q4_sat.q1_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==3]="inapp.q4_sat.q1_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==1 & prov_strat$visit_strat==4]="inapp.q4_sat.q1_visit.q4"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==1]="inapp.q4_sat.q2_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==2]="inapp.q4_sat.q2_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==3]="inapp.q4_sat.q2_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==2 & prov_strat$visit_strat==4]="inapp.q4_sat.q2_visit.q4"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==1]="inapp.q4_sat.q3_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==2]="inapp.q4_sat.q3_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==3]="inapp.q4_sat.q3_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==3 & prov_strat$visit_strat==4]="inapp.q4_sat.q3_visit.q4"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==1]="inapp.q4_sat.q4_visit.q1"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==2]="inapp.q4_sat.q4_visit.q2"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==3]="inapp.q4_sat.q4_visit.q3"
prov_strat$stratum_id_label[prov_strat$rate_strat==4 & prov_strat$sat_mean_q==4 & prov_strat$visit_strat==4]="inapp.q4_sat.q4_visit.q4"

#####
prov_strat$stratum_id_nosat[prov_strat$rate_strat==1 & prov_strat$visit_strat==1]=1
prov_strat$stratum_id_nosat[prov_strat$rate_strat==1 & prov_strat$visit_strat==2]=2
prov_strat$stratum_id_nosat[prov_strat$rate_strat==1 & prov_strat$visit_strat==3]=3
prov_strat$stratum_id_nosat[prov_strat$rate_strat==1 & prov_strat$visit_strat==4]=4
####
prov_strat$stratum_id_nosat[prov_strat$rate_strat==2 & prov_strat$visit_strat==1]=5
prov_strat$stratum_id_nosat[prov_strat$rate_strat==2 & prov_strat$visit_strat==2]=6
prov_strat$stratum_id_nosat[prov_strat$rate_strat==2 & prov_strat$visit_strat==3]=7
prov_strat$stratum_id_nosat[prov_strat$rate_strat==2 & prov_strat$visit_strat==4]=8
####
prov_strat$stratum_id_nosat[prov_strat$rate_strat==3 & prov_strat$visit_strat==1]=9
prov_strat$stratum_id_nosat[prov_strat$rate_strat==3 & prov_strat$visit_strat==2]=10
prov_strat$stratum_id_nosat[prov_strat$rate_strat==3 & prov_strat$visit_strat==3]=11
prov_strat$stratum_id_nosat[prov_strat$rate_strat==3 & prov_strat$visit_strat==4]=12
####
prov_strat$stratum_id_nosat[prov_strat$rate_strat==4 & prov_strat$visit_strat==1]=13
prov_strat$stratum_id_nosat[prov_strat$rate_strat==4 & prov_strat$visit_strat==2]=14
prov_strat$stratum_id_nosat[prov_strat$rate_strat==4 & prov_strat$visit_strat==3]=15
prov_strat$stratum_id_nosat[prov_strat$rate_strat==4 & prov_strat$visit_strat==4]=16
#######Labels
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==1 & prov_strat$visit_strat==1]="inapp.q1_visit.q1"
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==1 & prov_strat$visit_strat==2]="inapp.q1_visit.q2"
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==1 & prov_strat$visit_strat==3]="inapp.q1_visit.q3"
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==1 & prov_strat$visit_strat==4]="inapp.q1_visit.q4"
####
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==2 & prov_strat$visit_strat==1]="inapp.q2_visit.q1"
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==2 & prov_strat$visit_strat==2]="inapp.q2_visit.q2"
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==2 & prov_strat$visit_strat==3]="inapp.q2_visit.q3"
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==2 & prov_strat$visit_strat==4]="inapp.q2_visit.q4"
####
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==3 & prov_strat$visit_strat==1]="inapp.q3_visit.q1"
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==3 & prov_strat$visit_strat==2]="inapp.q3_visit.q2"
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==3 & prov_strat$visit_strat==3]="inapp.q3_visit.q3"
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==3 & prov_strat$visit_strat==4]="inapp.q3_visit.q4"
####
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==4 & prov_strat$visit_strat==1]="inapp.q4_visit.q1"
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==4 & prov_strat$visit_strat==2]="inapp.q4_visit.q2"
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==4 & prov_strat$visit_strat==3]="inapp.q4_visit.q3"
prov_strat$stratum_id_nosat_label[prov_strat$rate_strat==4 & prov_strat$visit_strat==4]="inapp.q4_visit.q4"

table(prov_strat$visit_strat,prov_strat$stratum_id)
table(prov_strat$sat_mean_q,prov_strat$stratum_id)
table(prov_strat$rate_strat,prov_strat$stratum_id)

table_all=subset(prov_strat,select=c(NPI,consult_state_cd,Region,provider_durable_key,provider_gender,sat_mean,
                                     inapp_abx_rate,inapp_abx_sum,rate_strat,visit_count,visit_strat,stratum_id_nosat,stratum_id_nosat_label))
table_sat=subset(prov_strat,!is.na(satisfaction),select=c(NPI,consult_state_cd,Region,provider_durable_key,provider_gender,sat_mean,
                                     inapp_abx_rate,inapp_abx_sum,rate_strat,visit_count,visit_strat,stratum_id,stratum_id_label))

table_all$v=1
table_sat$v=1
vsum_all_bystate=aggregate(as.numeric(table_all[, c("v")]), list(table_all$NPI,table_all$consult_state_cd), sum)
vsum_sat_bystate=aggregate(as.numeric(table_sat[, c("v")]), list(table_sat$NPI,table_sat$consult_state_cd), sum)

names(vsum_all_bystate)[names(vsum_all_bystate) == 'x'] <- 'visit_count_bystate'
names(vsum_all_bystate)[names(vsum_all_bystate) == 'Group.1'] <- 'NPI'
names(vsum_all_bystate)[names(vsum_all_bystate) == 'Group.2'] <- 'consult_state_cd'

names(vsum_sat_bystate)[names(vsum_sat_bystate) == 'x'] <- 'visit_count_bystate'
names(vsum_sat_bystate)[names(vsum_sat_bystate) == 'Group.1'] <- 'NPI'
names(vsum_sat_bystate)[names(vsum_sat_bystate) == 'Group.2'] <- 'consult_state_cd'

table_all=merge(table_all,vsum_all_bystate,by=c("NPI","consult_state_cd"),all.x=T)
table_sat=merge(table_sat,vsum_sat_bystate,by=c("NPI","consult_state_cd"),all.x=T)

table_all=table_all[order(table_all[,"NPI"],-table_all[,"visit_count_bystate"]),]
table_sat=table_sat[order(table_sat[,"NPI"],-table_sat[,"visit_count_bystate"]),]

uni_all = table_all[!duplicated(table_all$NPI),]
uni_sat = table_sat[!duplicated(table_sat$NPI),]

uni_all=subset(uni_all,select=-c(v,visit_count_bystate))
uni_sat=subset(uni_sat,select=-c(v,visit_count_bystate))

write.csv(uni_all,"./Output/table_all.csv",row.names = F)
write.csv(uni_sat,"./Output/table_sat.csv",row.names = F)

