/*Merge extract and abx inappropriate, consult region by dx code*/

create table extract as
  select a.*, b.flag, c.region as Consult_Region
  from Consult_Details a
  left join ICD_VALUE_SET_LINDER b
  on a.diagnosis_1=b.diagnosis_cd
  left join Region_List c
  on a.consult_state=c.state_abbreviation
 ;
 
 /*create 
 
 
