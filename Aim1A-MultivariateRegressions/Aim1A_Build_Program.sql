/*Merge extract and abx inappropriate, consult region by dx code*/

create table extract as
  select a.*, b.flag, c.region as Consult_Region, d.region as Patient_Region
  from Consult_Details a
  left join ICD_VALUE_SET_LINDER b
  on a.diagnosis_1=b.diagnosis_cd
  left join Region_List c
  on a.consult_state=c.state_abbreviation
  left join Region_List d
  on a.patient_state=d.state_abbreviation
 ;
 
/*create rolling cancellations flag*/
/*edit this to account for the code for non-patient cancelations, possibly add a monthly cutoff*/
create table extract1
  select a.* , sum(case when a.Cancelled_Consult="Y" then 1 else 0) over(partition by a.member_id order by a.member_id, a.consult_request_date) as Prev_Cancellations
  from extract a
  order by a.member_id, a.consult_request_date, a.consult_start_date_time
 ;
 
 
 
 
