/*Summary Tables and Sanity Checks*/

/*Count Consult IDs (compare to n obs)*/
select count(Consult_ID) as n_consults
from consult_details
;

/*Frequency of Cancellation Codes*/
select distinct Cancelled_Consult_Reason_Code, Cancelled_Consult_Reasons_Description, count(consult_id) as n_consults
from consult_details
group by Cancelled_Consult_Reason_Code, Cancelled_Consult_Reasons_Description
;

/*Percent 
