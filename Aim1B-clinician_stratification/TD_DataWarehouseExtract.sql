# THIS IS FOR REFERENCE PURPOSES ONLY, SHARED BY TELADOC WITH USC

# Jan 1, 2018 to Dec 31, 2018
# GenMed only
# Primary member only
# Includes all completed visits, but only prescriptions that are only flagged as antibiotic
# There may be cases where two antibiotics were prescribed, in which case there will be two rows for the same consult.
# No test visits
# Survey question: ‘Overall, how would you rate the service provided by the Teladoc physician?’
# May or may not have completed a survey
# Includes one week per month, for 12 months



# Update March 14, 2019
# Include all codes for acute respiratory infections

SELECT DISTINCT
    provider_durable_key,
    NPI,
    a.member_durable_key,
    consult_key,
    consult_state_cd,
    consult_status_dt,
    question,
    answer,
    member_dob,
    member_gender,
    provider_gender,
    diagnosis_type_cd,
    encrypt_diagnosis_cd,
    encrypt_diagnosis_nm,
    consult_scheduled_dt,
    encrypt_ndc,
    month
FROM
    (SELECT DISTINCT
        a.provider_durable_key,
            a.member_durable_key,
            a.consult_key,
            d.dob_dt member_dob,
            d.gender_cd member_gender,
            f.gender_cd provider_gender,
            consult_state_cd,
            a.consult_status_dt,
            question,
            answer,
            f.npi NPI,
            a.encrypt_diagnosis_cd,
            a.encrypt_diagnosis_nm,
            a.diagnosis_type_cd,
            a.consult_scheduled_dt,
            d.gender_nm,
            quarter_nbr,
            WEEK(consult_status_dt) week,
            MONTH(consult_status_dt) month,
            encrypt_ndc
    FROM
        DW.transdim_consult a
    JOIN DW.dim_member d ON (d.member_key = a.member_key)
        AND DATE(a.consult_status_dt) BETWEEN '2018-01-01' AND '2018-12-31'
        AND a.consult_status_cd = 'CONSULTSTATUS_COM'
        AND a.`scd_current_flg` = 'Y'
        AND a.test_consult_flg = 'N'
        ## POTENTIALLY ADD FILTER FOR ARIs e.g. a.acute_respiratory_flg = 'Y' 


    JOIN DW.dim_provider f ON (f.provider_durable_key = a.provider_durable_key)
        AND f.scd_current_flg = 'Y'
    JOIN DW.dim_date h ON (a.date_key = h.date_key)
    LEFT JOIN (SELECT DISTINCT
        consult_durable_key, j.encrypt_ndc
    FROM
        DW.transdim_prescription i
    JOIN DW.dim_drug j ON (i.drug_key = j.drug_key)
    WHERE
        antibiotic_flg = 'Y'
            AND i.`scd_current_flg` = 'Y'
            AND prescription_completion_flg = 'Y') i ON (i.consult_durable_key = a.consult_durable_key)

    LEFT JOIN (SELECT DISTINCT
        a.consult_durable_key, question, answer
    FROM
        DW.transdim_survey_response a
    JOIN DW.dim_survey_qa b ON (a.survey_qa_durable_key = b.survey_qa_durable_key)
    WHERE
        question_mnemonic_cd IN ('Q_3_3')
            AND b.scd_current_flg = 'Y'
            AND survey_mnemonic_cd = 'SURVEYMNEMONIC_GEN'
            AND a.scd_
current_flg = 'Y') b ON (a.consult_durable_key = b.consult_durable_key)
    WHERE
        a.service_key = 1
            AND d.primary_flg = 'Y') a
WHERE
    week MOD 4 = 0;

