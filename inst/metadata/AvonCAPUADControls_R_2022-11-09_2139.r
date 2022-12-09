#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('AvonCAPUADControls_DATA_2022-11-09_2139.csv')
#Setting Labels

label(data$study_number)="Record  Number"
label(data$enrollment_date)="Enrollment Date"
label(data$hosp)="Hospital"
label(data$ip_op)="Inpatient versus Outpatient"
label(data$speciality)="Department/Speciality"
label(data$gender)="Gender"
label(data$care_home)="Care home resident"
label(data$ethnicity)="Ethnicity"
label(data$smoking)="Smoking status"
label(data$vaping)="Vaping (in last 30 days)"
label(data$age_at_admission)="Age at enrollment"
label(data$age_group_2)="Age Group"
label(data$infection)="Does this patient have an acute, recent (under 28 days) or chronic infection?"
label(data$disease_exclusion)="Does this patient have any respiratory conditions which necessitate them being excluded as a control?(e.g. COPD, bronchiectasis, CF, etc) *COPD is permissible, provided there has not been an exacerbation within 3 months prior to enrolment. "
label(data$lung_cancer)="Does this patient have lung cancer or any malignancy metastatic to the lungs? "
label(data$temp)="Does this participant have fever (Temp ≥38.0°C)? "
label(data$imm_disease)="Does the participant have any immunosuppressive disease (e.g. Leukaemia)? "
label(data$pneumo_vaccine)="Does this participant have a Pneumococcal conjugate vaccine (PCV) or Pneumococal Polysacharide Vaccine (PPV) within the past 30 days? "
label(data$staffdoingscreening)="Study Staff Member completing form"
label(data$screening_complete)="Complete?"
label(data$aconvap_number)="Avon CAP Study Number"
label(data$consent_nurse)="Name of person obtaining consent"
label(data$pis_version)="Which version of PIS has been given? "
label(data$gp_practice)="Registered GP practice"
label(data$pt_ad_np)="Should this patient have a control nose/throat swab?"
label(data$np_swab)="Nose/Throat swab taken"
label(data$adm_np_type)="Site taken from"
label(data$np_date)="Date and time of Nose/Throat swab"
label(data$np_date_2)="Date of Nose/Throat swab"
label(data$study_staff_member_swab)="Study staff member taking the Nose/Throat swab (initials)"
label(data$pt_control_ur)="Should this patient have a control urine sample?"
label(data$adm_ur_taken)="Urine sample obtained?"
label(data$adm_ur_date)="Date and time of urine sample"
label(data$adm_ur_date_2)="Date of urine sample"
label(data$freezer)="Date and time Frozen"
label(data$study_staff_member_urine)="Study staff member taking the urine sample (initials)"
label(data$adm_ser)="Should this patient have a control serum sample?"
label(data$adm_serum_tak)="Serum sample obtained?"
label(data$adm_seru_date)="Date and time of Serum sample"
label(data$adm_seru_date_2)="Date of Serum sample"
label(data$study_staff_member_serum)="Study staff member taking the Serum sample (initials)"
label(data$study_and_sample_details_complete)="Complete?"
label(data$resp_disease___1)="Respiratory Disease (choice=None)"
label(data$resp_disease___2)="Respiratory Disease (choice=COPD (Chronic Obstructive Pulmonary Disease/Emphysema))"
label(data$resp_disease___3)="Respiratory Disease (choice=Asthma)"
label(data$resp_disease___4)="Respiratory Disease (choice=Other Respiratory Disease)"
label(data$resp_disease___ni)="Respiratory Disease (choice=No information)"
label(data$resp_disease___unk)="Respiratory Disease (choice=Unknown)"
label(data$resp_disease___na)="Respiratory Disease (choice=Not applicable)"
label(data$other_respiratory_disease___1)="Specify Other Respiratory Disease (choice=Bronchiectasis)"
label(data$other_respiratory_disease___2)="Specify Other Respiratory Disease (choice=Interstitial lung disease/pulmonary fibrosis)"
label(data$other_respiratory_disease___3)="Specify Other Respiratory Disease (choice=Cystic Fibrosis)"
label(data$other_respiratory_disease___4)="Specify Other Respiratory Disease (choice=Pulmonary Hypertension)"
label(data$other_respiratory_disease___5)="Specify Other Respiratory Disease (choice=Chronic pleural disease)"
label(data$other_respiratory_disease___6)="Specify Other Respiratory Disease (choice=Other chronic respiratory disease)"
label(data$other_respiratory_disease___ni)="Specify Other Respiratory Disease (choice=No information)"
label(data$other_respiratory_disease___unk)="Specify Other Respiratory Disease (choice=Unknown)"
label(data$other_respiratory_disease___na)="Specify Other Respiratory Disease (choice=Not applicable)"
label(data$spec_other_resp)="Please specify other respiratory disease"
label(data$chd___1)="Chronic Heart Disease (choice=None)"
label(data$chd___2)="Chronic Heart Disease (choice=Congestive heart failure)"
label(data$chd___3)="Chronic Heart Disease (choice=Ischaemic heart disease (coronary artery disease))"
label(data$chd___4)="Chronic Heart Disease (choice=Hypertension (only if causing cardiac complications))"
label(data$chd___5)="Chronic Heart Disease (choice=Other)"
label(data$chd___ni)="Chronic Heart Disease (choice=No information)"
label(data$chd___unk)="Chronic Heart Disease (choice=Unknown)"
label(data$chd___na)="Chronic Heart Disease (choice=Not applicable)"
label(data$mi)="Has the patient had a documented MI (unstable angina, NSTEMI or STEMI)?"
label(data$other_chd___1)="Other Chronic Heart Disease (choice=Congenital heart disease)"
label(data$other_chd___2)="Other Chronic Heart Disease (choice=Atrial Fibrillation)"
label(data$other_chd___3)="Other Chronic Heart Disease (choice=Other arrhythmia)"
label(data$other_chd___4)="Other Chronic Heart Disease (choice=PPM or ICD in situ)"
label(data$other_chd___5)="Other Chronic Heart Disease (choice=Valvular heart disease)"
label(data$other_chd___6)="Other Chronic Heart Disease (choice=Other cardiac pathology)"
label(data$other_chd___ni)="Other Chronic Heart Disease (choice=No information)"
label(data$other_chd___unk)="Other Chronic Heart Disease (choice=Unknown)"
label(data$other_chd___na)="Other Chronic Heart Disease (choice=Not applicable)"
label(data$spec_other_cardiac)="Please specify other cardiac pathology"
label(data$ckd)="Chronic Kidney Disease (CKD) Mod-Severe =eGFR< 30, Cr>265 umol/L, dialysis, transplantation, uremic syndrome"
label(data$liver_disease)="Liver Disease "
label(data$diabetes)="Diabetes"
label(data$dm_meds)="Diabetes medication"
label(data$dementia___1)="Cognitive Impairment/Dementia (choice=None)"
label(data$dementia___2)="Cognitive Impairment/Dementia (choice=Dementia)"
label(data$dementia___3)="Cognitive Impairment/Dementia (choice=Cognitive Impairment)"
label(data$dementia___ni)="Cognitive Impairment/Dementia (choice=No information)"
label(data$dementia___unk)="Cognitive Impairment/Dementia (choice=Unknown)"
label(data$dementia___na)="Cognitive Impairment/Dementia (choice=Not applicable)"
label(data$neurological_disease___1)="Neurological Disease (choice=Chronic neurological disease (excluding CVA and TIA))"
label(data$neurological_disease___2)="Neurological Disease (choice=CVA (stroke))"
label(data$neurological_disease___3)="Neurological Disease (choice=TIA (mini-stroke))"
label(data$neurological_disease___4)="Neurological Disease (choice=Hemiplegia)"
label(data$neurological_disease___5)="Neurological Disease (choice=Paraplegia)"
label(data$neurological_disease___6)="Neurological Disease (choice=None)"
label(data$neurological_disease___ni)="Neurological Disease (choice=No information)"
label(data$neurological_disease___unk)="Neurological Disease (choice=Unknown)"
label(data$neurological_disease___na)="Neurological Disease (choice=Not applicable)"
label(data$gastric_ulcers)="Gastric/Duodenal Ulcer Disease Patients who have required treatment for PUD"
label(data$pvd)="Peripheral Vascular Disease Intermittent claudication, periph. arterial bypass for insufficiency, gangrene, acute arterial insufficiency, untreated aneurysm (>=6cm)"
label(data$ctd)="Connective Tissue Disease (SLE, polymyositis, mixedConnective Tissue Disease, polymyalgia rheumatica, moderate to severe Rheumatoid Arthritis)"
label(data$immsup)="Immunosuppressive Medication(includes oral steroids, biologics, chemotherapy)"
label(data$immunodeficiency)="Immunodeficiency(eg SCID, hypogammaglobulinaemia, splenectomy)"
label(data$other_pn_disease)="Other pneumococcal disease risk factors? (eg CSF leak, cochlear implant, asplenia)"
label(data$hiv___1)="HIV status (choice=Negative (no HIV), or not tested)"
label(data$hiv___2)="HIV status (choice=HIV)"
label(data$hiv___3)="HIV status (choice=AIDS)"
label(data$hiv___ni)="HIV status (choice=No information)"
label(data$hiv___unk)="HIV status (choice=Unknown)"
label(data$hiv___na)="HIV status (choice=Not applicable)"
label(data$cancer)="Solid Organ Cancer/Malignancy Initially treated in the last 5 years exclude non-melanomatous skin cancers and in situ cervical carcinoma"
label(data$haem_malig___1)="Haematological Malignancy Leukaemia =CML, CLL, AML, ALL, Polycythaemia Vera Lymphoma =NHL, Hodgkins, Waldenström, multiple myeloma  (choice=None)"
label(data$haem_malig___2)="Haematological Malignancy Leukaemia =CML, CLL, AML, ALL, Polycythaemia Vera Lymphoma =NHL, Hodgkins, Waldenström, multiple myeloma  (choice=Leukaemia)"
label(data$haem_malig___3)="Haematological Malignancy Leukaemia =CML, CLL, AML, ALL, Polycythaemia Vera Lymphoma =NHL, Hodgkins, Waldenström, multiple myeloma  (choice=Lymphoma)"
label(data$haem_malig___ni)="Haematological Malignancy Leukaemia =CML, CLL, AML, ALL, Polycythaemia Vera Lymphoma =NHL, Hodgkins, Waldenström, multiple myeloma  (choice=No information)"
label(data$haem_malig___unk)="Haematological Malignancy Leukaemia =CML, CLL, AML, ALL, Polycythaemia Vera Lymphoma =NHL, Hodgkins, Waldenström, multiple myeloma  (choice=Unknown)"
label(data$haem_malig___na)="Haematological Malignancy Leukaemia =CML, CLL, AML, ALL, Polycythaemia Vera Lymphoma =NHL, Hodgkins, Waldenström, multiple myeloma  (choice=Not applicable)"
label(data$transplant)="Organ Transplantation(includes bone marrow stem cells etc)"
label(data$pregnancy)="Pregnancy/Post partum"
label(data$flu_vaccine)="Seasonal Influenza Vaccination (in last 12 months)"
label(data$flu_date)="Date of Last Flu Vaccination"
label(data$ppv23)="PneumoVax (PPV23)"
label(data$ppv23_date)="Date of PneumoVax Vaccination"
label(data$covid19_vax)="COVID-19 Vaccination"
label(data$covidvax_date)="Date of COVID19 Vaccination"
label(data$brand_of_covid19_vaccinati)="Brand of COVID19 vaccination"
label(data$abx_14d_prior)="Used Antibiotics in 14 days prior to Hospitalisation"
label(data$antibiotic_used___1)="Antibiotic Used (choice=Amoxicillin)"
label(data$antibiotic_used___2)="Antibiotic Used (choice=Co-amoxiclav (Augmentin))"
label(data$antibiotic_used___3)="Antibiotic Used (choice=Doxycycline)"
label(data$antibiotic_used___4)="Antibiotic Used (choice=Trimethoprim)"
label(data$antibiotic_used___5)="Antibiotic Used (choice=Nitrofurantoin)"
label(data$antibiotic_used___6)="Antibiotic Used (choice=Clarithromycin/Erythromycin)"
label(data$antibiotic_used___7)="Antibiotic Used (choice=Metronidazole)"
label(data$antibiotic_used___8)="Antibiotic Used (choice=Septrin (Co-trimoxazole))"
label(data$antibiotic_used___9)="Antibiotic Used (choice=Penicillin V)"
label(data$antibiotic_used___10)="Antibiotic Used (choice=Benzylpenicillin (BenPen))"
label(data$antibiotic_used___11)="Antibiotic Used (choice=Tazocin)"
label(data$antibiotic_used___12)="Antibiotic Used (choice=Meropenem)"
label(data$antibiotic_used___13)="Antibiotic Used (choice=Other)"
label(data$antibiotic_used___ni)="Antibiotic Used (choice=No information)"
label(data$antibiotic_used___unk)="Antibiotic Used (choice=Unknown)"
label(data$antibiotic_used___na)="Antibiotic Used (choice=Not applicable)"
label(data$antibiotic_other)="Please specify other antibiotic"
label(data$cci_age_score)="Age at admission: [age_at_admission]"
label(data$mi_score)="Myocardial infarction: [mi]"
label(data$chf_score)="CHF: [chd(2)]"
label(data$pvd_score)="Peripheral Vascular disease: [pvd]"
label(data$neuro_score)="CVA: [neurological_disease(2)] or TIA:[neurological_disease(3)]"
label(data$dementia_score)="Dementia: [dementia(2)]"
label(data$copd_score)="COPD: [resp_disease(2)]"
label(data$peptic_ulcer_score)="Peptic ulcer disease: [gastric_ulcers]"
label(data$ctd_score)="Connective tissue disease: [ctd]"
label(data$liver_disease_score)="Liver disease: [liver_disease]"
label(data$diabetes_score)="Diabetes mellitus: [diabetes]"
label(data$neuro_disease_score)="Hemiplegia: [neurological_disease(4)] or Paraplegia: [neurological_disease(5)]"
label(data$ckd_score)="Moderate to severe CKD: [ckd]"
label(data$cancer_score)="Solid tumor: [cancer]"
label(data$leukemia_score)="Leukemia: [haem_malig(2)]"
label(data$lymphoma_score)="Lymphoma: [haem_malig(3)]"
label(data$hiv_score)="AIDS: [hiv(3)]"
label(data$cci_total_score)="Total score:"
label(data$comorbidities_complete)="Complete?"
label(data$newaf)="In the 2 months before enrolment, has the participant had new episode of atrial fibrillation (AF)"
label(data$newarrhythmia)="In the 2 months before enrolment, has the participant had new episode of arrhythmia other than AF?"
label(data$newnstemi)="In the 2 months before enrolment, as the participant had new NSTEMI?"
label(data$newstemi)="In the 2 months before enrolment, has the participant had new STEMI?"
label(data$new_hf)="In the 2 months before enrolment, has the participant had new or worsening heart failure?"
label(data$new_cva)="In the 2 months before enrolment, has the participant had a new CVA, TIA or stroke?"
label(data$new_pe)="In the 2 months before enrolment, has the participant had a new PE (pulmonary embolism)?"
label(data$new_dvt)="In the 2 months before enrolment, has the participant had a new DVT (deep vein thrombosis)?"
label(data$cvs_events_complete)="Complete?"
label(data$date_of_withdrawal)="Date of Withdrawal"
label(data$reason_for_withdrawal)="Reason for withdrawal "
label(data$does_participant_want_the)="Does participant want the samples to be destroyed? "
label(data$withdrawal_complete)="Complete?"
label(data$sae_v2_v2)="Were there any AE as a result of the study? "
label(data$what_was_the_adverse_event_v2)="What was the Adverse Event? "
label(data$date_of_sae_v2_v2)="Date of AE"
label(data$severity_v2)="Severity "
label(data$study_intervention_relatio_v2)="Study Intervention Relationship"
label(data$action_taken_v2)="Action taken "
label(data$diagnosis_v2_v2)="Outcome of AE "
label(data$expected_v2)="Expected "
label(data$serious_adverse_event_v2)="Serious Adverse Event? "
label(data$site_aware_date_of_sae_v2_v2)="When the site became aware of the AE? "
label(data$reported_to_chief_investig_v2_v2)="Reported to Chief Investigator Date"
label(data$date_of_resolution_v2_v2)="Date of Resolution "
label(data$comments_v2_v2)="Comments "
label(data$aes_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$hosp.factor = factor(data$hosp,levels=c("1","2","3"))
data$ip_op.factor = factor(data$ip_op,levels=c("1","2","3","4"))
data$gender.factor = factor(data$gender,levels=c("1","2"))
data$care_home.factor = factor(data$care_home,levels=c("1","0"))
data$ethnicity.factor = factor(data$ethnicity,levels=c("1","2","3","4","5","6","7"))
data$smoking.factor = factor(data$smoking,levels=c("1","2","3","4"))
data$vaping.factor = factor(data$vaping,levels=c("1","2","3"))
data$age_group_2.factor = factor(data$age_group_2,levels=c("1","2","3","4","5","6"))
data$infection.factor = factor(data$infection,levels=c("1","0"))
data$disease_exclusion.factor = factor(data$disease_exclusion,levels=c("1","0"))
data$lung_cancer.factor = factor(data$lung_cancer,levels=c("1","0"))
data$temp.factor = factor(data$temp,levels=c("1","0"))
data$imm_disease.factor = factor(data$imm_disease,levels=c("1","0"))
data$pneumo_vaccine.factor = factor(data$pneumo_vaccine,levels=c("1","0"))
data$staffdoingscreening.factor = factor(data$staffdoingscreening,levels=c("1","2","3","4","5","6","7","8","101","102","103","104","105","106","107","109","110","111","112"))
data$screening_complete.factor = factor(data$screening_complete,levels=c("0","1","2"))
data$consent_nurse.factor = factor(data$consent_nurse,levels=c("1","2","3","4","5","6","7","8","101","102","103","104","105","106","107","109","110","111","112"))
data$pis_version.factor = factor(data$pis_version,levels=c("1"))
data$pt_ad_np.factor = factor(data$pt_ad_np,levels=c("1","0"))
data$np_swab.factor = factor(data$np_swab,levels=c("1","2"))
data$adm_np_type.factor = factor(data$adm_np_type,levels=c("1","2","3"))
data$study_staff_member_swab.factor = factor(data$study_staff_member_swab,levels=c("1","2","3","4","5","6","7","8","101","102","103","104","105","106","107","109","110","111","112"))
data$pt_control_ur.factor = factor(data$pt_control_ur,levels=c("1","0"))
data$adm_ur_taken.factor = factor(data$adm_ur_taken,levels=c("1","2"))
data$study_staff_member_urine.factor = factor(data$study_staff_member_urine,levels=c("1","2","3","4","5","6","7","8","101","102","103","104","105","106","107","109","110","111","112"))
data$adm_ser.factor = factor(data$adm_ser,levels=c("1","0"))
data$adm_serum_tak.factor = factor(data$adm_serum_tak,levels=c("1","0"))
data$study_staff_member_serum.factor = factor(data$study_staff_member_serum,levels=c("1","2","3","4","5","6","7","8","101","102","103","104","105","106","107","109","110","111","112"))
data$study_and_sample_details_complete.factor = factor(data$study_and_sample_details_complete,levels=c("0","1","2"))
data$resp_disease___1.factor = factor(data$resp_disease___1,levels=c("0","1"))
data$resp_disease___2.factor = factor(data$resp_disease___2,levels=c("0","1"))
data$resp_disease___3.factor = factor(data$resp_disease___3,levels=c("0","1"))
data$resp_disease___4.factor = factor(data$resp_disease___4,levels=c("0","1"))
data$resp_disease___ni.factor = factor(data$resp_disease___ni,levels=c("0","1"))
data$resp_disease___unk.factor = factor(data$resp_disease___unk,levels=c("0","1"))
data$resp_disease___na.factor = factor(data$resp_disease___na,levels=c("0","1"))
data$other_respiratory_disease___1.factor = factor(data$other_respiratory_disease___1,levels=c("0","1"))
data$other_respiratory_disease___2.factor = factor(data$other_respiratory_disease___2,levels=c("0","1"))
data$other_respiratory_disease___3.factor = factor(data$other_respiratory_disease___3,levels=c("0","1"))
data$other_respiratory_disease___4.factor = factor(data$other_respiratory_disease___4,levels=c("0","1"))
data$other_respiratory_disease___5.factor = factor(data$other_respiratory_disease___5,levels=c("0","1"))
data$other_respiratory_disease___6.factor = factor(data$other_respiratory_disease___6,levels=c("0","1"))
data$other_respiratory_disease___ni.factor = factor(data$other_respiratory_disease___ni,levels=c("0","1"))
data$other_respiratory_disease___unk.factor = factor(data$other_respiratory_disease___unk,levels=c("0","1"))
data$other_respiratory_disease___na.factor = factor(data$other_respiratory_disease___na,levels=c("0","1"))
data$chd___1.factor = factor(data$chd___1,levels=c("0","1"))
data$chd___2.factor = factor(data$chd___2,levels=c("0","1"))
data$chd___3.factor = factor(data$chd___3,levels=c("0","1"))
data$chd___4.factor = factor(data$chd___4,levels=c("0","1"))
data$chd___5.factor = factor(data$chd___5,levels=c("0","1"))
data$chd___ni.factor = factor(data$chd___ni,levels=c("0","1"))
data$chd___unk.factor = factor(data$chd___unk,levels=c("0","1"))
data$chd___na.factor = factor(data$chd___na,levels=c("0","1"))
data$mi.factor = factor(data$mi,levels=c("1","0"))
data$other_chd___1.factor = factor(data$other_chd___1,levels=c("0","1"))
data$other_chd___2.factor = factor(data$other_chd___2,levels=c("0","1"))
data$other_chd___3.factor = factor(data$other_chd___3,levels=c("0","1"))
data$other_chd___4.factor = factor(data$other_chd___4,levels=c("0","1"))
data$other_chd___5.factor = factor(data$other_chd___5,levels=c("0","1"))
data$other_chd___6.factor = factor(data$other_chd___6,levels=c("0","1"))
data$other_chd___ni.factor = factor(data$other_chd___ni,levels=c("0","1"))
data$other_chd___unk.factor = factor(data$other_chd___unk,levels=c("0","1"))
data$other_chd___na.factor = factor(data$other_chd___na,levels=c("0","1"))
data$ckd.factor = factor(data$ckd,levels=c("1","2","3"))
data$liver_disease.factor = factor(data$liver_disease,levels=c("1","2","3"))
data$diabetes.factor = factor(data$diabetes,levels=c("1","2","3","4","5"))
data$dm_meds.factor = factor(data$dm_meds,levels=c("1","2"))
data$dementia___1.factor = factor(data$dementia___1,levels=c("0","1"))
data$dementia___2.factor = factor(data$dementia___2,levels=c("0","1"))
data$dementia___3.factor = factor(data$dementia___3,levels=c("0","1"))
data$dementia___ni.factor = factor(data$dementia___ni,levels=c("0","1"))
data$dementia___unk.factor = factor(data$dementia___unk,levels=c("0","1"))
data$dementia___na.factor = factor(data$dementia___na,levels=c("0","1"))
data$neurological_disease___1.factor = factor(data$neurological_disease___1,levels=c("0","1"))
data$neurological_disease___2.factor = factor(data$neurological_disease___2,levels=c("0","1"))
data$neurological_disease___3.factor = factor(data$neurological_disease___3,levels=c("0","1"))
data$neurological_disease___4.factor = factor(data$neurological_disease___4,levels=c("0","1"))
data$neurological_disease___5.factor = factor(data$neurological_disease___5,levels=c("0","1"))
data$neurological_disease___6.factor = factor(data$neurological_disease___6,levels=c("0","1"))
data$neurological_disease___ni.factor = factor(data$neurological_disease___ni,levels=c("0","1"))
data$neurological_disease___unk.factor = factor(data$neurological_disease___unk,levels=c("0","1"))
data$neurological_disease___na.factor = factor(data$neurological_disease___na,levels=c("0","1"))
data$gastric_ulcers.factor = factor(data$gastric_ulcers,levels=c("1","0"))
data$pvd.factor = factor(data$pvd,levels=c("1","0"))
data$ctd.factor = factor(data$ctd,levels=c("1","0"))
data$immsup.factor = factor(data$immsup,levels=c("1","0"))
data$immunodeficiency.factor = factor(data$immunodeficiency,levels=c("1","0"))
data$other_pn_disease.factor = factor(data$other_pn_disease,levels=c("1","0"))
data$hiv___1.factor = factor(data$hiv___1,levels=c("0","1"))
data$hiv___2.factor = factor(data$hiv___2,levels=c("0","1"))
data$hiv___3.factor = factor(data$hiv___3,levels=c("0","1"))
data$hiv___ni.factor = factor(data$hiv___ni,levels=c("0","1"))
data$hiv___unk.factor = factor(data$hiv___unk,levels=c("0","1"))
data$hiv___na.factor = factor(data$hiv___na,levels=c("0","1"))
data$cancer.factor = factor(data$cancer,levels=c("1","2","3"))
data$haem_malig___1.factor = factor(data$haem_malig___1,levels=c("0","1"))
data$haem_malig___2.factor = factor(data$haem_malig___2,levels=c("0","1"))
data$haem_malig___3.factor = factor(data$haem_malig___3,levels=c("0","1"))
data$haem_malig___ni.factor = factor(data$haem_malig___ni,levels=c("0","1"))
data$haem_malig___unk.factor = factor(data$haem_malig___unk,levels=c("0","1"))
data$haem_malig___na.factor = factor(data$haem_malig___na,levels=c("0","1"))
data$transplant.factor = factor(data$transplant,levels=c("1","0"))
data$pregnancy.factor = factor(data$pregnancy,levels=c("1","2","3","4","5","6"))
data$flu_vaccine.factor = factor(data$flu_vaccine,levels=c("1","2","3"))
data$ppv23.factor = factor(data$ppv23,levels=c("1","2","3"))
data$covid19_vax.factor = factor(data$covid19_vax,levels=c("1","2","3"))
data$abx_14d_prior.factor = factor(data$abx_14d_prior,levels=c("1","2","3"))
data$antibiotic_used___1.factor = factor(data$antibiotic_used___1,levels=c("0","1"))
data$antibiotic_used___2.factor = factor(data$antibiotic_used___2,levels=c("0","1"))
data$antibiotic_used___3.factor = factor(data$antibiotic_used___3,levels=c("0","1"))
data$antibiotic_used___4.factor = factor(data$antibiotic_used___4,levels=c("0","1"))
data$antibiotic_used___5.factor = factor(data$antibiotic_used___5,levels=c("0","1"))
data$antibiotic_used___6.factor = factor(data$antibiotic_used___6,levels=c("0","1"))
data$antibiotic_used___7.factor = factor(data$antibiotic_used___7,levels=c("0","1"))
data$antibiotic_used___8.factor = factor(data$antibiotic_used___8,levels=c("0","1"))
data$antibiotic_used___9.factor = factor(data$antibiotic_used___9,levels=c("0","1"))
data$antibiotic_used___10.factor = factor(data$antibiotic_used___10,levels=c("0","1"))
data$antibiotic_used___11.factor = factor(data$antibiotic_used___11,levels=c("0","1"))
data$antibiotic_used___12.factor = factor(data$antibiotic_used___12,levels=c("0","1"))
data$antibiotic_used___13.factor = factor(data$antibiotic_used___13,levels=c("0","1"))
data$antibiotic_used___ni.factor = factor(data$antibiotic_used___ni,levels=c("0","1"))
data$antibiotic_used___unk.factor = factor(data$antibiotic_used___unk,levels=c("0","1"))
data$antibiotic_used___na.factor = factor(data$antibiotic_used___na,levels=c("0","1"))
data$comorbidities_complete.factor = factor(data$comorbidities_complete,levels=c("0","1","2"))
data$newaf.factor = factor(data$newaf,levels=c("1","0"))
data$newarrhythmia.factor = factor(data$newarrhythmia,levels=c("1","0"))
data$newnstemi.factor = factor(data$newnstemi,levels=c("1","0"))
data$newstemi.factor = factor(data$newstemi,levels=c("1","0"))
data$new_hf.factor = factor(data$new_hf,levels=c("1","0"))
data$new_cva.factor = factor(data$new_cva,levels=c("1","0"))
data$new_pe.factor = factor(data$new_pe,levels=c("1","0"))
data$new_dvt.factor = factor(data$new_dvt,levels=c("1","0"))
data$cvs_events_complete.factor = factor(data$cvs_events_complete,levels=c("0","1","2"))
data$does_participant_want_the.factor = factor(data$does_participant_want_the,levels=c("1","0"))
data$withdrawal_complete.factor = factor(data$withdrawal_complete,levels=c("0","1","2"))
data$sae_v2_v2.factor = factor(data$sae_v2_v2,levels=c("1","0"))
data$severity_v2.factor = factor(data$severity_v2,levels=c("1","2","3"))
data$study_intervention_relatio_v2.factor = factor(data$study_intervention_relatio_v2,levels=c("1","2","3"))
data$expected_v2.factor = factor(data$expected_v2,levels=c("1","0"))
data$serious_adverse_event_v2.factor = factor(data$serious_adverse_event_v2,levels=c("1","0"))
data$aes_complete.factor = factor(data$aes_complete,levels=c("0","1","2"))

levels(data$hosp.factor)=c("Southmead","BRI","RUH")
levels(data$ip_op.factor)=c("Inpatient","Day Case","Outpatient","Other volunteer")
levels(data$gender.factor)=c("Male","Female")
levels(data$care_home.factor)=c("Yes","No")
levels(data$ethnicity.factor)=c("White British(e.g. Welsh, Irish, English, Scottish, Gypsy or Irish Traveller, etc.)","White other(e.g. Italian, Polish, Spanish, German, French, Russian etc.)","Mixed origin(e.g. White and Asian, Asian and Black, White and Black/Caribbean/African etc.)","Black or African American(e.g. African American, Caribbean, Black British, Jamaican, Haitian, Nigerian, Ethiopian, Somalian, etc)","Asian(e.g. Chinese, Filipino, Asian Indian, Pakistani, Bangladeshi, Vietnamese, Korean, Japanese, Asian British, etc)","Some other race, ethnicity or origin","Unknown")
levels(data$smoking.factor)=c("Current smoker","Ex-smoker","Non-smoker","Unknown")
levels(data$vaping.factor)=c("Yes","No","Unknown")
levels(data$age_group_2.factor)=c("18-30","31-40","41-50","51-60","61-70","71-100+")
levels(data$infection.factor)=c("Yes","No")
levels(data$disease_exclusion.factor)=c("Yes","No")
levels(data$lung_cancer.factor)=c("Yes","No")
levels(data$temp.factor)=c("Yes","No")
levels(data$imm_disease.factor)=c("Yes","No")
levels(data$pneumo_vaccine.factor)=c("Yes","No")
levels(data$staffdoingscreening.factor)=c("Catherine Hyams","Jade King","Kelly Turner","Oliver Griffiths","Julie Cloake","Anabella Turner","Kaz Fox","Robyn Heath","Catherine Hyams","Zandile Maseko","Lana Ward","Zsolt Friedrich","Elena Bellavia","Kate Helliker","Marianne Vasquez","Leigh Morrison","Anna Morley","Gabriella Ruffino","Johanna Kellett Wright")
levels(data$screening_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$consent_nurse.factor)=c("Catherine Hyams","Jade King","Kelly Turner","Anabella Turner","Oliver Griffiths","Julie Cloake","Kaz Fox","Robyn Heath","Catherine Hyams","Zandile Maseko","Lana Ward","Zsolt Friedrich","Elena Bellavia","Kate Helliker","Marianne Vasquez","Leigh Morrison","Anna Morley","Gabriella Ruffino","Johanna Kellett Wright")
levels(data$pis_version.factor)=c("v1.1 date 05/06/2020")
levels(data$pt_ad_np.factor)=c("Yes","No")
levels(data$np_swab.factor)=c("Yes","No")
levels(data$adm_np_type.factor)=c("Nose","Throat","Midturbinate")
levels(data$study_staff_member_swab.factor)=c("Catherine Hyams","Jade King","Kelly Turner","Anabella Turner","Oliver Griffiths","Julie Cloake","Kaz Fox","Robyn Heath","Catherine Hyams","Zandile Maseko","Lana Ward","Zsolt Friedrich","Elena Bellavia","Kate Helliker","Marianne Vasquez","Leigh Morrison","Anna Morley","Gabriella Ruffino","Johanna Kellett Wright")
levels(data$pt_control_ur.factor)=c("Yes","No")
levels(data$adm_ur_taken.factor)=c("Yes","No")
levels(data$study_staff_member_urine.factor)=c("Catherine Hyams","Jade King","Kelly Turner","Anabella Turner","Oliver Griffiths","Julie Cloake","Kaz Fox","Robyn Heath","Catherine Hyams","Zandile Maseko","Lana Ward","Zsolt Friedrich","Elena Bellavia","Kate Helliker","Marianne Vasquez","Leigh Morrison","Anna Morley","Gabriella Ruffino","Johanna Kellett Wright")
levels(data$adm_ser.factor)=c("Yes","No")
levels(data$adm_serum_tak.factor)=c("Yes","No")
levels(data$study_staff_member_serum.factor)=c("Catherine Hyams","Jade King","Kelly Turner","Anabella Turner","Oliver Griffiths","Julie Cloake","Kaz Fox","Robyn Heath","Catherine Hyams","Zandile Maseko","Lana Ward","Zsolt Friedrich","Elena Bellavia","Kate Helliker","Marianne Vasquez","Leigh Morrison","Anna Morley","Gabriella Ruffino","Johanna Kellett Wright")
levels(data$study_and_sample_details_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$resp_disease___1.factor)=c("Unchecked","Checked")
levels(data$resp_disease___2.factor)=c("Unchecked","Checked")
levels(data$resp_disease___3.factor)=c("Unchecked","Checked")
levels(data$resp_disease___4.factor)=c("Unchecked","Checked")
levels(data$resp_disease___ni.factor)=c("Unchecked","Checked")
levels(data$resp_disease___unk.factor)=c("Unchecked","Checked")
levels(data$resp_disease___na.factor)=c("Unchecked","Checked")
levels(data$other_respiratory_disease___1.factor)=c("Unchecked","Checked")
levels(data$other_respiratory_disease___2.factor)=c("Unchecked","Checked")
levels(data$other_respiratory_disease___3.factor)=c("Unchecked","Checked")
levels(data$other_respiratory_disease___4.factor)=c("Unchecked","Checked")
levels(data$other_respiratory_disease___5.factor)=c("Unchecked","Checked")
levels(data$other_respiratory_disease___6.factor)=c("Unchecked","Checked")
levels(data$other_respiratory_disease___ni.factor)=c("Unchecked","Checked")
levels(data$other_respiratory_disease___unk.factor)=c("Unchecked","Checked")
levels(data$other_respiratory_disease___na.factor)=c("Unchecked","Checked")
levels(data$chd___1.factor)=c("Unchecked","Checked")
levels(data$chd___2.factor)=c("Unchecked","Checked")
levels(data$chd___3.factor)=c("Unchecked","Checked")
levels(data$chd___4.factor)=c("Unchecked","Checked")
levels(data$chd___5.factor)=c("Unchecked","Checked")
levels(data$chd___ni.factor)=c("Unchecked","Checked")
levels(data$chd___unk.factor)=c("Unchecked","Checked")
levels(data$chd___na.factor)=c("Unchecked","Checked")
levels(data$mi.factor)=c("Yes","No")
levels(data$other_chd___1.factor)=c("Unchecked","Checked")
levels(data$other_chd___2.factor)=c("Unchecked","Checked")
levels(data$other_chd___3.factor)=c("Unchecked","Checked")
levels(data$other_chd___4.factor)=c("Unchecked","Checked")
levels(data$other_chd___5.factor)=c("Unchecked","Checked")
levels(data$other_chd___6.factor)=c("Unchecked","Checked")
levels(data$other_chd___ni.factor)=c("Unchecked","Checked")
levels(data$other_chd___unk.factor)=c("Unchecked","Checked")
levels(data$other_chd___na.factor)=c("Unchecked","Checked")
levels(data$ckd.factor)=c("No","Mild (CKD 1-3)","Moderate or Severe CKD (CKD 4-5, ESRF, Dialysis)")
levels(data$liver_disease.factor)=c("None","Liver disease without hepatic failure (no portal hypertension)","Liver disease with hepatic failure (including portal hypertension, variceal bleeding)")
levels(data$diabetes.factor)=c("No","Type 1 DM - no complications","Type 1 DM - complications","Type 2 DM - no complications","Type 2 DM - complications")
levels(data$dm_meds.factor)=c("Oral medication only","S/C insulin (or insulin pump)")
levels(data$dementia___1.factor)=c("Unchecked","Checked")
levels(data$dementia___2.factor)=c("Unchecked","Checked")
levels(data$dementia___3.factor)=c("Unchecked","Checked")
levels(data$dementia___ni.factor)=c("Unchecked","Checked")
levels(data$dementia___unk.factor)=c("Unchecked","Checked")
levels(data$dementia___na.factor)=c("Unchecked","Checked")
levels(data$neurological_disease___1.factor)=c("Unchecked","Checked")
levels(data$neurological_disease___2.factor)=c("Unchecked","Checked")
levels(data$neurological_disease___3.factor)=c("Unchecked","Checked")
levels(data$neurological_disease___4.factor)=c("Unchecked","Checked")
levels(data$neurological_disease___5.factor)=c("Unchecked","Checked")
levels(data$neurological_disease___6.factor)=c("Unchecked","Checked")
levels(data$neurological_disease___ni.factor)=c("Unchecked","Checked")
levels(data$neurological_disease___unk.factor)=c("Unchecked","Checked")
levels(data$neurological_disease___na.factor)=c("Unchecked","Checked")
levels(data$gastric_ulcers.factor)=c("Yes","No")
levels(data$pvd.factor)=c("Yes","No")
levels(data$ctd.factor)=c("Yes","No")
levels(data$immsup.factor)=c("Yes","No")
levels(data$immunodeficiency.factor)=c("Yes","No")
levels(data$other_pn_disease.factor)=c("Yes","No")
levels(data$hiv___1.factor)=c("Unchecked","Checked")
levels(data$hiv___2.factor)=c("Unchecked","Checked")
levels(data$hiv___3.factor)=c("Unchecked","Checked")
levels(data$hiv___ni.factor)=c("Unchecked","Checked")
levels(data$hiv___unk.factor)=c("Unchecked","Checked")
levels(data$hiv___na.factor)=c("Unchecked","Checked")
levels(data$cancer.factor)=c("None","Solid Organ Cancer - no mets","Solid Organ Cancer - Metastatic Disease")
levels(data$haem_malig___1.factor)=c("Unchecked","Checked")
levels(data$haem_malig___2.factor)=c("Unchecked","Checked")
levels(data$haem_malig___3.factor)=c("Unchecked","Checked")
levels(data$haem_malig___ni.factor)=c("Unchecked","Checked")
levels(data$haem_malig___unk.factor)=c("Unchecked","Checked")
levels(data$haem_malig___na.factor)=c("Unchecked","Checked")
levels(data$transplant.factor)=c("Yes","No")
levels(data$pregnancy.factor)=c("Not pregnant","First Trimester","Second Trimester","Third Trimester","Pregnant, unsure of trimester","Post-partum")
levels(data$flu_vaccine.factor)=c("Received Seasonal Flu vaccine","Not received","Unknown")
levels(data$ppv23.factor)=c("Received PneumoVax","Not received","Unknown")
levels(data$covid19_vax.factor)=c("Received COVID19 vaccine","Not received","Unknown")
levels(data$abx_14d_prior.factor)=c("Yes","No","Unknown")
levels(data$antibiotic_used___1.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___2.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___3.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___4.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___5.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___6.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___7.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___8.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___9.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___10.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___11.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___12.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___13.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___ni.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___unk.factor)=c("Unchecked","Checked")
levels(data$antibiotic_used___na.factor)=c("Unchecked","Checked")
levels(data$comorbidities_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$newaf.factor)=c("Yes","No")
levels(data$newarrhythmia.factor)=c("Yes","No")
levels(data$newnstemi.factor)=c("Yes","No")
levels(data$newstemi.factor)=c("Yes","No")
levels(data$new_hf.factor)=c("Yes","No")
levels(data$new_cva.factor)=c("Yes","No")
levels(data$new_pe.factor)=c("Yes","No")
levels(data$new_dvt.factor)=c("Yes","No")
levels(data$cvs_events_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$does_participant_want_the.factor)=c("Yes","No")
levels(data$withdrawal_complete.factor)=c("Incomplete","Unverified","Complete")
levels(data$sae_v2_v2.factor)=c("Yes","No")
levels(data$severity_v2.factor)=c("Mild","Moderate","Severe")
levels(data$study_intervention_relatio_v2.factor)=c("Definetly related","Possibly related","Not related")
levels(data$expected_v2.factor)=c("Yes","No")
levels(data$serious_adverse_event_v2.factor)=c("Yes","No")
levels(data$aes_complete.factor)=c("Incomplete","Unverified","Complete")
