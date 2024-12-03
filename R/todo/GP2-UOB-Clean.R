############################# # 

# The purpose of this script is to prepare a clean (snapshot/point in time) 
# version of the data held in UOB REDCap for analysis. 
# It is not intended to replace the UOB REDCap data, however, this version
# of data is intended to enable consistent and easily accessible data.  
# 
# Any snapshot data will be extract from UOB REDCap, run through this script,
# and then imported into the clean REDCap dataset. 
# 
# It is intended that this script will grow according to the analyses required.
# Once a variable has been investigated and any manipulation determined it will 
# get added to the script.
# Please inform the data warehouse manager about any new fields added.

# ############################# # 
# VERSION CONTROL
# Current script Date:  26-09-2024
# Current version:      0.52
# 
# Script version, date modified and author, modification made:  
#   -- Add new to beginning of this list --  
# 
#   1.0                   Dataset live
#   0.52  2024-09-26  GO  Added symptom diary fields
#   0.51  2024-09-26  GO  Amended rockwood - dates had been removed from extract so affected code
#   0.5   2024-09-19  GO  Amended date format for vax, asthma, smoking
#   0.4   2024-08-16  GO  Amended ethnicity to character (to align with import)
#   0.3   2024-08-05  GO  Amended ethnicity to make blank any fields with scientific notation
#                         Added presenting symptoms
#   0.2   2024-01-15  GO  Added HEOR set
#   0.1   2024-01-12  GO  Script creation   
#   
# NB: Once dataset been made live in REDCap, any subsequent modifications please
# increase version number goes up by 1, e.g. 2.0, 2.1, 2.2, 3.0 etc

############################# # 
# Input data type: csv
# Output data type: csv
############################# # 

#install.packages("pacman")
# # 0 - Loading libraries ----------
if (!require(required_packages)) install.packages(c("tidyverse", "readr", "janitor","naniar", "scales","skimr","data.table",
                                                    "broom","pacman", "lubridate","openxlsx","svDialogs"),
                                                    dependencies = TRUE, quiet = TRUE)
pacman::p_load(tidyverse,            # data management and visualisation
               broom,
               lubridate,           # working with dates
               skimr,               # get overview of data
               janitor,             # data cleaning and tables
               openxlsx, 
               data.table,          # appending tables etc
               scales,              # for dates in graphs  
               naniar               # to assess and visualize missingness in the data frame
)


############################# # 
# 1 - Directory path ----------
# set path to the directory containing the files
path <- "~/BVC Analysis/GP2 study/Data/"
loc <- paste0("~/BVC Analysis/GP2 study/Data")
ref_path <- "~/BVC Analysis/GP2 study/Data/Reference/"

#---------------------------------------------------# 
# 2 - Reference (Lookup) file(s)

# (a) ETHNICITY DATA lookup table for concept codes
options(scipen = 999)
# eth_ref <- readxl::read_xlsx(paste0(ref_path,"ethnicity_code_list_snowmed.xlsx"),
#                              sheet = "ethnicity_codes_categories") %>%
# clean_names() %>%  #standardise the column name syntax e.g. ad underscores to spaces.
# mutate(eth_concept_id_numeric = as.numeric(code_concept_id)) 
# 
# saveRDS(eth_ref, paste0(path, "Analytical dataset/","ethnicity_concept_reflookup.rds"))
eth_lkup <- readRDS(paste0(path, "Analytical dataset/","ethnicity_concept_reflookup.rds")) 

########################################################## ##
# 3 - Importing numerator data 
# STEP 1) Reading raw snapshot data  --------------
#----------------reading in latest data extract ------------------#
#Opens a dialogue box to select file to import 
# - if nothing happens then Alt-TAB until you find the dialogue box
#NB: Outputs filename and path into global environment
#Numerator
snap_data_raw <- readr::read_csv(snapshot_filename <- file.path(svDialogs::dlg_open("","select file to open", multiple = FALSE, gui = .GUI)$res), #file.choose(), 
                                 guess_max = 10000, show_col_types = F) #Select the excluding dataset


snap_repeat_raw <- readr::read_csv(repeat_filename <- file.path(svDialogs::dlg_open("","select file to open", multiple = FALSE, gui = .GUI)$res), #file.choose(), 
                                 guess_max = 10000, show_col_types = F) #Select the repeated dataset

# 1: consent issues/National opt-out/Do not transfer (full exclusion but may change)
# 2: Yes - participant has been deemed as ineligible (data deletion pending)
#  3: Yes - this is a TEST record
#---------------------------------------------------#
#skimr::skim(snap_data_raw) %>%  view()  

########################################################## ##
# STEP 2) Data cleaning ------ ------

##Using the export report called.....
gp_epidt_in <- snap_data_raw %>% 
  #filter(is.na(exclude_from_analysis))     #excluding from ALL analyses%>% 
  #creating a variable to filter out duplicate or test data ------#
  mutate(exclude = case_when(grepl("test|______",study_id_se, ignore.case = TRUE) ~ 1, #excluding test data
                             !is.na(redcap_repeat_instance) ~ 1, 
                             str_detect(study_id_se,"-") == FALSE ~ 1, #excluding those with incorrect id
                             TRUE ~ 0)) #excluding the communication records that create additional records w no info

gp_snapdata_in <-  gp_epidt_in %>% 
  mutate( 
    #removing the white spaces inside the ids  
    study_id = gsub(" ","",study_id_se),  
    # adding site information extracted from study id and eligible id (for pending) codes ------#
    site_code = ifelse(startsWith(study_id, "77"), "700", 
                       str_extract(string = study_id, pattern = "^[:digit:]{1,1}")),                          
    site_nhs_code = case_when(startsWith(eligible_id, "626") ~ "3", # Concord
                              startsWith(eligible_id, "627") ~ "4", # Courtside
                              startsWith(eligible_id, "628") ~ "5", # Montpelier
                              startsWith(eligible_id, "629") ~ "6", # Pioneer
                              startsWith(eligible_id, "630") ~ "7", # Tyntesfield
                              startsWith(eligible_id, "631") ~ "8", # Wellspring
                              TRUE ~ NA),
    site = case_when(is.na(study_id) ~ site_nhs_code,
                     TRUE ~ site_code), 
    site_order = factor(case_when(is.na(study_id) ~ site_nhs_code,
                                  TRUE ~ site_code), 
                        levels = c(3, 6, 4, 7, 5, 8),
                        labels = c(1, 2, 3, 4, 5, 6))
  ) %>% 
  #select(-c(site_code, site_nhs_code)) %>% 
  # Collated consult date variable (KEY DATE) ------------ ------
#Note index_date is date used for pfizer only
mutate(date_consult = case_when(is.na(indexdate) & !is.na(dc_eligibility_date) ~ as.Date(dc_eligibility_date),
                               # is.na(indexdate) & is.na(study_entry_auto_timestamp) ~ as.Date(study_entry_auto_timestamp), 
                                TRUE ~ as.Date(indexdate)),
       date_incomplete = case_when(!is.na(indexdate) ~ NA ,# "Complete", 
                                   !is.na(dc_eligibility_date) ~ NA, # "Complete",
                                   TRUE ~ 1) # "Incomplete"
) %>% 
  select(record_number_1, sl_id, eligible_id, study_id, study_arm, date_consult, date_incomplete, everything()) #caregiver_id, 

######################################################### ###
# As there is a lot of manipulation that has to occur on the raw RedCAP data 
# The following continues the preparation

# STEP 3) Appending lookup table information ------ ------
#Excluding the automated search 
gp_episodedta_w_lkups <- 
  gp_snapdata_in %>% 
  filter(exclude != 1) %>% 
  mutate(ethnicity_latest_conceptcode = as.character(case_when(str_detect(ethnicity_latest_code_concept_id_1, "00000$") ~ NA,
                                                               str_detect(ethnicity_latest_code_concept_id_1, "^[:digit:]+$") == TRUE ~ 
                                                                 ethnicity_latest_code_concept_id_1,
                                                               ethnicity_latest_code_concept_id_1 == "NI" ~ ethnicity_latest_code_concept_id_1,
                                                               TRUE ~ NA))) %>% 
  #--- Appending ethnicity codes to the ethnic concept id ------ ------
left_join(eth_lkup, by=c("ethnicity_latest_conceptcode" = "code_concept_id")
) %>%  rename(eth_code_term = code_term) %>% 
  select(-c(ethnicity_latest_code_concept_id_1, eth_concept_id_numeric)) 
# Exporting raw single episode analytical data ----
saveRDS(gp_episodedta_w_lkups, paste0(path, "GP2_single_episode_in.rds")) 

########################################################## ##
# STEP 4) Data preparation ------ ------
##  Variable preparation and categorisation will occur in separate
#   data frame for each grouping of data and then joined together
#   at end.  Thereby enabling iterative development of new variables of 
#   interest. 
#Merge all components at end
#gp_episodedta_w_lkups <- readRDS(paste0(path, "GP2_single_episode_in.rds")) 
######### STEP 4A - episode ids  ----------------
gp_dta_prep_base <- gp_episodedta_w_lkups %>%
  # Study arm ------------ ------
mutate(#In order to get subgroups have to use a combo of study_arm and arm from id.
  stdyarm = case_when(#exclude == 1 ~ NA, #| !is.na(exclude_from_analysis) 
                      is.na(study_id) & !is.na(eligible_id) ~ 9 ,
                      TRUE ~ as.numeric(substr(study_id,4,4))),
  #Subgroup creation
  # #study_arm in RedCap is manually assigned by database manager.   
  studyarm_subgrp = case_when(study_arm == 2 & stdyarm == 1 ~ "2", #"Surveillance: Data only",
                              study_arm == 1 & stdyarm == 1 ~ "1" ,#"Embedded",
                              study_arm == 2 & stdyarm == 2 ~ "3", #"Surveillance: CAG",
                              study_arm == 3 & stdyarm == 9 ~ "9", #"Pending",
                              study_arm == 3 ~ "4", #"Anonymised descriptive",
                              study_arm == 0 ~ "9", #"Pending",
                              is.na(study_arm) & !is.na(study_id) ~ as.character(stdyarm),
                              is.na(study_id) & !is.na(date_consult) ~ as.character(stdyarm),
                              TRUE ~ as.character(study_arm))
)  %>% 
  select(c(record_number_1, study_id, 
           sl_id, eligible_id, #caregiver_id,
           date_consult, 
           #site_code, site_nhs_code, 
           site,  site_order,  
           study_arm, studyarm_subgrp) ,
         starts_with("exclude_"))


######### STEP 4B - Study consent  ----------------
gp_dta_prep_consent <- gp_episodedta_w_lkups %>%
  filter(is.na(date_incomplete)) %>% 
  mutate(
        withdrawal = factor(entry_withdrawn,  #removed from data by DBM
                         levels = c(2,1),
                         labels = c(1,0)),
         #Did this patient provide consent for the study? 
         consented = case_when(decline_consent == 2 ~ 2, 
                               data_only == 2 ~ 1, 
                               decline_consent == 1 ~ 1,
                               TRUE ~ 0),
         include_patient = case_when(#is.na(exclude_from_analysis) | 
                                       exclude != 1 | 
                                       consented != 0 ~ 1,
                                     TRUE ~ 0), #Should this participant be included in the Avon CAP GP2 study?
  ) %>% 
  #embedded diagnostic study
  #Consent
  mutate(consent_sympdiary = case_when(symptom_diaries_yesno_se == 2 ~ 1,
                                       TRUE ~ 0),
         consent_confusion_screen = case_when(cmplt_confusion == 2 ~ 1,
                                              TRUE ~ 0),
         consent_qol = case_when(cmplt_qol == 2 ~ 1,
                                 TRUE ~ 0),
         consent_samples_swab = case_when(samples_agreed_se == 1 ~ 0, #"Didnt agree to provide samples",
                                          samples_agreed_se == 2 & #"Agreed to provide samples",
                                            samples_variation_se___1 == 1 &
                                            (is.na(exclude_sample_analysis___1) | exclude_sample_analysis___1 == 0) ~ 1, # Consent given
                                          TRUE ~ 0),
         consent_samples_saliva = case_when(samples_agreed_se == 1 ~ 0, #"Didnt agree to provide samples",
                                            samples_agreed_se == 2 & #"Agreed to provide samples",
                                              samples_variation_se___3 == 1 &
                                              (is.na(exclude_sample_analysis___2) | exclude_sample_analysis___2 == 0) ~ 1, # Consent given
                                            TRUE ~ 0),	
         consent_samples_urine = case_when(samples_agreed_se == 1 ~ 0, #"Didnt agree to provide samples",
                                           samples_agreed_se == 2 & #"Agreed to provide samples",
                                             samples_variation_se___2 == 1 &
                                             (is.na(exclude_sample_analysis___3) | exclude_sample_analysis___3 == 0) ~ 1, # Consent given
                                           TRUE ~ 0),	
         consent_samples_serial = case_when(samples_rsv_consent_yesno == 0 ~ 0, #"Didnt agree to provide samples",
                                            samples_rsv_consent_yesno == 1 & #"Agreed to provide samples",
                                              samples_variation_se___4 == 1 &
                                              (is.na(exclude_sample_analysis___4) | exclude_sample_analysis___4 == 0) ~ 1, # Consent given
                                            samples_rsv_consent_yesno == 1 & #"Agreed to provide samples",
                                              samples_variation_se___4 == 1 &
                                              exclude_sample_analysis___4 == 1 ~ 0, # Consent given
                                            TRUE ~ 0)
  ) %>% 
  select(c(record_number_1, date_consult, include_patient, withdrawal, consented,
           consent_sympdiary, consent_confusion_screen, consent_qol),
         starts_with(c("consent_samples_","short_visit")))

######### STEP 4C - Demographics component ----------------
gp_dta_prep_demo <- gp_episodedta_w_lkups %>%
  filter(is.na(date_incomplete)) %>% 
  # Preparing Demographic variables ------   #
  ########## Age  ----
mutate(age_at_enrolment = case_when(is.na(age_enrol) & 
                                      !is.na(decline_consent_age) ~ decline_consent_age,
                                    is.na(age_enrol) & 
                                      is.na(decline_consent_age) & 
                                      !is.na(age_enrolled) ~ age_enrolled, 
                                    is.na(age_enrol) & 
                                      is.na(decline_consent_age) & 
                                      is.na(age_enrolled) ~ 0 , #Have used 0 as missing
                                    TRUE ~ age_enrol)
) %>%   
  ########## Ethnicity categorisation ------  
# There are differences in the level of information captured between the arm 3 and arm 1.
#as per data dictionary
#consented
# 1	      White British (e.g. Welsh, Irish, English, Scottish, Gypsy or Irish Traveller, etc)
# 2	      White other (e.g. Italian, Polish, Spanish, German, French, Russian, etc)
# 3	      Mixed or multiple ethnic groups (white and black Caribbean, white and black African, white and Asian, any other mixed or multiple ethnic background)
# Black	  black British, Caribbean or African (Caribbean, African, Any other black, black British or Caribbean background)
# 4	      Asian or Asian British (Indian, Pakistani, Bangladeshi, Chinese, any other Asian background)
# 5	      Other ethnic group (Arab, any other ethnic group)
# 6	      Not known
# Declined
# 1	      	White
# 2	      	Ethnic minorities (rather than Black, Asian and minority ethnic (non-White)) ## NB: In March 2021, the Commission on Race and Ethnic Disparities recommended that the government stop using the term BAME. 
# 3	      	Not Known
#
#https://www.gov.uk/government/publications/standards-for-ethnicity-data/standards-for-ethnicity-data
#https://www.ethnicity-facts-figures.service.gov.uk/style-guide/writing-about-ethnicity 
#https://www.ons.gov.uk/peoplepopulationandcommunity/culturalidentity/ethnicity/articles/producingadminbasedethnicitystatisticsforenglandchangestodataandmethods/2022-05-23#exploring-alternative-ethnicity-selection-rules
# ethnicity captured at research visit
mutate(eth_cat_manual = case_when(recorded_ethnicity == 2 & manual_ethnicity %in% ( "1":"3") ~ manual_ethnicity,  
                                  recorded_ethnicity == 2 & manual_ethnicity == "Black" ~ "4",       # "black British, Caribbean or African",
                                  recorded_ethnicity == 2 & manual_ethnicity == "4" ~ "5",           # "Asian or Asian British"
                                  recorded_ethnicity == 2 & manual_ethnicity == "5" ~ "6" ,          # "Other ethnic group"
                                  recorded_ethnicity == 2 & manual_ethnicity == "6" ~  "9" ,         # Not known                           
                                  TRUE ~ manual_ethnicity),
       # ethnicity captured from concept code in EMIS/GP records 
       eth_cat_code =  as.character(ethnic_code_level1),        
       # ethnicity captured from automated search of EMIS/GP records 
       eth_cat_autosearch = case_when(ethnicity_white_british_date_1 == 2 ~ "1",                 # White British
                                      ethnicity_white_other_date_1  == 2 ~ "2",                         # White other 
                                      #missing black search ~ "4",                                      # "black British, Caribbean or African",
                                      ethnicity_asian_or_asian_british_date_1 == 2 ~ "5",               # "Asian or Asian British"
                                      ethnicity_mixed_or_multiple_ethnic_groups_date_1 == 2 ~ "3",      # Mixed or multiple ethnic groups
                                      ethnicity_other_ethnic_group_date_1 == 2 ~ "6" ,                  # "Other ethnic group" 
                                      TRUE ~ "9")  
) %>% 
  mutate(eth_code = as.numeric(case_when(eth_cat_manual %in% ("1":"6") ~ eth_cat_manual,
                                         eth_cat_manual == "9" ~ 
                                           case_when(eth_cat_code %in% ("1":"6") ~ eth_cat_code, 
                                                     TRUE ~ eth_cat_autosearch),
                                         is.na(manual_ethnicity) ~ 
                                           case_when(eth_cat_code %in% ("1":"6") ~ eth_cat_code, 
                                                     TRUE ~ eth_cat_autosearch),
                                         TRUE ~ eth_cat_autosearch)) 
  ) %>% 
  mutate(#As declined consent is only 2 ethnic groups
    eth_code_2gp_consent = case_when(eth_code %in% ("1":"2") ~ 1, # White
                                     eth_code %in% ("3":"6") ~ 2, # Black, Asian and minority ethnic (non-White)
                                     eth_code == "9" ~ 3, #Not Known
                                     TRUE ~ NA), 
    eth_code_2gp_decline = dc_ethnicity, 
    # Grouping to join with declined set 
    ethnic_code_2gp = as.numeric(case_when(study_arm == 3 ~ eth_code_2gp_decline, 
                                           (is.na(eth_code_2gp_consent) | eth_code_2gp_consent == 9) &
                                             eth_code_2gp_decline != 3 ~ eth_code_2gp_decline,
                                           TRUE ~ eth_code_2gp_consent))
  ) %>% 
  ########## gender  ------
mutate(gender_consent = case_when(patient_identifiers_confidential_patient_information_gender_1 == "Female" ~ 2,
                                  patient_identifiers_confidential_patient_information_gender_1 == "Male" ~ 1,
                                  patient_identifiers_confidential_patient_information_gender_1 == "Other" ~ 3,
                                  patient_identifiers_confidential_patient_information_gender_1 == "Missing" ~ 4, #4	Not recorded
                                  TRUE ~ NA),
       gender = case_when(study_arm == 3 ~ dc_gender, 
                          is.na(gender_consent) ~ dc_gender, 
                          TRUE ~ gender_consent)
) %>% 
  ########## Deprivation  ------                     
mutate(dep_decile = as.numeric(case_when(is.na(imd) | imd == "______" ~ 
                                           as.character(dc_deprivation_decile),
                                         !is.na(imd) & imd == "UNMATCHED" ~ "99",
                                         TRUE ~ imd))
) %>% 
  select(record_number_1, date_consult, #study_id, study_arm,
         age_at_enrolment, eth_code, # eth_code_2gp_consent, eth_code_2gp_decline,
         ethnic_code_2gp, gender, dep_decile
  ) 

######### STEP 4D - Clinical diagnosis #########   
gp_dta_prep_clindx <- gp_episodedta_w_lkups %>% 
  filter(is.na(date_incomplete)) %>% 
  rename(acute_illness = acute_ill, #Is this an acute illness (present for 28 days or less)?
         previous_enrolled_particip = prev_enrolled, #Is this a previously enrolled participant within 28 days of the onset of their study qualifying aLRTD illness?
         alrtd_hosp_prev48hrs = alrtd_hosp_hrs,
         symptom_onset_days_preconsult = manual_symp_onset,
         date_symptom_onset = manual_symp_onset_date,
         d30_date_death = d30_date_of_death,
         date_research_visit = research_visit_date
  ) %>%  
  #### Categorising (final) diagnostic information  ------ 
mutate(
  # Consented or CAG arms -----------#        
  final_diag_consent = case_when(d30_final_diagnosis %in% 1:9 ~ 1, 
                                 d30_final_diagnosis == 12 ~ 3,
                                 d30_final_diagnosis %in% 10:11 ~ 2,
                                 d30_final_diagnosis == 13 ~ 9,
                                 TRUE ~ NA),
  final_diag_decline = dc_qualifying_condition, # only 1-3
  #Combined data for each arm together. Note consented has different layout to descriptive. 
  final_diag = case_when(is.na(final_diag_consent) & is.na(final_diag_decline) ~ NA,
                         is.na(final_diag_consent) & !is.na(final_diag_decline) ~ final_diag_decline,
                         (!is.na(final_diag_consent) & !is.na(final_diag_decline)) & 
                           (final_diag_consent == final_diag_decline) ~ final_diag_consent,
                         TRUE ~ final_diag_consent),
  
  clindiag_consent = case_when(d30_final_diagnosis %in% c(1,2,5) ~ d30_final_diagnosis,
                               d30_final_diagnosis == 3 ~ 4,  #change to align with dc_ arm
                               d30_final_diagnosis == 4 ~ 3,
                               d30_final_diagnosis == 6 ~ 13, #	Infective exacerbation of cystic fibrosis => Other aLRTI diagnosis
                               d30_final_diagnosis %in% c(7:9) ~ 6, #grouping pneumonia 
                               d30_final_diagnosis %in% c(10:11) ~ d30_final_diagnosis-1, #the -1 is to align the number with declined consent
                               TRUE ~ d30_final_diagnosis),
  
  clindiag_decline = case_when(dc_clin_diag %in% c(1:6) ~ dc_clin_diag,
                               dc_clin_diag == 7 ~ 13, #Lower respiratory tract infection due to COVID-19 => Other aLRTI diagnosis
                               dc_qualifying_condition == 2 & dc_any_diag___1 == 1 ~ 9, #Non-infective exacerbation of asthma
                               dc_qualifying_condition == 2 & dc_any_diag___2 == 1 ~ 10, #Non-infective exacerbation of COPD
                               dc_qualifying_condition == 2 & dc_any_diag___3 == 1 ~ 11, #Non-infective exacerbation of other lung condition
                               dc_qualifying_condition == 1 & dc_clin_diag == 8 ~ #Other aLRTI diagnosis
                                 case_when(grepl("exacerbation", clin_diag_othr_decline, ignore.case = TRUE) == TRUE & 
                                             grepl("obstructive airways", clin_diag_othr_decline, ignore.case = TRUE) == TRUE ~ 4, 
                                           grepl("exacerbation", clin_diag_othr_decline, ignore.case = TRUE) == TRUE &
                                             grepl("infective | obstructive airways", clin_diag_othr_decline, ignore.case = TRUE) == TRUE ~ 4,
                                           grepl("asthma", clin_diag_othr_decline, ignore.case = TRUE ) == TRUE ~ 3,
                                           grepl("LRTI|respiratory tract infection|post Covid lung infection|possible chest infection", 
                                                 clin_diag_othr_decline, ignore.case = TRUE)  == TRUE ~ 1,
                                           !is.na(clin_diag_othr_decline) ~ 13, #Other aLRTI diagnosis
                                           TRUE ~ 98), #Not known
                               dc_qualifying_condition == 3 ~ 12, 
                               dc_clin_diag == 9 ~ 97,	#None of the above
                               dc_clin_diag == 10 ~ 98,	#Not known
                               TRUE ~ dc_clin_diag),
  
  clin_diag = case_when(is.na(clindiag_consent) & is.na(dc_clin_diag) ~ NA,
                        is.na(clindiag_consent) & 
                          !is.na(dc_clin_diag) ~ dc_clin_diag,
                        TRUE ~ clindiag_consent),
  
  
  # #anonymised descriptive/arm3/ declined consent arm 
  #TO COMPLETE CAP
  # final_diag_wCAP_decline = case_when(!is.na(dc_qualifying_condition) ~
  #                                       case_when(dc_qualifying_condition == 1 & dc_clin_diag == 6 ~ 4, #CAP
  #                                                 dc_qualifying_condition == 1 & dc_clin_diag %in% c(1:5,8) ~ 1, #NP-LRTI (non-pneumonic)
  #                                                 TRUE ~ dc_qualifying_condition),
  #                                     TRUE ~ NA)
  
) %>% 
  mutate(
    #documented non-LRTD diagnosis are made NA and therefore to be excluded from analyses
    doc_LRTD = case_when(!is.na(final_diag) & final_diag != 9 ~ 1,
                         TRUE ~ NA)
  )  %>% 
  rename(dc_clin_diag_othr = clin_diag_othr_decline) %>% 
  select(c(record_number_1, date_consult, doc_LRTD, 
           final_diag, clin_diag, clindiag_consent, clindiag_decline,  
           d30_final_diagnosis, dc_qualifying_condition, dc_clin_diag, dc_clin_diag_othr),
         starts_with(c("dc_any_diag")),
         c(acute_illness, previous_enrolled_particip, alrtd_hosp_prev48hrs, 
           symptom_onset_days_preconsult, date_symptom_onset, date_research_visit, 
           d30_outcome, d30_date_death,  d30_death_lrtd  
         ),
         starts_with(c("d30_radiolog"))
  )
######### STEP 4E - 1 Presenting signs & Symptoms ######### 
gp_dta_prep_presntsymp <- gp_episodedta_w_lkups %>% 
  filter(is.na(date_incomplete)) %>% 
  mutate(
    loc_pt_seen = case_when(is.na(patient_seen) & !is.na(patient_seen_decl) ~ patient_seen_decl,
                            TRUE ~ patient_seen),
    #  exclude_obs = ifelse(!is.na(exclude_sample_analysis___5), 1, 0) #consent issues for obs/exam findings
  ) %>% 
  rename(care_home = patient_carehome) %>% 
  #### Clinical features  -----
  
mutate(
  # severity_illness = case_when(study_arm == 3 ~ dc_eligibility_severity, 
  #                      TRUE ~ d30_lrtd_severity_pc),
  hr = hr_v2, # case_when(is.na(hr_v2) ~ hr_v2_v2, TRUE ~ hr_v2), 
  systolic_bp = systolic_bp_v2, # case_when(is.na(systolic_bp_v2) ~ systolic_bp_v2_v2, TRUE ~ systolic_bp_v2), 
  diastolic_bp = diastolic_bp_v2, # case_when(is.na(diastolic_bp_v2) ~ diastolic_bp_v2_v2, TRUE ~ diastolic_bp_v2),
  # temperature = , #1, Fever (T>38.0°C)|2, Hypothermia (T< 35.5°C)|3, Normal|4, Not recorded
  rr = rr_v2, # case_when(is.na(rr_v2) ~ rr_v2_v2, TRUE ~ rr_v2),  
  resp_30 = resp_30_v3, # case_when(is.na(resp_30_v3) ~ resp_30_v3_v2, TRUE ~ resp_30_v3),
  pulse_ox = pulse_ox_v2, # case_when(is.na(pulse_ox_v2) ~ pulse_ox_v2_v2, TRUE ~ pulse_ox_v2),
 # news_2_total = news_score_v2, # case_when(is.na(news_score) ~ news_score_v2, TRUE ~ news_score),
  height_cm = height_2, # case_when(is.na(height_2) ~ height_2_v2, TRUE ~ height_2),
  height_m = height_cm/100,
  weight_kg = weight_2, # case_when(is.na(weight_2) ~ weight_2_v2, TRUE ~ weight_2)
) %>% 
  mutate(bmi = weight_kg/(height_m*height_m),
         bmi_cat = fct_inorder(cut(bmi, 
                               breaks = c(10, 18.5, 25, 30, 35, 40, 101),
                               labels = c(1,2,3,4,5,6),
                               #labels = c("<18.5","18.5–24.9", "25.0–29.9", "30.0–34.9", "35.0–39.9", "≥40"),
                               include.lowest = T,
                               right = FALSE)),
  ) %>% 
  #### CRB65 score -----
#Source: https://www.nice.org.uk/guidance/qs110/chapter/quality-statement-1-mortality-risk-assessment-in-primary-care-using-crb65-score#definitions-of-terms-used-in-this-quality-statement
#CRB65 score for mortality risk assessment in primary care
# CRB65 score is calculated by giving 1 point for each of the following prognostic features:
# * confusion (abbreviated Mental Test score 8 or less, or new disorientation in person, place or time). 
# * raised respiratory rate (30 breaths per minute or more)
# * low blood pressure (diastolic 60 mmHg or less, or systolic less than 90 mmHg)
# * age 65 years or more.
# Patients are stratified for risk of death as follows:
# * 0: low risk (less than 1% mortality risk)
# * 1 or 2: intermediate risk (1–10% mortality risk)
# * 3 or 4: high risk (more than 10% mortality risk).
mutate(
#Confusion
crbcalc__cnfsn = case_when((!is.na(enrol_amt_score_calc) & enrol_amt_score_calc <= 8) | 
                             (!is.na(confused_recent) & confused_recent == 2) ~ 1,
                           is.na(enrol_amt_score_calc) & is.na(confused_recent) ~ NA,
                           TRUE ~ 0), 
#Respiratory Rate ≥30
crbcalc__rr = case_when(rr >= 30 ~ 1,
                        is.na(rr) ~ NA,
                        TRUE ~ 0),
#Systolic BP <90 mmHg or Diastolic BP ≤60 mmHg
crbcalc__lbp = case_when(systolic_bp < 90 |        # using examination findings
                           diastolic_bp_v2 <= 60 ~ 1,
                         is.na(systolic_bp) & is.na(diastolic_bp_v2) ~ NA,
                         TRUE ~ 0), 
#Age ≥65
age_at_enrolment = case_when(is.na(age_enrolled) & !is.na(decline_consent_age) ~ decline_consent_age,
                             !is.na(age_enrolled) & is.na(decline_consent_age) ~ age_enrolled, 
                             is.na(age_enrolled) & is.na(decline_consent_age) ~ NA , #Have used 0 as missing
                             TRUE ~ age_enrolled),
crbcalc__age_u65 = if_else(age_at_enrolment > 64,1,NA),
crb_obs_done = if_else(!is.na(crbcalc__cnfsn) & !is.na(crbcalc__rr) & 
                        !is.na(crbcalc__lbp) &!is.na(crbcalc__age_u65), 1, 0) 
  ) %>% 
  rowwise() %>% 
  mutate(crb65_score = sum(crbcalc__cnfsn, crbcalc__rr, crbcalc__lbp, crbcalc__age_u65, na.rm=T),
         crb65_score = if_else(crb_obs_done == 1, crb65_score, NA)) %>% 
  ungroup() %>% 
   #### Rockwood compilation -----
mutate(rockwood_frailty_score_date = case_when(is.Date(rockwood_frailty_score_date_1) == FALSE ~ 
                                                 as.Date(rockwood_frailty_score_date_1, "%d-%b-%y", origin = "1899-12-30"),#assuming import from excel issue
                                               TRUE ~ NA) , 
       rockwood_autosearch = str_extract(rockwood_frailty_score_code_term_1, "[0-9]+"),
       rockwood_enrol = case_when(enrol_rw_vfit == 2 ~ 1, 
                                  enrol_rw_well == 2 ~ 2,
                                  enrol_rw_manage == 2 ~ 3,
                                  enrol_rw_vul == 2 ~ 4,
                                  enrol_rw_mildfr == 2 ~ 5,
                                  enrol_rw_modfr == 2 ~ 6,
                                  enrol_rw_score == 0 & str_detect(enrol_rw_notes, "terminal") ~ 9,
                                  enrol_rw_score == 0 & str_detect(enrol_rw_notes, "end of life") ~ 8,
                                  enrol_rw_sevfr == 2 ~ 7,
                                  enrol_rw_score == 0 & str_detect(enrol_rw_notes, '^[0-9]+$') == TRUE ~ as.numeric(enrol_rw_notes),
                                  TRUE ~ NA),
      # rockwood_enrol_date = as.Date(enrolment_rockwood_score_timestamp),
       recorded_rockwood_date  = as.Date(recorded_rockwood_date)
) %>% 
  rowwise() %>% 
  mutate(date_rw_latest = as.Date(ifelse(!all(is.na(recorded_rockwood_date), is.na(rockwood_frailty_score_date)), #is.na(rockwood_enrol_date), 
                                         max(recorded_rockwood_date,rockwood_frailty_score_date, na.rm=T), NA)), # rockwood_enrol_date, 
         rockwood = case_when(recorded_rockwood_date == date_rw_latest ~ as.numeric(manual_rockwood),
                              #rockwood_enrol_date == date_rw_latest ~ as.numeric(rockwood_enrol), 
                              rockwood_frailty_score_date == date_rw_latest ~ as.numeric(rockwood_autosearch),
                              #TRUE ~ NA
                              TRUE ~ rockwood_enrol) ) %>% 
  ungroup() %>% 
  ####  -----
  select(record_number_1, date_consult,care_home, 
           loc_pt_seen, cmplt_exam, research_visit_date, #severity_illness, 
         short_visit_1, 
         starts_with(c("short_visit_cntnt","crb")), 
          bp_check, tymp_temp, 
           hr, systolic_bp, diastolic_bp, rr, resp_30 ,
           pulse_ox, #news_2_total, 
           bmi, bmi_cat, height_measure, height_cm, height_m, weight_measure_2, weight_kg, 
           rockwood, date_rw_latest,
           manual_cough:manual_orthopnoea
           ) %>% 
  rename(short_visit = short_visit_1,
         short_visit_samplesonly = short_visit_cntnt___1,
         short_visit_sympdiaryonly = short_visit_cntnt___2)
 
####  Charlson comorbidity index (cci)  ----------------- 
# TO DO 
gp_dta_prep_cci <- gp_episodedta_w_lkups %>% 
  filter(is.na(date_incomplete)) %>% 
  select(record_number_1, date_consult, 
         starts_with(c("cc_", "cc20_", "co_"))
          ) %>% 
  mutate(across(contains("date"), dmy))

####  Cambridge multimorbidity ----------------
# TO DO 
gp_dta_prep_cam <- gp_episodedta_w_lkups %>% 
  filter(is.na(date_incomplete)) %>% 
  select(c(record_number_1, date_consult),
         starts_with("cam")) %>% 
  mutate(across(contains("date"), dmy)) #accounting for multiple formats in date i.e. - vs /
  

####  Covid questionnaire ----------------
# TO BE COMPLETED  
gp_dta_prep_covid <- gp_episodedta_w_lkups %>%  
  filter(is.na(date_incomplete)) %>% 
  rename(covid_under5 = under5,
         covid_five_17y = five_17y,
         covid_yes_healthcare_worker = yes_healthcare_worker) %>% 
  select(record_number_1, date_consult,
         covid_under5, covid_five_17y, covid_adults, covid_yes_healthcare_worker)

#D1 samples  

######### STEP 4F - Respiratory Samples ####
  ##### - Respiratory samples - Swab and saliva ----------------
gp_dta_prep_sample_resp <- gp_episodedta_w_lkups %>% 
  filter(!is.na(sample_type) | !is.na(saliva_v2) ) %>% 
  ##### - Sample collection type and dates -------
#some are missing swab collection dates but have a storage date (using as proxy)
# (1 = collected, 0 = not collected)
# Lab samples for exclusion from specimen-specific analyses should be flagged (as Y/1) 
# within RedCAP, specifically in the exclude_swab, exclude_saliva, and exclude_urine. 

mutate(lab_sample_type_resp = sample_type,
       resp_sample_provided = case_when(samples_agreed_se == 2 & 
                                        (!is.na(np_date_2_v2) | !is.na(saliva_date_v2)) ~ 1,
                                      samples_agreed_se == 2 & !is.na(adm_samp_store_datetime) & 
                                        (np_swab_v2 == 2 & is.na(np_date_2_v2)) | 
                                        (saliva_v2 == 2 & is.na(saliva_date_v2)) |
                                        (adm_ur_taken_v2 == 2 & is.na(adm_ur_date_v2)) ~  1,
                                      samples_agreed_se == 1 ~ NA, 
                                      TRUE ~ NA),
       
    # -- swab
      sample_np = case_when(exclude_sample_analysis___1 != 1 & 
                               resp_sample_provided == 1 & np_swab_v2 == 2 ~ 1, 
                                     TRUE ~ 0),
        sample_date_np = case_when(exclude_sample_analysis___1 != 1 & 
                                     np_swab_v2 == 2 & is.na(np_date_2_v2) & 
                                          !is.na(adm_samp_store_datetime) ~ adm_samp_store_datetime, 
                                        TRUE ~ np_date_2_v2),
        # -- saliva
        sample_saliva = case_when(exclude_sample_analysis___2 != 1 & 
                                    resp_sample_provided == 1 & saliva_v2 == 2 ~ 1, TRUE ~ 0),
        sample_date_saliva = case_when(exclude_sample_analysis___2 != 1 & 
                                         saliva_v2 == 2 & is.na(saliva_date_v2) & 
                                         !is.na(adm_samp_store_datetime) ~ adm_samp_store_datetime, 
                                       TRUE ~ saliva_date_v2),
        #May change to numeric values
        sample_provided_resptype = case_when(is.na(sample_date_np) & is.na(sample_date_saliva) ~ "Neither swab nor saliva provided", 
                                             exclude_sample_analysis___1 != 1 & !is.na(sample_date_np) & 
                                               is.na(sample_date_saliva) ~ "Nose/throat swab",
                                              exclude_sample_analysis___2 != 1 & is.na(sample_date_np) & 
                                               !is.na(sample_date_saliva) ~ "Saliva",       
                                             exclude_sample_analysis___1 != 1 & !is.na(sample_date_np) & 
                                               exclude_sample_analysis___2 != 1 & !is.na(sample_date_saliva) ~ "Both swab and saliva",
                                                 samples_agreed_se == 1 ~ "Declined to give samples", 
                                                 TRUE ~ NA)
        ) %>% 
  mutate(
    ##### - Research visit date ------
    lab_viraltest_date_sample_collection = case_when(!is.na(sample_date_np) ~ as.Date(sample_date_np),
                                       is.na(sample_date_np) & !is.na(sample_date_saliva) ~ as.Date(sample_date_saliva),
                                       TRUE ~ as.Date(adm_samp_store_datetime))
  ) %>% 
  ##### - Lab results ----------------------------------------------
    mutate(sample_sent_lab = case_when(exclude_sample_analysis___2 == 1 | 
                                         exclude_sample_analysis___1 == 1 ~ NA, 
                                       !is.na(lab_sample_type_resp) | !is.na(rsv_saliva) ~ 1, 
                                       TRUE ~ adm_samp_trans) ) %>%
      ###### - A) --- Defining lab tested ------
    mutate(lab_viraltest_sample_type = case_when(exclude_sample_analysis___2 == 1 ~ "Nose/throat swab", #May change to numeric values
                                                    exclude_sample_analysis___1 == 1 ~ "Saliva",
                                                    TRUE ~ sample_provided_resptype),
           lab_viraltest_sample_provided = case_when(is.na(sample_date_np) & is.na(sample_date_saliva) ~ 0, 
                                                     samples_agreed_se == 1 ~ 0, 
                                                     exclude_sample_analysis___2 == 1 & exclude_sample_analysis___1 == 1 ~ 0, 
                                                     TRUE ~ 1),
           lab_viraltest_sample_type_resp = case_when(!is.na(lab_sample_type_resp) & 
                                                        (is.na(exclude_sample_analysis___1) | 
                                                           exclude_sample_analysis___1 == 0) ~ lab_sample_type_resp, #1: NP, 2: OP,3: ON
                                              !is.na(sample_type_resp_covid) & 
                                                (is.na(exclude_sample_analysis___1) | 
                                                   exclude_sample_analysis___1 == 0) ~ sample_type_resp_covid,
                                            TRUE ~ NA), 
          #Does episode have test result / lab finding  (1=tested, 0=not tested) #LAB_tested
          lab_viraltest_done = factor(case_when(exclude_sample_analysis___1 == 1 | 
                                                 exclude_sample_analysis___2 == 1 ~ 0,
                                        !is.na(lab_viraltest_sample_type_resp) | !is.na(rsv_saliva) ~ 1, 
                                        (!is.na(sample_sent_lab) & is.na(lab_viraltest_sample_type_resp) & 
                                           is.na(rsv_saliva)) ~ 0,
                                        TRUE ~ NA)),
           rsv_saliva = ifelse(grepl('\\*$', rsv_saliva),gsub('\\*', '', rsv_saliva),rsv_saliva),
           flu_np = case_when(!is.na(as.numeric(flua_np)) ~ flua_np,
                              !is.na(as.numeric(flub_np)) ~ flub_np, 
                              TRUE ~ flua_np), 
           flu_saliva = case_when(!is.na(as.numeric(flua_saliva)) ~ flua_saliva,
                                  !is.na(as.numeric(flub_saliva)) ~ flub_saliva, 
                                  TRUE ~ flua_saliva),
           paraflu_np = case_when(!is.na(as.numeric(paraflu1_np)) ~ paraflu1_np,
                                  !is.na(as.numeric(paraflu2_np)) ~ paraflu2_np, 
                                  !is.na(as.numeric(paraflu3_np)) ~ paraflu3_np, 
                                  !is.na(as.numeric(paraflu4_np)) ~ paraflu4_np, 
                                  TRUE ~ paraflu1_np),
           paraflu_saliva = case_when(!is.na(as.numeric(paraflu1_saliva)) ~ paraflu1_saliva,
                                      !is.na(as.numeric(paraflu2_saliva)) ~ paraflu2_saliva, 
                                      !is.na(as.numeric(paraflu3_saliva)) ~ paraflu3_saliva, 
                                      !is.na(as.numeric(paraflu4_saliva)) ~ paraflu4_saliva, 
                                      TRUE ~ paraflu1_saliva)
    ) %>% 
  rename(covid_np = cq_resp_covid, 
         covid_saliva = cq_sal_covid,
         h5n1_conf_np = h5n1_np_conf,
         h7n9_conf_np = h7n9_np_conf) %>% 
  select(c(record_number_1, date_consult, resp_sample_provided),
         starts_with(c("lab_","sample_", "lab_viraltest_")),
         ends_with(c("_np","_saliva"))
        ) %>% 
  select(!starts_with(c("sample_destroyed_", "sample_destruction_"))) 
  #drop_na(date_consult)  

######### STEP 4F2 - Urine samples (TO BE COMPLETED) ----------------
gp_dta_prep_sample_urine <- gp_episodedta_w_lkups %>% 
  # -- urine     
mutate(sample_provided = as.factor(case_when(exclude_sample_analysis___3 != 1 ~ 
                                               case_when(
                                              samples_agreed_se == 2 & !is.na(adm_ur_date_v2) ~ 1,
                                              samples_agreed_se == 2 & !is.na(adm_samp_store_datetime) & 
                                               (adm_ur_taken_v2 == 2 & is.na(adm_ur_date_v2)) ~ 1,
                                               samples_agreed_se == 1 ~ NA), 
                                               TRUE ~ NA)),
      sample_urine = as.factor(ifelse(sample_provided == 1 & adm_ur_taken_v2 == 2, 1, NA)),
      #There are 4 were a sample was to be collected later - no date information available
      sample_date_urine = case_when(sample_provided == 1 & adm_ur_taken_v2 == 2 & 
                                      is.na(adm_ur_date_v2) & 
                                      !is.na(adm_samp_store_datetime) ~ adm_samp_store_datetime, 
                                    sample_provided == 1 & adm_ur_taken_v2 == 2 & 
                                      !is.na(adm_ur_date_v2) ~ adm_ur_date_v2,
                                    TRUE ~ NA)
        ) %>% 
  select(record_number_1, date_consult, sample_urine, sample_date_urine,
           contains(c("urine_spec","nax","NAX")), 
           ends_with("_UAD")) %>% 
  filter(!is.na(sample_urine))
        

######### STEP 4G - health behaviours (TO FIX)  ----------------
gp_dta_prep_behav <- gp_episodedta_w_lkups %>%  
  filter(is.na(date_incomplete)) %>% 
   # Health behaviours
  #lifestyle related  -----------------#
  #issue - mixed format for automated search dates - %d/%m/%Y or %d-%b-%y --
  mutate(across(contains("date"), dmy)) %>% 
  mutate(date_smoke_current = format(smoking_current_smoker_date_1,"%Y-%m-%d"),
       date_smoke_ex = format(smoking_ex_smoker_date_1,"%Y-%m-%d"),
       date_smoke_never = format(smoking_never_smoked_date_1,"%Y-%m-%d")
        ) %>% 
  rowwise() %>% 
  mutate(date_smoke_ever_max = max(date_smoke_current, date_smoke_ex, na.rm=T),
         smokestat = case_when(date_smoke_current == date_smoke_ever_max ~ 2,
                               date_smoke_ex == date_smoke_ever_max ~ 1, 
                               is.na(date_smoke_current) &  is.na(date_smoke_current) & 
                                 !is.na(date_smoke_never) ~ 3,
                               TRUE ~ NA)
          ) %>% 
  ungroup() %>% 
  mutate(smoke_status = case_when(manual_smoking == smokestat ~ manual_smoking,
                                  manual_smoking == 3 & smokestat %in% c(1,2) ~ smokestat,
                                  manual_smoking == 4 & !is.na(smokestat) ~ smokestat,
                                  is.na(manual_smoking) & !is.na(smokestat) ~ smokestat,
                                  TRUE ~ manual_smoking),
         smoking_status_fct = factor(smoke_status,
                                     levels = c(3, 2, 1, 4),
                                     labels = c("Never smoked","Current smoker","Ex-smoker","Not recorded"))
  ) %>% 
    select(c(record_number_1, date_consult, smoke_status),
         contains(c("alcohol_","vape","vaping"))) %>% 
  select(!starts_with(c("cam20_","cc20"))) 

######### STEP 4H - vaccination history  -----------
gp_dta_prep_vaxhist <- gp_episodedta_w_lkups %>%
  filter(is.na(date_incomplete)) %>% 
  #Determining most recent vaccination event (where applicable) -----------#  #Amended 26/09/2024 GO
  rowwise() %>% 
  mutate(date_latest_vax_flu = max(format(dmy(influenza_vaccination_date_1), format = "%Y-%m-%d"), 
                                   format(dmy(influenza_vaccination_date_2), format = "%Y-%m-%d"), 
                                   format(dmy(influenza_vaccination_date_3), format = "%Y-%m-%d"),
                                   format(dmy(influenza_vaccination_date_4), format = "%Y-%m-%d"),
                                   format(dmy(influenza_vaccination_date_5), format = "%Y-%m-%d"), na.rm=T) ,
         date_latest_vax_covid = max(format(dmy(covid_vaccination_history_date_1), format = "%Y-%m-%d"), 
                                     format(dmy(covid_vaccination_history_date_2), format = "%Y-%m-%d"), 
                                     format(dmy(covid_vaccination_history_date_3), format = "%Y-%m-%d"), 
                                     format(dmy(covid_vaccination_history_date_4), format = "%Y-%m-%d"), 
                                     format(dmy(covid_vaccination_history_date_5), format = "%Y-%m-%d"), 
                                     format(dmy(covid_vaccination_history_date_6), format = "%Y-%m-%d"),
                                     format(dmy(covid_vaccination_history_date_7), format = "%Y-%m-%d"),
                                     format(dmy(covid_vaccination_history_date_8), format = "%Y-%m-%d"),
                                     format(dmy(covid_vaccination_history_date_9), format = "%Y-%m-%d"),
                                     format(dmy(covid_vaccination_history_date_10), format = "%Y-%m-%d"), na.rm=T),
         date_latest_vax_pneumo = max(format(dmy(pneumococcal_vaccination_date_1), format = "%Y-%m-%d"), 
                                      format(dmy(pneumococcal_vaccination_date_2), format = "%Y-%m-%d"), 
                                      format(dmy(pneumococcal_vaccination_date_3), format = "%Y-%m-%d"), 
                                      format(dmy(pneumococcal_vaccination_date_4), format = "%Y-%m-%d"), 
                                      format(dmy(pneumococcal_vaccination_date_5), format = "%Y-%m-%d"), 
                                      na.rm=T) ,
  ) %>% 
  ungroup() %>% 
   select(c(record_number_1, date_consult),
          starts_with("date_latest_vax_"),
          contains("_vaccination_")
   ) %>% 
  select(-contains(c("vaccination_history_concept", "vaccination_concept")))
  
  
######### STEP 4I - (chronic) comorbidities / Long-term conditions -----------
gp_dta_prep_comorb <- gp_episodedta_w_lkups %>%
  filter(is.na(date_incomplete)) %>% 
mutate(
  comorbid_asthma = if_else(!is.na(cc20_asthma_date) & # == 2 &  
                              !is.na(cc20_asthma_medication_issued_past_12_months_date_of_issue), 
                                  1, NA), # in line with cambridge comorbid index
       comorbid_COPD = if_else(!is.na(cc20_copd_date), 1,  NA), # == 2, 
       comorbid_bronchiectasis = if_else(!is.na(cc_bronchiectasis_date), 1,  NA),   # == 2, 
       comorbid_other_lung = case_when(!is.na(cc_pulmonary_fibrosis_date) ~ 1, # == 2# "comorbid: Other chronic lung condition",  
                                       # !is.na(cc_other_resp_condition) ~ 1, # == 2  ~ 1, 
                                        !is.na(cc_cystic_fibrosis_date) ~ 1, # == 2 ~ 1, # cystic fibrosis, 
                                        !is.na(cc_pulmonary_hypertension_date) ~ 1, # == 2 ~ 1, # pulmonary hypertension, 
                                        !is.na(cc_chronic_pleural_disease_date) ~ 1, # == 2 ~ 1, # chronic pleural disease
                                      TRUE ~ NA),
       comorbid_heartfailure  = ifelse(!is.na(cc20_congestive_heart_failure_date), 1,  NA),   # == 2,
       #### Diabetes - still issues with dq by type  ----
       comorbid_diabetes = case_when(!is.na(cc20_diabetes_date) ~ 1, # "comorbid: Diabetes",
                                       !is.na(co_type1_diabetes_no_complications_date) | 
                                         !is.na(co_type1_diabetes_complications_present_date) ~
                                         1, # "comorbid: Diabetes",
                                       !is.na(co_type2_diabetes_complications_present_date) |
                                         !is.na(co_type2_diabetes_no_complications_date) ~
                                         1, # "comorbid: Diabetes",  
                                       !is.na(co_diabetic_insulin_date_of_issue)  ~
                                         1, # "comorbid: Diabetes",
                                       TRUE ~ NA)
        ) %>% 
       #### Chronic kidney disease (CKD) ----
       # https://www.nhs.uk/conditions/kidney-disease/diagnosis/
       #   eGFR results are given as a stage from 1 of 5:
       # stage 1 (G1) – a normal eGFR above 90ml/min, but other tests have detected signs of kidney damage
       #  stage 2 (G2) – a slightly reduced eGFR of 60 to 89ml/min, with other signs of kidney damage
       #  stage 3a (G3a) – an eGFR of 45 to 59ml/min
       #  stage 3b (G3b) – an eGFR of 30 to 44ml/min
       #  stage 4 (G4) – an eGFR of 15 to 29ml/min
       #  stage 5 (G5) – an eGFR below 15ml/min, meaning the kidneys have lost almost all of their function
       mutate(cc20_egf_2_most_recent_date_1 = format(dmy(cc20_egf_2_most_recent_date_1),"%Y-%m-%d"),
              cc20_egf_2_most_recent_date_2 = format(dmy(cc20_egf_2_most_recent_date_2),"%Y-%m-%d")) %>% 
         rowwise() %>% 
         mutate(date_egf_latest = max(cc20_egf_2_most_recent_date_1 , cc20_egf_2_most_recent_date_2, na.rm=T), 
                ckd_egf = case_when(cc20_egf_2_most_recent_date_1 == date_egf_latest ~ as.numeric(cc20_egf_2_most_recent_value_1),
                                    cc20_egf_2_most_recent_date_2 == date_egf_latest ~ as.numeric(cc20_egf_2_most_recent_value_2), 
                                    TRUE ~ NA)) %>% 
         ungroup() %>% 
         select(-c(cc20_egf_2_most_recent_date_1, cc20_egf_2_most_recent_date_2,
                   cc20_egf_2_most_recent_code_term_1, cc20_egf_2_most_recent_code_term_2)) %>%
         mutate(
           ckd_stage = case_when(ckd_egf < 15 ~ 5,
                                 ckd_egf < 30 ~ 4,
                                 ckd_egf < 60 ~ 3, #combines 3a and b
                                 ckd_egf < 90 ~ 2,
                                 ckd_egf >=90 ~ 1,
                                 TRUE ~ NA),
           ckd_stage_full = case_when(ckd_egf < 15 ~ "Stage 5",
                                      ckd_egf < 30 ~ "Stage 4",
                                      ckd_egf < 60 ~ "Stage 3a", 
                                      ckd_egf < 45 ~ "Stage 3b", 
                                      ckd_egf < 90 ~ "Stage 2",
                                      ckd_egf >=90 ~ "Stage 1",
                                      TRUE ~ NA),
           #Hosp version - 1, No | 2, Mild (CKD 1-3) | 3, Moderate or Severe CKD (CKD 4-5, ESRF, Dialysis)
           comorbid_ckd_hosp_version = case_when(ckd_egf < 30 ~ 3,
                                                  ckd_egf < 90 ~ 2,
                                                  ckd_egf >=90 ~ 1,
                                                  TRUE ~ NA),
         ) %>% 
  ####  ----
   select(c(record_number_1, date_consult),
          starts_with(c("comorbid_","ckd_","date_egf"))
          ) 

######### STEP 4J - consult in-hours related (From repeated measure TO ADD) -----------
#Day 30 Manual Review Inhours Primary Care#
# gp_repeatdta_w_lkups %>%
#   filter(is.na(date_incomplete)) %>% 
#   select(record_number_1, date_consult, ends_with("_pc")) %>%  view()

######### STEP 4K - consult out-hours related (From repeated measure TO ADD) -----------
#Day 30 Manual Review Outofhours Primary Care#
# gp_repeatdta_w_lkups %>%
#   filter(is.na(date_incomplete)) %>% 
#   select(record_number_1, date_consult, ends_with("_ooh")) %>%  view()


######### STEP 4L - consult Ed Attendance (From repeated measure TO ADD) -----------
#Day 30 Manual Review Ed Attendance#
# gp_repeatdta_w_lkups %>%
#   filter(is.na(date_incomplete)) %>% 
#   select(record_number_1, date_consult, ends_with("_ed")) %>%  view()

######### STEP 4M - Hospitalisation related (From repeated measure TO ADD) -----------
#Day 30 Manual Review Hospital Admissions#
# gp_repeatdta_w_lkups %>%
#   filter(is.na(date_incomplete)) %>% 
#   select(record_number_1, date_consult, ends_with("_ha")) %>%  view()

######### STEP 4N - consult Outpatients (From repeated measure TO ADD) -----------
#Day 30 Manual Review Hospital Outpatients#
# gp_repeatdta_w_lkups %>%
#   filter(is.na(date_incomplete)) %>% 
#   select(record_number_1, date_consult, ends_with("_op")) %>%  view()





######### STEP 4O - Health economics related -----------
gp_dta_prep_heor <- gp_episodedta_w_lkups %>%
  filter(is.na(date_incomplete)) %>% 
  select(record_number_1, date_consult,
         cmplt_qol, why_not_2, other_reason_2, worst_day_q, worst_day_date, 
           date_7, date_14, date_21, date_28, 
         starts_with(c("onset_of_symptoms","manual_symp_onset")),
        # enrol_symp_date, symptom_diary_method_esdc ,#How would they prefer to complete the symptom diary? #sd_completed_esdc_v2
         # online ppts there is the instrument to activate the online symptom diary that has two fields sd_send_esdc_2 and enrolment_send_first_diary_complete
        # #enrolment_send_first_diary_complete), 
         #QALY-specific
         starts_with(c("sd_send_esdc","eq5d", "symp_d_w",  
                       "symp_[0-9]_q","symp_[0-9]_c", "symp_[0-9]wk_c",
                       "symp_[0-9]+m_q", "symp_[0-9]+m_c"
                       # "symp_4m_q", "symp_4m_c",
                       # "symp_5m_q", "symp_5m_c",
                       # "symp_6m_q", "symp_6m_c",
                       # "symp_9m_q", "symp_9m_c",
                       # "symp_12m_q", "symp_12m_c"
                       ))
         ) %>% 
      rename(symp_d7_compl = date_7, 
             symp_d14_compl = date_14, 
             symp_d21_compl = date_21, 
             symp_d28_compl = date_28) 

######### STEP 4P - Symptom diary related (excluding follow-up diary) -----------
# Added GO 26/09/2024
gp_dta_prep_sympdiary <- gp_episodedta_w_lkups %>%
  filter(is.na(date_incomplete)) %>% 
  select(record_number_1, date_consult, #symptom_diary_method_esdc, sd_completed_esdc, #Last 2 missing from extract 26/9/24
         diary_day1_yesno,
          matches(c("^symp_d_wk[0-9]+_c","^date_[0-9]+","^cough_[0-9]+","^phlegm_[0-9]+",
                        "^short_breath_[0-9]+","^wheeze_[0-9]+","^block_nose_[0-9]+",
                        "^fever_[0-9]+","^interfere_[0-9]+","^dist_sleep_[0-9]+",
                    "^chest_pain_[0-9]+","^muscle_ache_[0-9]+","^headache_[0-9]+",
                    "^gen_unwell_[0-9]+","^symptom_diary_")) ) 






# JOINING DATASETS ###########################################################
# merge all data frames together
# this allows for iterative development. 

gp_df_list <- list(gp_dta_prep_base, 
                   gp_dta_prep_demo, 
                   gp_dta_prep_clindx,
                   gp_dta_prep_presntsymp,
                   gp_dta_prep_cci,
                   gp_dta_prep_cam,
                   gp_dta_prep_behav, 
                   gp_dta_prep_comorb,
                   gp_dta_prep_vaxhist, 
                   gp_dta_prep_sample_resp, 
                   gp_dta_prep_sample_urine, 
                   gp_dta_prep_covid,
                   gp_dta_prep_heor,
                   gp_dta_prep_sympdiary
                    )   

GP_dta_prep_clean <- gp_df_list %>% 
                reduce(full_join, by=c('record_number_1', 'date_consult')) #%>% 
                # filter(!is.na(doc_LRTD)) 
  
GP_clean_data <- GP_dta_prep_clean 
  
# exporting ###########################################################
#--D) exporting full numerator set with calculated fields ---------------------------- --
 saveRDS(GP_clean_data, paste0(path, "Analytical dataset/","GP2_analytical_set.rds"))
 write.csv(GP_clean_data, paste0(path, "Analytical dataset/","GP2_analytical_set.csv"), row.names = F, na="")

########################################################### #
  