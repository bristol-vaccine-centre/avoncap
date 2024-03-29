#
# ## TEST
#
# tmp1 = data_raw %>% dplyr::select(record_number,admission_date) %>% dplyr::mutate(
#   canonical.admission.date = as.Date(admission_date,"%d/%m/%Y"),
#   canonical.study_week = study_week(canonical.admission.date),
#   canonical.year = lubridate::year(canonical.admission.date)
# )
#
# tmp2 = lrtd_norm %>% dplyr::select(
#   record_number = admin.record_number,
#   inferred.admission.date = admission.date,
#   admission.year,
#   admission.study_week
# )
#
# # TODO: concrete tests
#
# countCompare = tmp1 %>% dplyr::group_by(study_week = canonical.study_week) %>% dplyr::count() %>% dplyr::inner_join(
#   tmp2 %>% dplyr::group_by(study_week = admission.study_week) %>% dplyr::count(),
#   by="study_week",
#   suffix=c(".nhs",".bri")
# )
#
# countCompare %>% glimpse()
#
#
# # comparing processed with canonical based on study_week - 63 rows
# tmp2 %>% dplyr::inner_join(tmp1, by="record_number") %>% dplyr::filter(abs(admission.study_week - canonical.study_week) > 1)
#
# # comparing raw BRI vs raw NHS based on study week
# tmp3 = lrtd_data %>% dplyr::select(record_number, bri.year = year, bri.week_number = week_number, enrollment_date) %>%
#   dplyr::mutate(bri.study_week = (bri.year-2020)*52+bri.week_number)
#
# # same 63 rows
# tmp3 %>% dplyr::inner_join(tmp1, by="record_number") %>% dplyr::filter(abs(bri.study_week - canonical.study_week) > 1)
#
#
# tmp3 %>% dplyr::inner_join(tmp1, by="record_number") %>% dplyr::filter(abs(as.integer(enrollment_date - canonical.admission.date)) > 7)
#
# # not a vast number of patients missing enrollment_date
# # lrtd_aug %>% dplyr::filter(is.na(admin.enrollment_date)) %>% dplyr::count()
# # more missing admission date though
# # lrtd_aug %>% dplyr::filter(is.na(admission.date)) %>% dplyr::count()
#
# # how many are missing year
# lrtd_aug %>% dplyr::filter(is.na(admission.year)) %>% dplyr::count()
# # how many are missing week
# lrtd_aug %>% dplyr::filter(is.na(admission.study_week)) %>% dplyr::count()
# # This is because of missing data in week_number in the raw data.
# # enrollment data present for a good number of those missing week_number,
# # we coudl possibly infer it
# lrtd_data %>% dplyr::filter(is.na(week_number) & !is.na(enrollment_date)) %>% dplyr::count()
#
# # There is a mismatch between study_week (as calculated from week_number and year) and
# # enrollment_date. This seems to be catch up enrollment
# lrtd_aug %>% dplyr::mutate(
#     enrollment_week = study_week(admin.enrollment_date),
#   ) %>%
#   dplyr::filter(abs(admission.study_week - enrollment_week) > 2) %>%
#   dplyr::group_by(enrollment_week) %>%
#   dplyr::count()
#
# # In a small minority of cases the enrollment is more than a week earlier that the admission
# # These are likely data quality issues either with the week_number or with the enrollment_date
# lrtd_aug %>% dplyr::mutate(
#   enrollment_week = study_week(admin.enrollment_date),
# ) %>%
#   dplyr::filter(abs(admission.study_week - enrollment_week) > 2) %>%
#   dplyr::filter(admission.study_week > enrollment_week+1) %>%
#   dplyr::count()
#
# lrtd_aug %>% dplyr::mutate(
#   enrollment_year = study_week(admin.enrollment_date),
# ) %>% dplyr::filter(abs(admission.study_week - enrollment_week) > 2) %>%
#   dplyr::group_by(enrollment_week) %>% dplyr::count()