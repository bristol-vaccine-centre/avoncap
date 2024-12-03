library(tidyverse)

avoncap::set_input(path = "~/Data/avoncap")

tmp=avoncap::load_data("admission-dates") %>%
  transmute(
    record_number,
    admission_date.true = as.Date(admission_date,format="%d/%m/%Y"))



# The avoncap data files
raw = avoncap::load_data("avoncap-export","central")

tmp2 = raw %>%
  select(record_number,enrollment_date,year,study_year,week_number,week_number_iso) %>%
  left_join(tmp, by="record_number")

tmp4 = tmp2 %>% group_by(year,study_year,week_number,admission_date.true) %>%
  count() %>%
  mutate(
    start_of_week = admission_date.true - as.numeric(admission_date.true-4) %% 7
  )

tmp4 %>% view()

tmp5 = tmp4 %>%
    group_by(year,week_number,start_of_week) %>%
    summarise(n=sum(n)) %>%
    group_by(year,week_number) %>%
    arrange(desc(n)) %>%
    filter(row_number()==1) %>%
    arrange(year,start_of_week,week_number) %>%
    filter(
      !is.na(start_of_week),
      year <= 2023 | week_number <= 31,
      year > 2020 | week_number >= 31
    ) %>%
    mutate(
      study_week = pmax(0,study_week(start_of_week)),
      start_of_week = start_date_of_week(study_week)
    )

year_week_number_lookup = tmp5 %>% select(-n)


# these should both be empty:
year_week_number_lookup %>% group_by(year,week_number) %>% count() %>% filter(n>1)
setdiff(full_seq(year_week_number_lookup$study_week,1), year_week_number_lookup$study_week)


usethis::use_data(year_week_number_lookup,overwrite = TRUE)





# tmp3  = tmp2 %>%
#   .reconstruct_admission_date() %>%
#   rename(admission_date.wrong = admission_date) %>%
#   mutate(diff = as.integer(admission_date.true - admission_date.wrong)) %>%
#   .reconstruct_admission_date2() %>%
#   rename(admission_date.test = admission_date) %>%
#   mutate(diff2 = as.integer(admission_date.true - admission_date.test))
#
# #tmp3 %>% filter(diff<0 | diff >= 7) %>% ggplot()+geom_histogram(aes(x=admission_date.true),binwidth = 1)
#
# tmp3 %>% filter(diff2<0 | diff2 >= 7) %>% ggplot()+geom_histogram(aes(x=admission_date.true),binwidth = 1)
#
#
# tmp3 %>% glimpse()
#
# tmp3  %>%
#   mutate(diff = as.integer(admission_date.original - admission_date)) %>%
#   filter(diff < 0 | diff >= 7) %>% view()
#
# source("~/Git/avoncap/R/dispatch-normalise.R")
# tmp3  = tmp2 %>% .reconstruct_admission_date()
# tmp3  = tmp2 %>% rename(admission_date.original = admission_date) %>% .reconstruct_admission_date()
# tmp3 %>% mutate(admission_date.original = as.Date(admission_date.original,format="%d/%m/%Y")) %>%
# mutate(diff = as.integer(admission_date.original - admission_date)) %>%
# filter(diff < 0 | diff >= 7) %>% view()
# error = tmp3 %>% mutate(admission_date.original = as.Date(admission_date.original,format="%d/%m/%Y")) %>%
# mutate(diff = as.integer(admission_date.original - admission_date)) %>%
# filter(diff < 0 | diff >= 7) %>% view()
# error = tmp3 %>% mutate(admission_date.original = as.Date(admission_date.original,format="%d/%m/%Y")) %>%
# mutate(diff = as.integer(admission_date.original - admission_date))
# ggplot(error)+geom_histogram(aes(x=diff))
# ggplot(error)+geom_histogram(aes(x=diff),binwidth = 1)
# error %>% filter(diff<0 | diff >= 7) %>% view()
#
# error %>% filter(diff<0 | diff >= 7) %>% ggplot()+geom_histogram(aes(x=diff),binwidth = 1)
#
# tmp3 %>% filter(diff<0 | diff >= 7) %>% ggplot()+geom_histogram(aes(x=admission_date.true),binwidth = 1)
# tmp3 %>% filter(diff2<0 | diff2 >= 7) %>% ggplot()+geom_histogram(aes(x=admission_date.true),binwidth = 1)
#
