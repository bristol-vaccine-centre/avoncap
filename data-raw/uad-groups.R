library(tidyverse)
usethis::proj_set("~/Git/avoncap/")
devtools::load_all("~/Git/avoncap/")
here::i_am("data-raw/uad-groups.R")
## code to prepare `uad_groups` dataset goes here

# The spreadsheet data
pcv_map = readxl::read_excel("~/Git/avoncap/data-raw/serotype-data.xlsx",sheet = "vaccines")
pcv_xr = readxl::read_excel("~/Git/avoncap/data-raw/serotype-data.xlsx",sheet = "cross-react")
pcv_uad = readxl::read_excel("~/Git/avoncap/data-raw/serotype-data.xlsx",sheet = "uad")
pcv_grps = readxl::read_excel("~/Git/avoncap/data-raw/serotype-data.xlsx",sheet = "group-names")
pcv_names = readxl::read_excel("~/Git/avoncap/data-raw/serotype-data.xlsx",sheet = "layout-one")

serotype_data = list(
  map = pcv_map,
  xr = pcv_xr,
  uad = pcv_uad,
  grps = pcv_grps,
  names = pcv_names
)

usethis::use_data(serotype_data, overwrite = TRUE)

# default mapping from UAD tests to serotype groups (including single serotypes as groups)

tmp = pcv_map %>%
  pivot_longer(cols = c(-serotype), names_to = "group", values_to = "tmp") %>%
  inner_join(pcv_xr, by="serotype") %>%
  filter(tmp) %>%
  select(-tmp, -order) %>%
  inner_join(pcv_grps, by="group", suffix=c("",".add")) %>%
  select(-group)

uad_pcv_map = bind_rows(
  tmp %>% mutate(type = "pcv_and_xr") %>% rename(label = uad_label) %>% select(-serotype_label),
  pcv_uad %>%
    inner_join(pcv_xr %>% filter(order==1), by="uad_analysis") %>%
    mutate(type = "uad_test") %>%
    select(-order)
) %>% mutate(
  # for backward compatibility
  group = label
)

t1 = uad_pcv_map %>% select(uad_analysis, group) %>% distinct() %>% nrow()
t2 = uad_pcv_map %>% select(uad_analysis, group) %>% nrow()
if (t1!=t2) stop("Duplicates")

usethis::use_data(uad_pcv_map, overwrite = TRUE)

# default mapping from serotypes to vaccine groups
# (not including single serotypes as groups because we would need to know them
# all)

serotype_pcv_map = pcv_map %>%
  pivot_longer(cols = c(-serotype), names_to = "group", values_to = "tmp") %>%
  filter(tmp) %>%
  select(-tmp) %>%
  inner_join(pcv_grps, by="group", suffix=c("",".add")) %>%
  select(-group) %>%
  mutate(type = "pcv_group") %>%
  rename(label = serotype_label) %>%
  select(-uad_label) %>%
  mutate(group = label)

t1 = serotype_pcv_map %>% select(serotype, group) %>% distinct() %>% nrow()
t2 = serotype_pcv_map %>% select(serotype, group) %>% nrow()
if (t1!=t2) stop("Duplicates")

usethis::use_data(serotype_pcv_map, overwrite = TRUE)

# TODO: default mapping from serotypes to UAD style vaccine groups and UAD xr test groups

tmp = pcv_map %>%
  pivot_longer(cols = c(-serotype), names_to = "group", values_to = "tmp") %>%
  inner_join(pcv_xr, by="serotype") %>%
  filter(tmp) %>%
  select(-tmp, -order, -serotype) %>%
  inner_join(pcv_xr, by="uad_analysis") %>%
  select(-uad_analysis, -order) %>%
  inner_join(pcv_grps, by="group", suffix=c("",".add")) %>%
  select(-group) %>% mutate(type = "pcv_and_xr") %>%
  rename(label = uad_label) %>% select(-serotype_label)

serotype_uad_map = bind_rows(
  tmp,
  pcv_uad %>%
    inner_join(pcv_xr, by="uad_analysis") %>%
    mutate(type = "uad_test") %>%
    select(-order, -uad_analysis)
) %>% mutate(
  # for backward compatibility
  group = label
)

t1 = serotype_uad_map %>% select(serotype, group) %>% distinct() %>% nrow()
t2 = serotype_uad_map %>% select(serotype, group) %>% nrow()
if (t1!=t2) stop("Duplicates")

usethis::use_data(serotype_uad_map, overwrite = TRUE)


# UAD groups

# {
#   uad_groups = list(
#     pcv7on0 = c("4", "6B", "9V", "14", "18C", "19F", "23F") %>% .serotype_levels(),
#     pcv7_xr = c("9A", "18A", "18B", "18F", "23B") %>% .serotype_levels(),
#     pcv13on7 = c("1", "5", "7F","6A", "3", "19A") %>% .serotype_levels(),
#     pcv13on7_xr = c("6C"),
#     pcv15on13 = c("22F", "33F") %>% .serotype_levels(),
#     pcv20on15 = c("8", "10A", "11A", "12F", "15B") %>% .serotype_levels(),
#     pcv20on15_xr = c("15C"),
#     ppv23on20 = c("2", "9N", "17F", "20") %>% .serotype_levels(),
#
#     ### Non pfizer vaccine coverage reference data ----
#     pcv10_SSI =  c("4", "6B", "9V", "14", "18C", "19F", "23F", "1", "5", "7F") %>% .serotype_levels(),
#     pcv10_GSK = c("4", "6B", "9V", "14", "18C", "19F", "23F", "1", "5", "7F") %>% .serotype_levels(),
#     pcv15_Zhifei = c("4", "6B", "9V", "14", "18C", "19F", "23F", "1", "5", "7F", "3", "6A", "19A", "2", "12F") %>% .serotype_levels(),
#     pcv24_Vaxcyte = c("4", "6B", "9V", "14", "18C", "19F", "23F", "1", "5", "7F", "3", "6A", "6B", "19A", "22F", "33F", "8", "10A", "11A", "12F", "15B", "2", "9N", "17F", "20") %>% .serotype_levels(),
#     pcv24_Affinivax = c("4", "6B", "9V", "14", "18C", "19F", "23F", "1", "5", "7F", "3", "6A", "6B", "19A", "22F", "33F", "8", "10A", "11A", "12F", "15B", "2", "9N", "17F", "20B") %>% .serotype_levels()
#   )
#   uad_groups$pcv7 = c(uad_groups$pcv7on0, uad_groups$pcv7_xr) %>% .serotype_levels()
#   uad_groups$pcv13 = c(uad_groups$pcv7, uad_groups$pcv13on7, uad_groups$pcv13on7_xr) %>% .serotype_levels()
#   uad_groups$pcv15 = c(uad_groups$pcv13, uad_groups$pcv15on13) %>% .serotype_levels()
#   uad_groups$pcv20 = c(uad_groups$pcv15, uad_groups$pcv20on15, uad_groups$pcv20on15_xr) %>% .serotype_levels()
#   uad_groups$ppv23 = c(uad_groups$pcv20, uad_groups$ppv23on20) %>% .serotype_levels()
#
#   uad_groups$pcv10_GSK_on7 = setdiff(uad_groups$pcv10_GSK, uad_groups$pcv7) %>% .serotype_levels()
#   uad_groups$pcv15_Zhifei_on13 = setdiff(uad_groups$pcv10_GSK, uad_groups$pcv13) %>% .serotype_levels()
#   uad_groups$pcv24_Vaxcyte_on20 = setdiff(uad_groups$pcv24_Vaxcyte, uad_groups$pcv20) %>% .serotype_levels()
#   uad_groups$pcv24_Affinivax_on20=  setdiff(uad_groups$pcv24_Affinivax, uad_groups$pcv20) %>% .serotype_levels()
#
#   uad_groups$uad1 = c(uad_groups$pcv13) %>% .serotype_levels()
#   uad_groups$uad2 = setdiff(c(uad_groups$ppv23), uad_groups$pcv13) %>% .serotype_levels()
# }

uad_groups = list(
  pcv7 = pcv_map %>% filter(PCV7) %>% inner_join(pcv_xr, by = "serotype") %>% pull(uad_analysis),
  pcv13 = pcv_map %>% filter(PCV13) %>% inner_join(pcv_xr, by = "serotype") %>% pull(uad_analysis),
  pcv15 = pcv_map %>% filter(PCV15) %>% inner_join(pcv_xr, by = "serotype") %>% pull(uad_analysis),
  pcv20 = pcv_map %>% filter(PCV20) %>% inner_join(pcv_xr, by = "serotype") %>% pull(uad_analysis),
  ppv23 = pcv_map %>% filter(PPV23) %>% inner_join(pcv_xr, by = "serotype") %>% pull(uad_analysis),
  pcv13on7 = pcv_map %>% filter(PCV13on7) %>% inner_join(pcv_xr, by = "serotype") %>% pull(uad_analysis),
  pcv15on13 = pcv_map %>% filter(PCV15on13) %>% inner_join(pcv_xr, by = "serotype") %>% pull(uad_analysis),
  pcv20on15 = pcv_map %>% filter(PCV20on15) %>% inner_join(pcv_xr, by = "serotype") %>% pull(uad_analysis),
  ppv23on20 = pcv_map %>% filter(PPV23on20) %>% inner_join(pcv_xr, by = "serotype") %>% pull(uad_analysis),
  uad1 = pcv_uad %>% filter(panel=="UAD1") %>% pull(uad_analysis),
  uad2 = pcv_uad %>% filter(panel=="UAD2") %>% pull(uad_analysis)
)

usethis::use_data(uad_groups, overwrite = TRUE)

# default_pcv_map = tibble::tribble(
#   ~panel,    ~group,                       ~serotype,
#   "UAD1",    "PCV7+9A+18A/B/F+23B,",       uad_groups$pcv7,
#   "UAD1",    "PCV13+6C (over PCV7)",       c(uad_groups$pcv13on7,"6C") %>% .serotype_levels(),
#   "UAD2",    "PCV15 (over PCV13)",         c(uad_groups$pcv15on13),
#   "UAD2",    "PCV20+15C (over PCV15)",     c(uad_groups$pcv20on15,"15C") %>% .serotype_levels(),
#   "UAD2",    "PPV23 (over PCV20)",         c(uad_groups$ppv23on20)
# ) %>% tidyr::unnest(serotype) %>%
#   dplyr::mutate(
#     group = factor(group,levels = c("PCV7","PCV13+6C (over PCV7)",
#                                          "PCV15 (over PCV13)",
#                                          "PCV20+15C (over PCV15)","PPV23 (over PCV20)")),
#     serotype_level = factor(serotype,levels = c(
#       uad_groups$pcv7,
#       c(uad_groups$pcv13on7,"6C") %>% .serotype_levels(),
#       c(uad_groups$pcv15on13),
#       c(uad_groups$pcv20on15,"15C") %>% .serotype_levels(),
#       c(uad_groups$ppv23on20)
#     )))





# default_uad_map = tibble::tribble(
#   ~group,                       ~serotype,
#   "PCV7",                       uad_groups$pcv7,
#   "PCV13+6C (over PCV7)",       c(uad_groups$pcv13on7,"6C") %>% .serotype_levels(),
#   "PCV15 (over PCV13)",         c(uad_groups$pcv15on13),
#   "PCV20+15C (over PCV15)",     c(uad_groups$pcv20on15,"15C") %>% .serotype_levels(),
#   "PPV23 (over PCV20)",         c(uad_groups$ppv23on20)
# ) %>% tidyr::unnest(serotype)
#
#
# usethis::use_data(default_pcv_map, overwrite = TRUE)

