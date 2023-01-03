library(tidyverse)
usethis::proj_set("~/Git/avoncap/")
devtools::load_all()
## code to prepare `uad_groups` dataset goes here

{
  uad_groups = list(
    pcv7 = c("4", "6B", "9V", "14", "18C", "19F", "23F") %>% .serotype_levels(),
    pcv13on7 = c("1", "5", "7F","6A", "3", "19A") %>% .serotype_levels(),
    pcv15on13 = c("22F", "33F") %>% .serotype_levels(),
    pcv20on15 = c("8", "10A", "11A", "12F", "15B") %>% .serotype_levels(),
    ppv23on20 = c("2", "9N", "17F", "20") %>% .serotype_levels(),

    ### Non pfizer vaccine coverage reference data ----
    pcv10_SSI =  c("4", "6B", "9V", "14", "18C", "19F", "23F", "1", "5", "7F") %>% .serotype_levels(),
    pcv10_GSK = c("4", "6B", "9V", "14", "18C", "19F", "23F", "1", "5", "7F") %>% .serotype_levels(),
    pcv15_Zhifei = c("4", "6B", "9V", "14", "18C", "19F", "23F", "1", "5", "7F", "3", "6A", "19A", "2", "12F") %>% .serotype_levels(),
    pcv24_Vaxcyte = c("4", "6B", "9V", "14", "18C", "19F", "23F", "1", "5", "7F", "3", "6A", "6B", "19A", "22F", "33F", "8", "10A", "11A", "12F", "15B", "2", "9N", "17F", "20") %>% .serotype_levels(),
    pcv24_Affinivax = c("4", "6B", "9V", "14", "18C", "19F", "23F", "1", "5", "7F", "3", "6A", "6B", "19A", "22F", "33F", "8", "10A", "11A", "12F", "15B", "2", "9N", "17F", "20B") %>% .serotype_levels()
  )
  uad_groups$pcv13 = c(uad_groups$pcv7, uad_groups$pcv13on7, "6C") %>% .serotype_levels()
  uad_groups$pcv15 = c(uad_groups$pcv13, uad_groups$pcv15on13, "6C") %>% .serotype_levels()
  uad_groups$pcv20 = c(uad_groups$pcv15, uad_groups$pcv20on15, "6C", "15C") %>% .serotype_levels()
  uad_groups$ppv23 = c(uad_groups$pcv20, uad_groups$ppv23on20, "6C", "15C") %>% .serotype_levels()

  uad_groups$pcv10_GSK_on7 = setdiff(uad_groups$pcv10_GSK, uad_groups$pcv7) %>% .serotype_levels()
  uad_groups$pcv15_Zhifei_on13 = setdiff(uad_groups$pcv10_GSK, uad_groups$pcv13) %>% .serotype_levels()
  uad_groups$pcv24_Vaxcyte_on20 = setdiff(uad_groups$pcv24_Vaxcyte, uad_groups$pcv20) %>% .serotype_levels()
  uad_groups$pcv24_Affinivax_on20=  setdiff(uad_groups$pcv24_Affinivax, uad_groups$pcv20) %>% .serotype_levels()

  uad_groups$uad1 = c(uad_groups$pcv13,"6C") %>% .serotype_levels()
  uad_groups$uad2 = setdiff(c(uad_groups$ppv23,"15C"), uad_groups$pcv13) %>% .serotype_levels()
}


usethis::use_data(uad_groups, overwrite = TRUE)

# The

default_pcv_map = tibble::tribble(
  ~panel,    ~group,                       ~serotype,
  "UAD1",    "PCV7",                       uad_groups$pcv7,
  "UAD1",    "PCV13+6C (over PCV7)",       c(uad_groups$pcv13on7,"6C") %>% .serotype_levels(),
  "UAD2",    "PCV15 (over PCV13)",         c(uad_groups$pcv15on13),
  "UAD2",    "PCV20+15C (over PCV15)",     c(uad_groups$pcv20on15,"15C") %>% .serotype_levels(),
  "UAD2",    "PPV23 (over PCV20)",         c(uad_groups$ppv23on20)
) %>% tidyr::unnest(serotype) %>%
  dplyr::mutate(
    group = factor(group,levels = c("PCV7","PCV13+6C (over PCV7)",
                                         "PCV15 (over PCV13)",
                                         "PCV20+15C (over PCV15)","PPV23 (over PCV20)")),
    serotype_level = factor(serotype,levels = c(
      uad_groups$pcv7,
      c(uad_groups$pcv13on7,"6C") %>% .serotype_levels(),
      c(uad_groups$pcv15on13),
      c(uad_groups$pcv20on15,"15C") %>% .serotype_levels(),
      c(uad_groups$ppv23on20)
    )))


usethis::use_data(default_pcv_map, overwrite = TRUE)


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

