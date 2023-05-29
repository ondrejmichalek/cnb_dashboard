mypackages <- c("readxl", "tidyverse", "broom", "lubridate", "rvest","httr", "glue",
                "magrittr", "kableExtra", "knitr")

for (p in mypackages){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}


# Prumyslova produkce -----------------------------------------------------

zac_prum <- "2000-01-01"


url <- "https://vdb.czso.cz/pll/eweb/vdb2xls.export?xid=561"
GET(url, write_disk("data/raw/prum.xlsx", overwrite = T))
prum_raw <- read_xlsx("data/raw/prum.xlsx")

p <- as_tibble(t(prum_raw))

prumysl_temp <- p %>%
  select(V7:V41)

colnames(prumysl_temp) <- c("date", prumysl_temp[2,2:ncol(prumysl_temp)])

prumysl <- prumysl_temp %>%
  filter(!is.na(date)) %>%
  # mutate(date = seq(as.Date(zac_prum),as.Date(kon_prum),by = "1 month")) %>%
  mutate(date = seq(from=as.Date(zac_prum), by="1 month", length.out = nrow(.))) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  select(-c("06","07","12","19"))

# prejmenovani sloupcu kvuli snadnejsimu zpracovani dat
colnames(prumysl)[2:ncol(prumysl)] <- c("pr", paste0("pr_", colnames(prumysl)[3:ncol(prumysl)]))

write_rds(prumysl, "pr_p.rds")
# 
# # Prumysl - zakazky celkem ------------------------------------------------
# 
# url <- "https://vdb.czso.cz/pll/eweb/vdb2xls.export?xid=667"
# GET(url, write_disk("data/raw/prum_zak_celkem.xlsx", overwrite = T))
# prum_zak_cel_raw <- read_xlsx("data/raw/prum_zak_celkem.xlsx")
# 
# p <- as_tibble(t(prum_zak_cel_raw))
# 
# prumysl_zak_cel_temp <- p %>%
#   select(V7:V20)
# 
# colnames(prumysl_zak_cel_temp) <- c("date", "celkem",prumysl_zak_cel_temp[2,3:ncol(prumysl_zak_cel_temp)])
# 
# prumysl_zak_cel <- prumysl_zak_cel_temp %>%
#   filter(!is.na(.)) %>%
#   slice(-1) %>% 
#   # mutate(date = seq(as.Date(zac_prum),as.Date(kon_prum),by = "1 month")) %>%
#   mutate(date = seq(from=as.Date(zac_prum), by="1 month", length.out = nrow(.))) %>%
#   mutate(across(where(is.character), as.numeric))
# 
# # prejmenovani sloupcu kvuli snadnejsimu zpracovani dat
# colnames(prumysl_zak_cel)[2:ncol(prumysl_zak_cel)] <- c("pr_zak_celkem", paste0("pr_zak_", colnames(prumysl_zak_cel)[3:ncol(prumysl_zak_cel)]))
# 
# write_tsv(prumysl_zak_cel, "data/tsv/pr_zc.tsv.gz")
# 
# # Prumysl - zakazky domaci ------------------------------------------------
# 
# url <- "https://vdb.czso.cz/pll/eweb/vdb2xls.export?xid=669"
# GET(url, write_disk("data/raw/prum_zak_dom.xlsx", overwrite = T))
# prum_zak_dom_raw <- read_xlsx("data/raw/prum_zak_dom.xlsx")
# 
# p <- as_tibble(t(prum_zak_dom_raw))
# 
# prumysl_zak_dom_temp <- p %>%
#   select(V7:V20)
# 
# colnames(prumysl_zak_dom_temp) <- c("date", "celkem",prumysl_zak_dom_temp[2,3:ncol(prumysl_zak_dom_temp)])
# 
# prumysl_zak_dom <- prumysl_zak_dom_temp %>%
#   filter(!is.na(.)) %>%
#   slice(-1) %>% 
#   # mutate(date = seq(as.Date(zac_prum),as.Date(kon_prum),by = "1 month")) %>%
#   mutate(date = seq(from=as.Date(zac_prum), by="1 month", length.out = nrow(.))) %>%
#   mutate(across(where(is.character), as.numeric))
# 
# # prejmenovani sloupcu kvuli snadnejsimu zpracovani dat
# colnames(prumysl_zak_dom)[2:ncol(prumysl_zak_dom)] <- c("pr_zak_dom_celkem", paste0("pr_zak_dom_", colnames(prumysl_zak_dom)[3:ncol(prumysl_zak_dom)]))
# 
# write_tsv(prumysl_zak_dom, "data/tsv/pr_zd.tsv.gz")
# 
# # Prumysl - zakazky zahranici ------------------------------------------------
# 
# url <- "https://vdb.czso.cz/pll/eweb/vdb2xls.export?xid=668"
# GET(url, write_disk("data/raw/prum_zak_zahr.xlsx", overwrite = T))
# prum_zak_zahr_raw <- read_xlsx("data/raw/prum_zak_zahr.xlsx")
# 
# p <- as_tibble(t(prum_zak_zahr_raw))
# 
# prumysl_zak_zahr_temp <- p %>%
#   select(V7:V20)
# 
# colnames(prumysl_zak_zahr_temp) <- c("date", "celkem",prumysl_zak_zahr_temp[2,3:ncol(prumysl_zak_zahr_temp)])
# 
# prumysl_zak_zahr <- prumysl_zak_zahr_temp %>%
#   filter(!is.na(.)) %>%
#   slice(-1) %>% 
#   # mutate(date = seq(as.Date(zac_prum),as.Date(kon_prum),by = "1 month")) %>%
#   mutate(date = seq(from=as.Date(zac_prum), by="1 month", length.out = nrow(.))) %>%
#   mutate(across(where(is.character), as.numeric))
# 
# # prejmenovani sloupcu kvuli snadnejsimu zpracovani dat
# colnames(prumysl_zak_zahr)[2:ncol(prumysl_zak_zahr)] <- c("pr_zak_zahr_celkem", paste0("pr_zak_zahr_", colnames(prumysl_zak_zahr)[3:ncol(prumysl_zak_zahr)]))
# 
# write_tsv(prumysl_zak_zahr, "data/tsv/pr_zz.tsv.gz")
# 
# # Merge prumyslove zakazky
# prum_zak_vse <- full_join(prumysl_zak_cel, prumysl_zak_dom, by = "date")
# prum_zak_vse <- full_join(prum_zak_vse, prumysl_zak_zahr, by = "date")
# 
# write_tsv(prum_zak_vse, "data/tsv/pr_z.tsv.gz")
# 
# 
# 
# 
# # Prumysl - trzby celkem --------------------------------------------------
# 
# 
# url <- "https://vdb.czso.cz/pll/eweb/vdb2xls.export?xid=782"
# GET(url, write_disk("data/raw/prum_trzby_celkem.xlsx", overwrite = T))
# prum_trzby_cel_raw <- read_xlsx("data/raw/prum_trzby_celkem.xlsx")
# 
# p <- as_tibble(t(prum_trzby_cel_raw))
# 
# prumysl_trzby_cel_temp <- p %>%
#   select(V7:V41)
# 
# colnames(prumysl_trzby_cel_temp) <- c("date", "celkem",prumysl_trzby_cel_temp[2,3:ncol(prumysl_trzby_cel_temp)])
# 
# prumysl_trzby_cel <- prumysl_trzby_cel_temp %>%
#   filter(!is.na(.)) %>%
#   slice(-1) %>% 
#   mutate(date = seq(from=as.Date(zac_prum), by="1 month", length.out = nrow(.))) %>%
#   mutate(across(where(is.character), as.numeric)) %>% 
#   select(-c("06","07","12","19"))
# 
# # prejmenovani sloupcu kvuli snadnejsimu zpracovani dat
# colnames(prumysl_trzby_cel)[2:ncol(prumysl_trzby_cel)] <- c("pr_trz_celkem", paste0("pr_trz_", colnames(prumysl_trzby_cel)[3:ncol(prumysl_trzby_cel)]))
# 
# write_tsv(prumysl_trzby_cel, "data/tsv/pr_tc.tsv.gz")
# 
# 
# # Prumysl - trzby domaci --------------------------------------------------
# 
# url <- "https://vdb.czso.cz/pll/eweb/vdb2xls.export?xid=780"
# GET(url, write_disk("data/raw/prum_trzby_domaci.xlsx", overwrite = T))
# prum_trzby_dom_raw <- read_xlsx("data/raw/prum_trzby_domaci.xlsx")
# 
# p <- as_tibble(t(prum_trzby_dom_raw))
# 
# prumysl_trzby_dom_temp <- p %>%
#   select(V7:V41)
# 
# colnames(prumysl_trzby_dom_temp) <- c("date", "celkem",prumysl_trzby_dom_temp[2,3:ncol(prumysl_trzby_dom_temp)])
# 
# prumysl_trzby_dom <- prumysl_trzby_dom_temp %>%
#   filter(!is.na(.)) %>%
#   slice(-1) %>% 
#   mutate(date = seq(from=as.Date(zac_prum), by="1 month", length.out = nrow(.))) %>%
#   mutate(across(where(is.character), as.numeric)) %>% 
#   select(-c("06","07","12","19"))
# 
# # prejmenovani sloupcu kvuli snadnejsimu zpracovani dat
# colnames(prumysl_trzby_dom)[2:ncol(prumysl_trzby_dom)] <- c("pr_trz_dom_celkem", paste0("pr_trz_dom_", colnames(prumysl_trzby_dom)[3:ncol(prumysl_trzby_dom)]))
# 
# write_tsv(prumysl_trzby_dom, "data/tsv/pr_td.tsv.gz")
# 
# 
# # Prumysl - trzby zahranicni ----------------------------------------------
# 
# url <- "https://vdb.czso.cz/pll/eweb/vdb2xls.export?xid=781"
# GET(url, write_disk("data/raw/prum_trzby_zahranicni.xlsx", overwrite = T))
# prum_trzby_zahr_raw <- read_xlsx("data/raw/prum_trzby_zahranicni.xlsx")
# 
# p <- as_tibble(t(prum_trzby_zahr_raw))
# 
# prumysl_trzby_zahr_temp <- p %>%
#   select(V7:V41)
# 
# colnames(prumysl_trzby_zahr_temp) <- c("date", "celkem",prumysl_trzby_zahr_temp[2,3:ncol(prumysl_trzby_zahr_temp)])
# 
# prumysl_trzby_zahr <- prumysl_trzby_zahr_temp %>%
#   filter(!is.na(.)) %>%
#   slice(-1) %>% 
#   mutate(date = seq(from=as.Date(zac_prum), by="1 month", length.out = nrow(.))) %>%
#   mutate(across(where(is.character), as.numeric)) %>% 
#   select(-c("06","07","12","19"))
# 
# # prejmenovani sloupcu kvuli snadnejsimu zpracovani dat
# colnames(prumysl_trzby_zahr)[2:ncol(prumysl_trzby_zahr)] <- c("pr_trz_zahr_celkem", paste0("pr_trz_zahr_", colnames(prumysl_trzby_zahr)[3:ncol(prumysl_trzby_zahr)]))
# 
# write_tsv(prumysl_trzby_zahr, "data/tsv/pr_tz.tsv.gz")