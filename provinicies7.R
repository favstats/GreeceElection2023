source("utils.R")
# ?get_targeting
# get_targeting("41459763029", timeframe = "LAST_90_DAYS")
# debugonce(get_targeting)

library(httr)
library(tidyverse)

tstamp <- Sys.time()

# read_csv("data/brem")

more_data <- dir("data/reports", full.names = T) %>%
    map_dfr(~{print(.x)
        yo <- read.csv(.x) %>% mutate(path = .x)
        return(yo)
            }) %>%
    mutate(date_produced = str_remove_all(path, "data/reports/FacebookAdLibraryReport_|_DE_last_30_days_Bremen\\.csv")) %>%
    mutate(date_produced = lubridate::ymd(date_produced)) %>%
    janitor::clean_names()%>% #rename(advertiser_id = page_id) %>%
    mutate(spend = readr::parse_number(amount_spent_eur)) %>%
    mutate(spend = ifelse(spend == 100, 50, spend)) %>%
    distinct(page_id, .keep_all = T)  %>%
  mutate(party1 = case_when(
    str_detect(page_name, "\\bDrugo\\b") ~ "Drugo",
    str_detect(page_name, "\\bDCG\\b") ~ "DCG",
    str_detect(page_name, "\\bDF\\b") ~ "DF",
    str_detect(page_name, "\\bURA\\b") ~ "URA",
    str_detect(page_name, "\\bDPS\\b") ~"DPS",
    str_detect(page_name, "\\bSDP\\b") ~ "SDP",
    str_detect(page_name, "\\bUCG\\b") ~ "UCG",
    str_detect(page_name, "\\bVlada Crne Gore\\b") ~ "Vlada Crne Gore",
    str_detect(page_name, "\\bDEMOS\\b") ~ "DEMOS",
    str_detect(page_name, "\\bHGI\\b") ~ "HGI",
    str_detect(page_name, "\\bLP\\b") ~ "LP",
    str_detect(page_name, "\\bPrava Crna Gora\\b") ~ "Prava Crna Gora",
    T ~ NA_character_
  ))%>%
    mutate(party2 = case_when(
      str_detect(disclaimer, "\\bDrugo\\b") ~ "Drugo",
      str_detect(disclaimer, "\\bDCG\\b") ~ "DCG",
      str_detect(disclaimer, "\\bDF\\b") ~ "DF",
      str_detect(disclaimer, "\\bURA\\b") ~ "URA",
      str_detect(disclaimer, "\\bDPS\\b") ~"DPS",
      str_detect(disclaimer, "\\bSDP\\b") ~ "SDP",
      str_detect(disclaimer, "\\bUCG\\b") ~ "UCG",
      str_detect(disclaimer, "\\bVlada Crne Gore\\b") ~ "Vlada Crne Gore",
      str_detect(disclaimer, "\\bDEMOS\\b") ~ "DEMOS",
      str_detect(disclaimer, "\\bHGI\\b") ~ "HGI",
      str_detect(disclaimer, "\\bLP\\b") ~ "LP",
      str_detect(disclaimer, "\\bPrava Crna Gora\\b") ~ "Prava Crna Gora",
      T ~ NA_character_
    )) %>%
    mutate(party = ifelse(is.na(party1), party2, party1)) %>%
    # drop_na(party) %>%
    distinct(page_id, .keep_all = T) %>%
    filter(str_detect(page_name, "Global Space Conference on Climate Change|de Alliantie|PvdA - GroenLinks", negate = T)) %>%
    mutate(page_id = as.character(page_id))


source("utils.R")

unlink("provincies/7", recursive = T, force = T)
unlink("provincies/30", recursive = T, force = T)

dir.create("provincies/7")
dir.create("provincies/30")

# rawadvertisers <- read_csv("data/advertisers - advertisers.csv")  %>%
#   mutate(party_lab = case_when(
#     str_detect(advertiser_name, "VVD") ~ "VVD",
#     str_detect(advertiser_name, "\\bCDA\\b") ~ "CDA",
#     str_detect(advertiser_name, "PvdA|Jonge Socialisten") ~ "PvdA",
#     str_detect(advertiser_name, "D66|Jonge Democraten") ~ "D66",
#     str_detect(advertiser_name, "GroenLinks") ~ "GroenLinks",
#     str_detect(advertiser_name, "ChristenUnie") ~ "ChristenUnie",
#     str_detect(advertiser_name, "\\bSP\\b") ~ "SP",
#     str_detect(advertiser_name, "FvD|FVD|Forum voor Democratie") ~ "FvD",
#     str_detect(advertiser_name, "50PLUS") ~ "50PLUS",
#     str_detect(advertiser_name, "\\bSGP\\b") ~ "SGP",
#     str_detect(advertiser_name, "PvdD|Partij voor de Dieren") ~ "PvdD",
#     str_detect(advertiser_name, "PVV") ~ "PVV",
#     str_detect(advertiser_name, "DENK") ~ "DENK",
#     str_detect(advertiser_name, "Volt") ~ "Volt Nederland",
#     str_detect(advertiser_name, "BIJ1") ~ "BIJ1",
#     str_detect(advertiser_name, "BVNL") ~ "BVNL",
#     str_detect(advertiser_name, "Ja21") ~ "Ja21",
#     T ~ ""
#   ))



# internal_page_ids <- read_csv("data/nl_advertisers.csv") %>%
#   mutate(page_id = as.character(page_id))

# internal_page_ids <- read_csv("https://raw.githubusercontent.com/favstats/ProvincialeStatenverkiezingen2023/main/data/nl_advertisers.csv") %>%
#     mutate(page_id = as.character(page_id))

# internal_page_ids %>%
#     count(party, sort = T) %>% View
wtm_data <- read_csv("data/wtm-advertisers-gr-2023-05-20T08_49_00.571Z.csv") %>% #names
    select(page_id = advertisers_platforms.advertiser_platform_ref,
           page_name = name, party = entities.short_name)  %>%
    mutate(page_id = as.character(page_id)) 

wtm_data %>% count(party, sort = T) %>% pull(party) %>% clipr::write_clip()

    # filter(party == "And") %>% #View
  #   # count(party, sort = T)  %>%
  # mutate(party = case_when(
  #   str_detect(party, "\\bKOK\\b") ~ "KOK",
  #   str_detect(party, "\\bVIH\\b") ~ "VIH",
  #   str_detect(party, "\\bVAS\\b") ~ "VAS",
  #   str_detect(party, "\\bPER\\b") ~ "PER",
  #   str_detect(party, "\\bMUUT\\b") ~ "MUUT",
  #   str_detect(party, "\\bKES\\b") ~ "KES",
  #   str_detect(party, "\\bSDP\\b") ~ "SDP",
  #   str_detect(party, "\\bRKP\\b") ~ "KD",
  #   str_detect(party, "\\bKOR\\b") ~ "KOR",
  #   T ~ party
  # )) #%>% #View
    # count(party, sort = T)

# wtm_data %>% 
#   filter(party == "And") %>% View

# wtm_data %>% count(party)

# 338750440106782

all_dat <- wtm_data %>%
    distinct(page_id, .keep_all = T) %>%
    add_count(page_name, sort  =T) %>%
    mutate(remove_em = n >= 2 & str_ends(page_id, "0")) %>%
    filter(!remove_em) %>%
    # filter(n >= 2) %>%
    # filter(n >= 2 & str_ends(page_id, "0", negate = T)) %>%
    select(-n)  

all_dat %>% count(party, sort = T)
all_dat %>% nrow
# 
# all_dat %>% filter(source == "new") %>% View
# 
# more_data %>% 
#   select(-party) %>% 
#   left_join(wtm_data %>% select(page_id, party)) %>% 
#   openxlsx::write.xlsx("data/tolabel.xlsx")


# all_dat %>%
#     filter(party == "And") %>% View
#     count(party, sort = T) %>% View

#
# all_dat %>% View
#     filter(page_id == 1519997944789250)
#
# all_dat %>%
#     add_count(page_name, sort  =T) %>%
#     filter(n >= 2) %>% View
#     filter(str_ends(page_id, "0", negate = T)) %>% View

# all_dat %>%
#     mutate(source = "already_there") %>%
#     # filter(str_detect(party, "FvD")) %>% View
#     bind_rows(more_data) %>%
#     distinct(page_id, .keep_all = T) %>%
#     # filter(page_id == 609816282477420) %>%
#     # filter(page_id == 609816282477420) %>% View
#
#     filter(is.na(source)) %>% View

# all_dat %>%
#     count(party, sort  =T)

# all_dat %>%
#     bind_rows(rep %>% select(page_name, page_id, disclaimer, party))  %>%
#     distinct(page_id, .keep_all = T) %>%
#     filter(!(page_id %in% all_dat$page_id)) %>%
#     filter(str_detect(page_name, "Global Space Conference on Climate Change|de Alliantie|PvdA - GroenLinks", negate = T)) %>% View

# all_dat %>% filter(str_detect(page_name, "BBB")) %>% View

# write_csv(all_dat, file = "data/me_advertisers.csv")

# janitor::clean_names() %>%
# arrange(desc(amount_spent_usd)) %>%
# mutate(spend_upper = amount_spent_usd %>% as.numeric()) %>%
# arrange(-spend_upper) %>%
# mutate_all(as.character)C

# internal_page_ids %>% count(party, sort =T) %>% slice(11:17)
#
# internal_page_ids %>%
#   filter(party == "Politiek Op Maat")
#
# rawadvertisers %>%
#   # filter(category == "Political Organization") %>% View
#   # filter(str_detect(category, "Party|Politician|Candidade")) %>%
#   rename(page_id = advertiser_id) %>%
#   select(page_id, page_name = advertiser_name, party = party_lab)
#   left_join(internal_page_ids) %>%
#   # drop_na(party) %>%
#   filter(!is.na(party) | party_lab != "") %>%
#   # filter(party == "PvdA" & party_lab == "")
#   count(party, party_lab, sort = T)  %>% View
#
#
#
#   internal_page_ids %>%
#     bind_rows(
#       rawadvertisers %>%
#         rename(page_id = advertiser_id) %>%
#         select(page_id, page_name = advertiser_name, party = party_lab) %>%
#         filter(party != "") %>%
#         filter(str_starts(page_id, "AR", negate = T)) %>%
#         mutate(source = "yo")
#     ) %>%
#     distinct(page_id, .keep_all = T) %>%
#     write_csv("data/nl_advertisers.csv")


# georgia_wtm <- readr::read_csv("data/wtm-advertisers-us-2022-11-28T14_22_01.338Z.csv") %>%
#   select(page_name = name,
#          page_id = advertisers_platforms.advertiser_platform_ref) %>%
#   mutate(page_id = as.character(page_id))

# options(scipen = 999999)

# georgia_wtm

# internal_page_ids <- georgia_wtm %>%
#   mutate_all(as.character) %>%
#   bind_rows(last90days)  %>%
#   distinct(page_id, .keep_all = T)

# get_targeting(internal_page_ids$page_id[1], timeframe = "LAST_30_DAYS")
# debugonce(get_targeting)
# get_targeting("121264564551002", timeframe = "LAST_30_DAYS")

scraper <- function(.x, time = "7") {

  # print(paste0(.x$page_name,": ", round(which(internal_page_ids$page_id == .x$page_id)/nrow(internal_page_ids)*100, 2)))

  yo <- get_targeting(.x$page_id, timeframe = glue::glue("LAST_{time}_DAYS")) %>%
    mutate(tstamp = tstamp)

  if(nrow(yo)!=0){
    path <- paste0(glue::glue("provincies/{time}/"),.x$page_id, ".rds")
    # if(file.exists(path)){
    #   ol <- read_rds(path)
    #
    #   saveRDS(yo %>% bind_rows(ol), file = path)
    # } else {

    saveRDS(yo, file = path)
    # }
  }

  # print(nrow(yo))
  # })

}

scraper <- possibly(scraper, otherwise = NULL, quiet = F)


# if(F){
#     # dir("provincies/7", full.names
# }
# da30 <- readRDS("data/election_dat30.rds")
# da7 <- readRDS("data/election_dat7.rds")

already_there <- dir("provincies/7", full.names = T) %>% 
  str_remove_all("provincies/7/|\\.rds")

### save seperately
yo <- all_dat %>% #count(cntry, sort  =T) %>%
    # filter(!(page_id %in% already_there)) %>%
  # filter(!(page_id %in% unique(da7$page_id))) %>%
  # filter(cntry == "GB") %>%
  # slice(1:10) %>%
  split(1:nrow(.)) %>%
  map_dfr_progress(scraper, 7)

already_there <- dir("provincies/30", full.names = T) %>% 
  str_remove_all("provincies/30/|\\.rds")

yo <- all_dat %>% #count(cntry, sort  =T) %>%
    # filter(!(page_id %in% already_there)) %>%
    # filter(!(page_id %in% unique(da30$page_id))) %>%
    # filter(cntry == "GB") %>%
    # slice(1:10) %>%
    split(1:nrow(.)) %>%
    map_dfr_progress(scraper, 30)

# saveRDS(yo, file = )
library(tidyverse)
da30  <- dir("provincies/30", full.names = T) %>%
  map_dfr_progress(readRDS)  %>%
    mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
    rename(page_id = internal_id) %>%
    left_join(all_dat)

# da30 %>%
#     count(party, sort = T) %>% View


da7  <- dir("provincies/7", full.names = T) %>%
    map_dfr_progress(readRDS) %>%
    mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
    rename(page_id = internal_id) %>%
    left_join(all_dat)

saveRDS(da30, "data/election_dat30.rds")
saveRDS(da7, "data/election_dat7.rds")

da30 %>%
  # filter(party == "OTHER") %>% 
  # # distinct(page_id, page_name) %>% 
  # dput()
  count(party)
da7 %>% count(party)
da30 %>% count(page_id)
da30 %>% count(page_id)
da7 %>% count(ds)

da30 %>% 
  # filter(party == "Diğ") 
  filter(str_detect(page_name, "\\bDEVA\\b|\\bDeva\\b"))

# bbb %>% filter(str_detect(funding_, "Strijker"))

# da7 %>%
#   distinct(internal_id, .keep_all = T) %>%
#   mutate(total_spend = parse_number(total_spend_formatted)) %>%
#   rename(page_id = internal_id) %>%
#   left_join(internal_page_ids) %>%
#   group_by(party) %>%
#   summarize(total_spend = sum(total_spend))
#
#
# amgna <- da7 %>%
#   mutate(total_spend = parse_number(total_spend_formatted)) %>%
#   rename(page_id = internal_id) %>%
#   left_join(internal_page_ids)
#
#
# amgna %>%
#   filter(type == "gender") %>%
#   filter(value == "Women") %>%
#   # mutate(total_spend = total_spend*total_spend_pct) %>%
#   ggplot(aes(party, total_spend_pct)) +
#   geom_boxplot() #+
#   # scale_y_log10()
#
#
#
# amgna %>%
#   filter(type == "detailed")



wk_spend <- read_csv("data/google-political-ads-advertiser-weekly-spend.csv")

ggl_spend <- wk_spend  %>%
  mutate(party1 = case_when(
    str_detect(Advertiser_Name, "\\bKOK\\b|Kokoomus") ~ "KOK",
    str_detect(Advertiser_Name, "\\bVIH\\b|Vihreä") ~ "VIH",
    str_detect(Advertiser_Name, "\\bVAS\\b") ~ "VAS",
    str_detect(Advertiser_Name, "\\bPER\\b") ~ "PER",
    str_detect(Advertiser_Name, "\\bMUUT\\b") ~"MUUT",
    str_detect(Advertiser_Name, "\\bKES\\b|Keskusta") ~ "KES",
    str_detect(Advertiser_Name, "\\bSDP\\b|Sosialidemokraat") ~ "SDP",
    str_detect(Advertiser_Name, "\\bRKP\\b|Svenska folkpartiet i Finland") ~ "RKP",
    str_detect(Advertiser_Name, "\\bKD\\b") ~ "KD",
    str_detect(Advertiser_Name, "\\bKOR\\b") ~ "KOR",
    str_detect(Advertiser_Name, "\\bPS\\b|Perussuomalaisten") ~ "KOR",
    T ~ NA_character_
  )) %>%
    # distinct(Advertiser_Name, .keep_all = T) %>%
    filter(!(str_detect(Advertiser_Name, "JUNTS PER CATALUNYA|Gleichheitspartei|Nieuw-Vlaamse|SP Digital LLC|MURRAY|REVOLT|Angelenos Against Higher Property Taxes|ITALIA|Volt Deutschland"))) %>%
    drop_na(party1) %>%
    mutate(Week_Start_Date = lubridate::ymd(Week_Start_Date)) %>%
    filter(Week_Start_Date >= as.Date("2023-02-01"))
    # count(Week_Start_Date)
# ggl_spend %>% distinct(Advertiser_Name, .keep_all = T) %>% select(party1, everything()) %>% View

saveRDS(ggl_spend, "data/ggl_spend.rds")

# all_ads <- vroom::vroom("C:/Users/fabio/Downloads/skeptic/google-political-ads-creative-stats.csv")

source("selenium.R")

ggl_sel_sp <- readRDS("data/ggl_sel_sp.rds")

tt_ads <- ggl_sel_sp %>%
    rename(Advertiser_ID = advertiser_id) %>%
    left_join(ggl_spend %>% distinct(Advertiser_ID, party1))  %>%
    # mutate(Date_Range_Start = lubridate::ymd(Date_Range_Start)) %>%
    # filter(Date_Range_Start >= as.Date("2023-02-05")) %>%
    group_by(party1) %>%
    summarize(total_num_ads = sum(as.numeric(num_ads))) %>%
    # count(party1, name = "total_num_ads") %>%
    mutate(total_num_ads = scales::comma(total_num_ads)) %>%
    pivot_wider(names_from = party1, values_from = total_num_ads) %>%
    mutate(`Coalizione/Partito` = "Number of Ads")


ttl_spn <- ggl_sel_sp %>%
    rename(Advertiser_ID = advertiser_id) %>%
    left_join(ggl_spend %>% distinct(Advertiser_ID, party1)) %>%
    mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\."))) %>%
    group_by(party1) %>%
    summarize(Spend_EUR = sum(Spend_EUR)) %>%
    arrange(desc(Spend_EUR)) %>%
    select(party = party1, spend = Spend_EUR) %>%
    mutate(spend = scales::comma(spend)) %>%
    mutate(spend = paste0("€", spend)) %>%
    drop_na() %>%
    pivot_wider(names_from = party, values_from = spend) %>%
    mutate(`Coalizione/Partito` = "Total Spend")



tp_spnders <- ggl_sel_sp %>%
    rename(Advertiser_ID = advertiser_id) %>%
    left_join(ggl_spend %>% distinct(Advertiser_ID, party1, .keep_all = T) %>% select(Advertiser_ID, party1, Advertiser_Name)) %>%
    mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\.")))   %>%
    group_by(Advertiser_Name, party1) %>%
    summarize(Spend_EUR = sum(Spend_EUR)) %>%
    ungroup() %>%
    group_by(party1) %>%
    arrange(desc(Spend_EUR)) %>%
    slice(1:3) %>%
    mutate(Spend_EUR = scales::comma(Spend_EUR)) %>%
    mutate(n_words = str_count(Advertiser_Name, " ")) %>%
    # mutate(lab = paste0(word(str_remove(page_name, "-"), 1,ifelse(n_words>=2, 3, 2), sep=" "), "<br>(€", total_spend_formatted, ")")) %>%
    mutate(lab = paste0(Advertiser_Name, " (€", Spend_EUR, ")")) %>%
    select(party1, lab) %>%
    drop_na() %>%
    summarize(lab = paste0("<br>", 1:n(), ". ", lab, collapse = "")) %>%
    pivot_wider(names_from = party1, values_from = lab) %>%
    mutate(`Coalizione/Partito` = "Top Spenders")

ggl_all <- tt_ads %>%
    bind_rows(tp_spnders) %>%
    bind_rows(ttl_spn) %>%
    t() %>%
    as.data.frame() %>%
    rownames_to_column("Coalizione/Partito") %>%
    set_names(.[nrow(.),] %>% as.character()) %>%
    slice(1:(n()-1))


saveRDS(ggl_all, file = "data/ggl_all.rds")



ggl_sel_sp7 <- readRDS("data/ggl_sel_sp7.rds") %>% 
  filter(num_ads != "0")

tt_ads <- ggl_sel_sp7 %>%
  rename(Advertiser_ID = advertiser_id) %>%
  left_join(ggl_spend %>% distinct(Advertiser_ID, party1))  %>%
  # mutate(Date_Range_Start = lubridate::ymd(Date_Range_Start)) %>%
  # filter(Date_Range_Start >= as.Date("2023-02-05")) %>%
  group_by(party1) %>%
  summarize(total_num_ads = sum(as.numeric(num_ads))) %>%
  # count(party1, name = "total_num_ads") %>%
  mutate(total_num_ads = scales::comma(total_num_ads)) %>%
  pivot_wider(names_from = party1, values_from = total_num_ads) %>%
  mutate(`Coalizione/Partito` = "Number of Ads")


ttl_spn <- ggl_sel_sp7 %>%
  rename(Advertiser_ID = advertiser_id) %>%
  left_join(ggl_spend %>% distinct(Advertiser_ID, party1)) %>%
  mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\."))) %>%
  group_by(party1) %>%
  summarize(Spend_EUR = sum(Spend_EUR)) %>%
  arrange(desc(Spend_EUR)) %>%
  select(party = party1, spend = Spend_EUR) %>%
  mutate(spend = scales::comma(spend)) %>%
  mutate(spend = paste0("€", spend)) %>%
  drop_na() %>%
  pivot_wider(names_from = party, values_from = spend) %>%
  mutate(`Coalizione/Partito` = "Total Spend")



tp_spnders <- ggl_sel_sp7 %>%
  rename(Advertiser_ID = advertiser_id) %>%
  left_join(ggl_spend %>% distinct(Advertiser_ID, party1, .keep_all = T) %>% select(Advertiser_ID, party1, Advertiser_Name)) %>%
  mutate(Spend_EUR = readr::parse_number(str_remove(eur_amount, "\\.")))   %>%
  group_by(Advertiser_Name, party1) %>%
  summarize(Spend_EUR = sum(Spend_EUR)) %>%
  ungroup() %>%
  group_by(party1) %>%
  arrange(desc(Spend_EUR)) %>%
  slice(1:3) %>%
  mutate(Spend_EUR = scales::comma(Spend_EUR)) %>%
  mutate(n_words = str_count(Advertiser_Name, " ")) %>%
  # mutate(lab = paste0(word(str_remove(page_name, "-"), 1,ifelse(n_words>=2, 3, 2), sep=" "), "<br>(€", total_spend_formatted, ")")) %>%
  mutate(lab = paste0(Advertiser_Name, " (€", Spend_EUR, ")")) %>%
  select(party1, lab) %>%
  drop_na() %>%
  summarize(lab = paste0("<br>", 1:n(), ". ", lab, collapse = "")) %>%
  pivot_wider(names_from = party1, values_from = lab) %>%
  mutate(`Coalizione/Partito` = "Top Spenders")

ggl_all7 <- tt_ads %>%
  bind_rows(tp_spnders) %>%
  bind_rows(ttl_spn) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("Coalizione/Partito") %>%
  set_names(.[nrow(.),] %>% as.character()) %>%
  slice(1:(n()-1))


saveRDS(ggl_all7, file = "data/ggl_all7.rds")






all_ads <- vroom::vroom("C:/Users/fabio/Downloads/skeptic/google-political-ads-creative-stats.csv")

all_ads %>%
  filter(Advertiser_ID %in% ggl_sel_sp$advertiser_id) %>%
  mutate(Date_Range_Start = lubridate::ymd(Date_Range_Start)) %>%
  filter(Date_Range_Start >= as.Date("2023-02-05")) %>%
  mutate(Spend_Range_Max_EUR = as.numeric(Spend_Range_Max_EUR)) %>% 
  left_join(ggl_spend %>% distinct(Advertiser_ID, .keep_all = T) %>% select(Advertiser_ID, party1)) %>% 
  group_by(party1) %>% 
  mutate(total = sum(Spend_Range_Max_EUR)) %>% 
  mutate(perc = Spend_Range_Max_EUR/total) %>% 
  ungroup() %>% View
  # arrange(desc(Spend_Range_Max_EUR)) %>% View
  count(Gender_Targeting, sort = T) %>% View
  count(Geo_Targeting_Included, sort = T) %>%
  slice()
  # sample_n(5) %>% dput()
  mutate(Geo_Targeting_Included = str_remove_all(Geo_Targeting_Included, ",Netherlands")) %>% View

  

  Drenthe,Netherlands
  North Holland,Netherlands
  South Holland,Netherlands
  Utrecht,Netherlands
  North Brabant,Netherlands
  Overijssel,Netherlands
  Zeeland,Netherlands
  Limburg,Netherlands
  Flevoland,Netherlands
  Friesland,Netherlands
  Groningen,Netherlands
  Gelderland,Netherlands


  

  
  
  
  library(stringr)
  
  # your list of Dutch province names
  province_names <- c("Drenthe,Netherlands", "North Holland,Netherlands", "South Holland,Netherlands", "Utrecht,Netherlands", "North Brabant,Netherlands", "Overijssel,Netherlands", "Zeeland,Netherlands", "Limburg,Netherlands", "Flevoland,Netherlands", "Friesland,Netherlands", "Groningen,Netherlands", "Gelderland,Netherlands")
  
  # example strings to match
  
  # example strings to match
  example_strings <- c("South Holland,Netherlands",
                       "Zoeterwoude,Zoeterwoude,South Holland,Netherlands",
                       "Zuidhorn,Groningen,Netherlands", 
                       "Zuidplas,South Holland,Netherlands",
                       "North Brabant,Netherlands, Zwartewaterland,Overijssel,Netherlands, Zwijndrecht,Zwijndrecht,South Holland,Netherlands, het Bildt,Friesland,Netherlands", 
                       "North Brabant,Netherlands", 
                       "South Holland,Netherlands, Groningen,Netherlands")
  
  # pattern to match province names
  pattern <- paste0("(?<=^|,\\s)(?:", paste(province_names, collapse = "|"), ")(?=,|$)")
  
  # extract province names from example strings
  province_matches <- str_extract_all(example_strings, pattern)
  
  # print matches
  province_matches
  
  
sp_all_ads <- all_ads %>%
    filter(Advertiser_ID %in% ggl_sel_sp$advertiser_id) %>%
    mutate(Date_Range_Start = lubridate::ymd(Date_Range_Start)) %>%
    filter(Date_Range_Start >= as.Date("2023-02-05")) %>%
    mutate(Spend_Range_Max_EUR = as.numeric(Spend_Range_Max_EUR)) %>% 
    left_join(ggl_spend %>% distinct(Advertiser_ID, .keep_all = T) %>% select(Advertiser_ID, party1)) 

geo_sp_all <- sp_all_ads %>% 
    group_by(party1) %>% 
    mutate(total = sum(Spend_Range_Max_EUR)) %>% 
    mutate(perc = Spend_Range_Max_EUR/total) %>% 
    ungroup() %>% 
    # arrange(desc(Spend_Range_Max_EUR)) %>% View
    # count(Age_Targeting, sort = T) %>% View
    # count(Geo_Targeting_Included, sort = T) %>% 
    rowwise() %>% 
    mutate(provinces = paste0(unlist(str_extract_all(Geo_Targeting_Included, pattern)), collapse = "---")) %>% 
    ungroup() %>% 
    separate_rows(provinces, sep = "---") %>% 
    mutate(sep_entities_count = str_count(Geo_Targeting_Included, ", ")) %>% 
    mutate(sep_entities_count = sep_entities_count+1) %>% 
    mutate(province_count = str_count(Geo_Targeting_Included, provinces)) %>% 
    mutate(same_provinces = sep_entities_count==province_count) %>% 
    # sample_n(20) %>% 
    mutate(partial_spend = case_when(
      !same_provinces ~ Spend_Range_Max_EUR/sep_entities_count,
      same_provinces ~ Spend_Range_Max_EUR
    )) %>% 
    select(Spend_Range_Max_EUR, partial_spend, everything()) %>% 
    arrange(desc(partial_spend)) %>% 
    group_by(party1, provinces) %>% 
    summarize(partial_spend = sum(partial_spend)) %>% 
    mutate(provinces = str_remove_all(provinces, ",Netherlands")) %>% 
    drop_na(partial_spend) %>% 
    ungroup()


hc_geo <- sp_all_ads %>% 
  filter(Geo_Targeting_Included == "Netherlands") %>% 
  group_by(party1) %>% 
  summarize(Spend_Range_Max_EUR = sum(Spend_Range_Max_EUR)) %>% 
  mutate(partial_spend = Spend_Range_Max_EUR/12) %>% 
  expand_grid(province_names) %>% 
  select(party1, partial_spend, provinces = province_names) %>% 
  mutate(provinces = str_remove_all(provinces, ",Netherlands")) %>% 
  bind_rows(geo_sp_all) %>% 
  group_by(party1, provinces) %>%
  summarize(partial_spend = sum(partial_spend)) %>% 
  mutate(name = case_when(
    str_detect(provinces, "North Holland") ~ "Noord-Holland",
    str_detect(provinces, "South Holland") ~ "Zuid-Holland",
    str_detect(provinces, "North Brabant") ~ "Noord-Brabant",
    T ~ provinces
  ))



hc_geo %>% 
  group_split(party1) %>% 
  map(~{chart_maps(.x, F, mapdata)}) %>% hw_grid(ncol = 4) %>% 
  htmltools::browsable()

chart_maps(hc_geo %>% filter(party1 == "Volt Nederland"), F, mapdata)

library(highcharter)
mapdata <- get_data_from_map(download_map_data("https://code.highcharts.com/mapdata/countries/nl/nl-all.js"))

mapdata %>% count(name, sort =T)

chart_maps <- function(x, download_data = T, mapdata) {
  hc <- hcmap2(
    "https://code.highcharts.com/mapdata/countries/nl/nl-all.js",
    custom_map = mapdata,
    data = x,
    download_map_data = T,
    value = "partial_spend ",
    joinBy = c("name", "name"),
    # name = trans_internal$plot_tooltip_geo,
    dataLabels = list(enabled = TRUE, format = "{point.name}"),
    borderColor = "#FAFAFA",
    borderWidth = 0.2,
    tooltip = list(
      valueDecimals = 0,
      valueSuffix = "€"
    )
  ) %>% 
    # hc_colorAxis(
    #   minColor = "white",
    #   maxColor = unique(x$colorful),
    #   min = 0,
    #   max = 40
    # )%>% 
    hc_title(
      text = unique(x$party1)
    ) %>%
    hc_exporting(
      enabled = TRUE
    )
  
  # download_data <<- F
  
  return(hc)
}



fb_aggr %>% 
  hc_plotter(filters = dutch_parties_fb,
             plot_type = unlist_it(trans$choices, 4),
             plot_type_sub = unlist_it(trans$targeted_ads_choices, 3),
             platform = "Facebook",
             mapdata = map_data,
             trans_internal = trans,
             last_updated = update_time, minmax = "Minimum"
  )

library(tidyverse)
total_spend_id <- election_dat30 %>% 
  distinct(internal_id, .keep_all = T) %>% 
  group_by(party) %>% 
  summarize(spend = sum(total_spend_formatted)) %>% 
  ungroup() %>% 
  mutate(platform = "Meta")
  


hc_data <-  ggl_daily %>%
  rename(Advertiser_ID = advertiser_id) %>%
  left_join(ggl_spend %>% distinct(Advertiser_ID, party1)) %>% 
  janitor::clean_names()  %>% 
  rename(party = party1) %>% 
  mutate(date_produced = lubridate::ymd(date)) %>%
  mutate(spend = readr::parse_number(str_remove(eur_amount, "\\."))) %>%  
  group_by(date_produced, party) %>% 
  summarize(spend  = sum(spend)) %>% 
  ungroup() %>% 
  # mutate(party = ifelse(party == "JA21", "Ja21", party))  %>%
  group_by(party) %>%
  mutate(total_spend = max(spend)) %>%
  ungroup()  %>%
  left_join(color_dat, by = "party") %>%
  mutate(party = as.factor(party)) %>% 
  mutate(party = fct_reorder(party, total_spend)) %>% 
  filter(date_produced >= as.Date("2023-02-11") & date_produced <= as.Date("2023-03-15")) %>% 
  group_by(party) %>% 
  summarize(spend = sum(spend)) %>% 
  ungroup() %>% 
  mutate(platform = "Google")



platform_dat <- hc_data %>% 
  bind_rows(total_spend_id) %>% 
  group_by(party) %>% 
  mutate(total = sum(spend)) %>% 
  mutate(perc = spend/total)

lab_dat <- platform_dat %>% 
  distinct(party, .keep_all = T) %>%
  # filter(party == "VVD") %>% 
  mutate(labb = paste0("€", scales::comma(round(total)))) %>% 
  select(party, labb)

the_order <- platform_dat %>% 
  filter(platform == "Meta") %>%   arrange(desc(perc)) %>% 
  pull(party) %>% 
  unique()


platform_dat %>% 
  # mutate(party = fct_reorder(party, total)) %>% 
  left_join(lab_dat) %>% 
  mutate(party = factor(party, the_order)) %>% 
  mutate(platform = factor(platform, c("Meta", "Google"))) %>% 
  drop_na(party) %>% 
  ggplot(aes(party, perc))  +
  geom_col(aes(fill = platform), position = position_stack(reverse = T), alpha = 0.8) +
  coord_flip() +
  geom_label(aes(label = labb),y=1.225,
             position = position_stack(vjust = 0.5),
             hjust = 1, label.size = NA,
             size = 4) + expand_limits(y = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_fill_manual("Platform", values = c("#ff2700", "#008fd5") %>% rev) +
  ggthemes::theme_hc() +
  labs(x = "", y = "% of budget spent on Platform", title = "Meta vs. Google", subtitle = "Where do Dutch parties focus their money?", 
       caption = "Source: Meta Ad Library, Google Transparency Report & data compiled by Who Targets Me.\nData Viz: Fabio Votta (@favstats). Timeframe: 13th Feb - 14th Mar 2023.") +
  theme(legend.position = "bottom", plot.title = element_text(size = 20, face = "bold", hjust = 0.35), text=element_text(family="mono", face = "bold"), 
        plot.caption = element_text(size = 8)) +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) 
  
ggsave("img/ggl_vs_meta.png", width = 6, height = 8, dpi = 300)



platform_dat %>% 
  distinct(party, .keep_all = T) %>%
  ungroup() %>% 
  mutate(party = fct_reorder(party, total)) %>% 
  ggplot(aes(party, total)) +
  geom_col(aes(fill = party)) +
  coord_flip() +
  scale_fill_parties()  +
  ggthemes::theme_hc() +
  theme(legend.position = "none", plot.subtitle = element_text(size = 9, hjust = 0.3), 
        plot.title = element_text(size = 15, face = "bold", hjust = 0.35), text=element_text(family="mono", face = "bold", size = 9), 
        plot.caption = element_text(size = 5))  +
  labs(x = "", y = "Total Budget on Google (incl. YouTube) & Meta (Facebook & Instagram) ads", title = "Digital Campaigning in the Netherlands", subtitle = "How much did Dutch parties spend on Meta & Google during Provincial Elections?", 
       caption = "Source: Meta Ad Library, Google Transparency Report & data compiled by Who Targets Me.\nData Viz: Fabio Votta (@favstats). Timeframe: 13th Feb - 14th Mar 2023.")  +
  geom_text(aes(label = paste0("€",scales::comma_format()(total))),#y=1.225,
             # position = position_stack(vjust = 0.5),
             hjust = 1.15, label.size = NA, color = "white",
             size = 3) 
  
ggsave("img/total_spend.png", width = 8, height = 5, dpi = 300)

platformsum <- platform_dat %>% 
  # mutate(party = fct_reorder(party, total)) %>% 
  left_join(lab_dat) %>% 
  mutate(party = factor(party, the_order)) %>% 
  mutate(platform = factor(platform, c("Meta", "Google"))) %>% 
  drop_na(platform) %>% 
  group_by(platform) %>% 
  summarize(total = sum(spend)) 
platformsum$total[1]/sum(platformsum$total)


totalgoogle <- 268150


more_data_ggl <- ggl_daily %>%
  rename(Advertiser_ID = advertiser_id) %>%
  left_join(ggl_spend %>% distinct(Advertiser_ID, party1)) %>% 
  janitor::clean_names()  %>% 
  rename(party = party1) %>% 
  mutate(date_produced = lubridate::ymd(date)) %>%
  mutate(spend = readr::parse_number(str_remove(eur_amount, "\\."))) %>%  
  group_by(date_produced, party) %>% 
  summarize(spend  = sum(spend)) %>% 
  ungroup() %>% 
  mutate(party = ifelse(party == "JA21", "Ja21", party))  %>%
  group_by(party) %>%
  mutate(total_spend = max(spend)) %>%
  ungroup()  %>%
  left_join(color_dat, by = "party") %>%
  mutate(party = as.factor(party)) %>% 
  mutate(party = fct_reorder(party, total_spend)) %>% 
  filter(date_produced >= as.Date("2023-02-11") & date_produced <= as.Date("2023-03-13"))# %>% 
  # summarise(spend =sum(spend))


more_data <- dir("data/reports", full.names = T) %>% 
  map_dfr(~read_csv(.x) %>% mutate(path = .x)) %>% 
  mutate(date_produced = str_remove_all(path, "data/reports/FacebookAdLibraryReport_|_NL_yesterday_advertisers\\.csv")) %>% 
  mutate(date_produced = lubridate::ymd(date_produced)) %>% 
  janitor::clean_names()%>% rename(advertiser_id = page_id) %>% 
  mutate(spend = readr::parse_number(amount_spent_eur)) %>% 
  # bind_rows(nlsb %>% 
              # janitor::clean_names() ) %>% 
  mutate(spend = ifelse(spend == 100, 50, spend))%>% 
  filter(date_produced >= as.Date("2023-02-13"))  %>% 
  # mutate(advertiser_id = as.character(advertiser_id)) %>% 
  left_join(fi_advertisers %>% rename(advertiser_id = page_id) %>% 
              select(advertiser_id, party)) %>% 
  drop_na(party)



platform_dat_daily <- more_data_ggl %>% 
  mutate(platform = "Google") %>% 
bind_rows(more_data%>% 
            mutate(platform = "Meta")) %>% 
group_by(party) %>% 
mutate(total = sum(spend)) %>% 
mutate(perc = spend/total) %>% 
  ungroup()

total_spend <- platform_dat_daily %>% 
  # group_by(party, date_produced) %>% 
  # summarize(spend = sum(spend)) %>% 
  ungroup() %>%
  drop_na(party)  %>% #View
  summarise(spend = sum(spend))

last_7_days <- platform_dat_daily %>% 
  filter(date_produced >= as.Date("2023-03-06")) %>% 
  # group_by(party, date_produced) %>% 
  # summarize(spend = sum(spend)) %>% 
  ungroup() %>%
  drop_na(party)  %>% #View
  summarise(spend = sum(spend))

after6 <- round(last_7_days$spend/total_spend$spend*100, 2)


platform_dat_daily %>% 
  group_by(party, date_produced) %>% 
  summarize(spend = sum(spend)) %>% 
  ungroup() %>% 
  drop_na(party) %>% 
  mutate(party = fct_reorder(party, spend, .fun = sum)) %>% 
  ggplot(aes(date_produced, spend)) +
  geom_area(position = position_stack(), aes(fill = party), alpha = 0.85) +
  scale_fill_parties() +
  ggthemes::theme_hc() +
  labs(x = "", y = "Daily Budget on Meta and Google Ads", title = "Daily Spending in 2023 Provincial Elections", subtitle = "How much did Dutch parties spend on Meta & Google during Provincial Elections?", 
       caption = "Source: Meta Ad Library, Google Transparency Report & data compiled by Who Targets Me.\nData Viz: Fabio Votta (@favstats). Timeframe: 13th Feb - 14th Mar 2023.") +
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 15, hjust = 0.35),
        plot.title = element_text(size = 28, face = "bold", hjust = 0.35), text=element_text(family="mono", face = "bold"), 
        plot.caption = element_text(size = 8)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE, reverse = T)) +
  scale_x_date(date_breaks = "4 days", date_labels = "%b %d") +
  geom_vline(xintercept = as.Date("2023-03-06"), linetype = "dashed") +
  annotate(geom = "label", label = glue::glue("{after6}% of total budget spend after March 6th"), x = as.Date("2023-03-02"), y = 125000, size = 4)

ggsave("img/daily_spend.png", width = 12, height = 8, dpi = 300)






total_budget <- rvest::read_html("https://www.rtlnieuws.nl/nieuws/politiek/artikel/5357473/partijen-geven-kwart-meer-uit-aan-campagne-provinciale") %>% 
  rvest::html_table() %>% 
  .[[1]] %>% 
  set_names(.[1,] %>% as.character) %>% 
  slice(-1) %>% 
  janitor::clean_names() %>% 
  mutate(budget_ps_2023 = ifelse(partij == "PvdA", budget_ps_2019, budget_ps_2023)) %>% 
  mutate(budget_ps_2023 = str_remove_all(budget_ps_2023, "\\.")) %>% 
  mutate(budget_ps_2023  = readr::parse_number(budget_ps_2023)) %>% 
  rename(party = partij)

digital_budget <- platform_dat %>% 
  # mutate(party = fct_reorder(party, total)) %>% 
  left_join(lab_dat) %>% 
  # mutate(party = factor(party, the_order)) %>% 
  # filter(is.na(party))
  mutate(platform = factor(platform, c("Meta", "Google"))) %>% 
  drop_na(platform) %>% 
  group_by(party) %>% 
  summarize(spend_digital = sum(spend)) 

total_budget %>% 
  mutate(party = case_when(
    party == "Partij voor de Dieren" ~ "PvdD",
    party == "Volt" ~ "Volt Nederland",
    party == "Van Haga/bvNL" ~ "BVNL",
    T ~ party
  )) %>% 
  left_join(digital_budget) %>% 
  mutate(perc = spend_digital/budget_ps_2023*100) %>% 
  arrange(desc(perc))  %>% 
  mutate(party = fct_reorder(party, perc)) %>% 
  ggplot(aes(party, perc)) +
  geom_col(aes(fill = party)) +
  coord_flip() +
  scale_fill_parties()  +
  ggthemes::theme_hc() +
  theme(legend.position = "none", plot.subtitle = element_text(size = 8, hjust = 0.3), 
        plot.title = element_text(size = 13, face = "bold", hjust = 0.35), text=element_text(family="mono", face = "bold", size = 9), 
        plot.caption = element_text(size = 5))  +
  labs(x = "", y = "% of Total Budget Spend on Google (incl. YouTube) & Meta (Facebook & Instagram) ads", title = "Digital Campaigning in the 2023 Dutch Provincial Elections", subtitle = "How much did Dutch parties spend on digital ads compared to their total campaign budget?", 
       caption = "Source: Meta Ad Library, Google Transparency Report, RTL Nieuws & data compiled by Who Targets Me.\nData Viz: Fabio Votta (@favstats). Timeframe: 13th Feb - 14th Mar 2023.")  +
  geom_text(aes(label = round(perc)),#y=1.225,
            # position = position_stack(vjust = 0.5),
            hjust = 1.45, label.size = NA, color = "white",
            size = 3) +
  annotate(geom = "text", label = "No Numbers on Total Budget", x = 15.5, y = 8, size = 3)+
  annotate(geom = "label", label = "*2019 Budget", x = 13, y = 80, size = 3, label.size = NA)

ggsave("img/digital_spend.png", width = 8, height = 5, dpi = 300)
