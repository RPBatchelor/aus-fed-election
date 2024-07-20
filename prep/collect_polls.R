

#--------------2010 election-----------------
url = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2010_Australian_federal_election"
tab_names = c("dates", 
              paste0(c("ALP", "Lib", "Nat", "Grn", "Oth"), "!First preference"), 
              "ALP!Two-party-preferred", "Lib/Nat!Two-party-preferred", 
              "x1", "x2", "x3")

last_election_date = as.Date("2007-11-24")
election_year = 2010

webpage <- url |> 
  read_html(encoding = "UTF-8") 

tabs <- webpage |> 
  html_nodes("table") 

# number below depends on the webpage...
tab1 <- html_table(tabs[[1]], fill = TRUE) 

names(tab1) <- tab_names

tab1 <- tab1 |>
  select(1:8) |> 
  slice(-1) |> 
  as_tibble() |> 
  mutate(firm = "Newspoll")


tab2 <- html_table(tabs[[5]], fill = TRUE) 

names(tab2) <- tab_names

tab2 <- tab2 |> 
  select(1:8) |> 
  slice(-1) |> 
  as_tibble() |> 
  mutate(firm = "Roy Morgan")


ozpolls_2010 <- bind_rows(tab1, tab2) |> 
  mutate(wiki_row = paste0("r", 1:n())) |> 
  pivot_longer(cols = 2:8, names_to = "variable", values_to = "intended_vote") |> 
  separate(variable, sep = "!", into = c("party", "preference_type")) |> 
  mutate(intended_vote = suppressWarnings(as.numeric(str_replace(intended_vote, "%", "")))) |> 
  mutate(election_year = election_year) |> 
  filter(!is.na(intended_vote)) |> 
  parse_dates() |> 
  mutate(start_date = if_else(original_dates == "2007 election", last_election_date, start_date),
         end_date = if_else(original_dates == "2007 election", last_election_date, end_date),
         firm = if_else(original_dates == "2007 election", "Election result", firm)) |> 
  filter(!is.na(start_date))

stopifnot(sum(is.na(ozpolls_2010$start_date)) == 0)




#--------------2013 election-----------------
url = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2013_Australian_federal_election"
tab_names = c("dates", "firm",
              paste0(c("ALP", "Lib/Nat", "Grn", "Oth"), "!First preference"), 
              "ALP!Two-party-preferred", "Lib/Nat!Two-party-preferred", 
              "x1")

last_election_date = as.Date("2010-08-21")
election_year = 2013

webpage <- url |> 
  read_html(encoding = "UTF-8") 

tabs <- webpage |> 
  html_nodes("table") 

# number below depends on the webpage...
tab <- html_table(tabs[[2]], fill = TRUE) 

names(tab) <- tab_names

ozpolls_2013 <- tab %>%
  select(1:8) |> 
  as_tibble() |> 
  mutate(wiki_row = paste0("r", 1:n())) |> 
  pivot_longer(cols = 3:8, names_to = "variable", values_to = "intended_vote") |> 
  separate(variable, sep = "!", into = c("party", "preference_type")) |> 
  mutate(intended_vote = suppressWarnings(as.numeric(str_replace(intended_vote, "%", "")))) |> 
  mutate(election_year = election_year) |> 
  filter(!is.na(intended_vote)) |> 
  parse_dates() |> 
  mutate(start_date = if_else(original_dates == "2010 election", last_election_date, start_date),
         end_date = if_else(original_dates == "2010 election", last_election_date, end_date),
         firm = if_else(original_dates == "2010 election", "Election result", firm)) |> 
  mutate(firm = str_replace(firm, "\\[.+\\]", ""),
         firm = str_squish(firm))


stopifnot(sum(is.na(ozpolls_2013$start_date)) == 0 )


#-----------------2016-------------
url <- "https://en.wikipedia.org/wiki/National_opinion_polling_for_the_2016_Australian_federal_election"
# parties in different order from 2013 and 2010:
tab_names = c("dates", "firm",
              paste0(c("Lib/Nat", "ALP", "Grn", "Oth"), "!First preference"), 
              "Lib/Nat!Two-party-preferred", "ALP!Two-party-preferred",  
              "sample_size", "margin_of_error", "method",
              "x1")

last_election_date = as.Date("2013-09-07")
election_year = 2016

webpage <- url |> 
  read_html(encoding = "UTF-8") 

tabs <- webpage |> 
  html_nodes("table") 

# number below depends on the webpage...
tab <- html_table(tabs[[3]], fill = TRUE) 

names(tab) <- tab_names

ozpolls_2016 <- tab |> 
  select(1:11) |> 
  as_tibble() |> 
  mutate(wiki_row = paste0("r", 1:n())) |> 
  pivot_longer(cols = 3:8, names_to = "variable", values_to = "intended_vote") |> 
  separate(variable, sep = "!", into = c("party", "preference_type")) |> 
  mutate(intended_vote = suppressWarnings(as.numeric(str_replace(intended_vote, "%", "")))) |> 
  mutate(sample_size = suppressWarnings(as.numeric(str_replace(sample_size, ",", "")))) |> 
  mutate(election_year = election_year) |> 
  filter(!is.na(intended_vote)) |> 
  parse_dates() |> 
  mutate(start_date = if_else(original_dates == "2013 election", last_election_date, start_date),
         end_date = if_else(original_dates == "2013 election", last_election_date, end_date),
         firm = if_else(original_dates == "2013 election" | firm == "2013 election", "Election result", firm)) %>%
  mutate(firm = gsub("\\[.+\\]", "", firm),
         firm = gsub("\\(.+\\)", "", firm),
         firm = str_squish(firm))
  

stopifnot(sum(is.na(ozpolls_2016$start_date)) == 0 )

#-----------------2019-------------
url <- "https://en.wikipedia.org/wiki/National_opinion_polling_for_the_2019_Australian_federal_election"
# parties in different order from 2013 and 2010:
tab_names = c("dates", "firm",
              paste0(c("Lib/Nat", "ALP", "Grn", "ONP", "Oth"), "!First preference"), 
              "Lib/Nat!Two-party-preferred", "ALP!Two-party-preferred",  
              "x1", "x2", "x3")

last_election_date = as.Date("2016-07-02")
election_year = 2019

webpage <- url |> 
  read_html(encoding = "UTF-8") 

tabs <- webpage |> 
  html_nodes("table") 

# number below depends on the webpage...
tab <- html_table(tabs[[2]], fill = TRUE) 

names(tab) <- tab_names

ozpolls_2019 <- tab |> 
  select(1:9) |> 
  as_tibble() |> 
  mutate(wiki_row = paste0("r", 1:n())) |>
  pivot_longer(cols = 3:9, names_to = "variable", values_to = "intended_vote") |> 
  separate(variable, sep = "!", into = c("party", "preference_type")) |> 
  mutate(intended_vote = suppressWarnings(as.numeric(str_replace(intended_vote, "%", "")))) |> 
  mutate(election_year = election_year) %>%
  filter(!is.na(intended_vote)) %>%
  parse_dates() %>%
  mutate(start_date = if_else(original_dates == "2 July 2016 election", last_election_date, start_date),
         end_date = if_else(original_dates == "2 July 2016 election", last_election_date, end_date),
         firm = if_else(original_dates == "2 July 2016 election", "Election result", firm)) %>%
  mutate(firm = gsub("\\[.+\\]", "", firm),
         firm = gsub("\\(.+\\)", "", firm),
         firm = str_squish(firm)) |> 
  filter(!is.na(start_date))
  

stopifnot(sum(is.na(ozpolls_2019$start_date)) == 0 )

#-------------------2022----------------------------
url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2022_Australian_federal_election"

tab1_names = c("dates", "firm", "method", "sample_size",
              paste0(c("Lib/Nat", "ALP", "Grn", "ONP", "UAP", "Oth", "Und"), "!First preference"),
              "Lib/Nat!Two-party-preferred", "ALP!Two-party-preferred",  
              "x1", "x2", "x3")

tab2_names = c("dates", "firm", "method", "sample_size",
               paste0(c("Lib/Nat", "ALP", "Grn", "ONP", "Oth", "Und"), "!First preference"),
               "Lib/Nat!Two-party-preferred", "ALP!Two-party-preferred",  
               "x1", "x2", "x3")

last_election_date = as.Date("2019-05-18")
election_year = 2022


webpage <- url |> 
  read_html(encoding = "UTF-8") 

tabs <- webpage |> 
  html_nodes("table") 

# number below depends on the webpage...
tab1 <- html_table(tabs[[2]], fill = TRUE) 

names(tab1) <- tab1_names

tab1 <- tab1 |> 
  select(1:13) |> 
  slice(-1) |> 
  as_tibble()


tab2 <- html_table(tabs[[3]], fill = TRUE)

names(tab2) <- tab2_names

tab2 <- tab2 |> 
  select(1:12) |> 
  slice(-1) |> 
  as_tibble() |> 
  mutate()



ozpolls_2022 <- bind_rows(tab1, tab2) |> 
  select(1:13) |> 
  as_tibble() |> 
  mutate(wiki_row = paste0("r", 1:n())) |>
  pivot_longer(cols = 5:13, names_to = "variable", values_to = "intended_vote") |> 
  separate(variable, sep = "!", into = c("party", "preference_type")) |> 
  mutate(intended_vote = suppressWarnings(as.numeric(str_replace(intended_vote, "%", "")))) |> 
  mutate(sample_size = suppressWarnings(as.numeric(str_replace(sample_size, ",", "")))) |> 
  mutate(election_year = election_year) |> 
  filter(!is.na(intended_vote)) |> 
  parse_dates() |> 
  mutate(firm = if_else(firm == "Election", "Election result", firm)) |> 
  mutate(firm = gsub("\\[.+\\]", "", firm),
         firm = gsub("\\(.+\\)", "", firm),
         firm = str_squish(firm)) |> 
  filter(!is.na(start_date))


stopifnot(sum(is.na(ozpolls_2022$start_date)) == 0 )


#-----------2025 election ----------------------

url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Australian_federal_election"

last_election_date = as.Date("2022-05-21")
election_year = 2025

tab_names <- c("dates", "firm", "method", "sample_size",
               paste0(c("Lib/Nat", "ALP", "Grn", "ONP", "UAP", "Oth", "Und"), "!First preference"),
               "ALP!Two-party-preferred", "Lib/Nat!Two-party-preferred")


webpage <- url |> 
  read_html(encoding = "UTF-8") 

tabs <- webpage |> 
  html_nodes("table") 

# number below depends on the webpage...
tab1 <- html_table(tabs[[1]], fill = TRUE) 

names(tab1) <- tab_names

tab1 <- tab1 |> 
  slice(-1) |> 
  as_tibble()


tab2 <- html_table(tabs[[2]], fill = TRUE) 

names(tab2) <- tab_names

tab2 <- tab2 |> 
  slice(-1) |> 
  as_tibble()



tab3 <- html_table(tabs[[3]], fill = TRUE) 

names(tab3) <- tab_names

tab3 <- tab3 |> 
  slice(-1) |> 
  as_tibble()



ozpolls_2025 <- bind_rows(tab1, tab2, tab3) |> 
  mutate(wiki_row = paste0("r", 1:n())) |>
  pivot_longer(cols = 5:13, names_to = "variable", values_to = "intended_vote") |> 
  separate(variable, sep = "!", into = c("party", "preference_type")) |> 
  mutate(intended_vote = suppressWarnings(as.numeric(str_replace(intended_vote, "%", "")))) |> 
  mutate(sample_size = suppressWarnings(as.numeric(str_replace(sample_size, ",", "")))) |> 
  mutate(election_year = election_year) |> 
  filter(!is.na(intended_vote)) |> 
  parse_dates() |> 
  mutate(firm = if_else(firm == "Election", "Election result", firm)) |> 
  mutate(firm = gsub("\\[.+\\]", "", firm),
         firm = gsub("\\(.+\\)", "", firm),
         firm = str_squish(firm)) |> 
  filter(!is.na(start_date))


stopifnot(sum(is.na(ozpolls_2025$start_date)) == 0 )



#----------------Combine-----------------

ozpolls <- bind_rows(ozpolls_2010, 
                     ozpolls_2013,
                     ozpolls_2016,
                     ozpolls_2019,
                     ozpolls_2022,
                     ozpolls_2025) |> 
  select(-sample_size, -margin_of_error, -method) |> 
  mutate(firm = str_squish(str_replace(firm, "\\(.+\\)", "")),
         firm = if_else(firm == "Morgan", "Roy Morgan", firm),
         firm = if_else(firm == "ReachTel", "ReachTEL", firm)) |> 
  mutate(mid_date = start_date + (as.numeric(end_date) - as.numeric(start_date)) / 2) |> 
  mutate(party = if_else(party %in% c("Lib", "Nat"), "Lib/Nat", party)) |> 
  group_by_if(function(x){!is.numeric(x) | min(x) > 1000}) |> 
  summarise(intended_vote = sum(intended_vote)) |> 
  ungroup() |> 
  select(-wiki_row)



save(ozpolls, file = "pkg/data/ozpolls.rda", compress = "xz")

ozpolls_2016 <- select(ozpolls_2016, -wiki_row)
ozpolls_2010 <- select(ozpolls_2010, -wiki_row)

# Two of the data frames have reasons for independent existence (see helpfile for why):
save(ozpolls_2016, file = "pkg/data/ozpolls_2016.rda", compress = "xz")
save(ozpolls_2010, file = "pkg/data/ozpolls_2010.rda", compress = "xz")

# This text version is basically so Git can observe changes
write_csv(ozpolls, file = "comparison-data/ozpolls.csv")

