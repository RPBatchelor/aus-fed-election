

url <- "https://en.wikipedia.org/wiki/Pre-election_pendulum_for_the_2019_Australian_federal_election"
page <- read_html(url)
page_data <- html_table(page)


gov_seats <- page_data[[2]]
opp_seats <- page_data[[3]]


names <- c("seat", "member", "incumbent", "margin")
names(gov_seats) <- names(opp_seats) <- names


oz_pendulum_2019 <- bind_rows(gov_seats, opp_seats) |> 
  as_tibble() |> 
  slice(-(1:3)) |> 
  filter(str_detect(seat, "\\w* \\(\\w{1,3}\\)"),
         !seat %in% c("Crossbench seats - 2016 (6)", "Opposition seats - 2016 (72)")) |> 
  mutate(state = str_extract(seat, "\\(\\w{1,3}\\)"),
         state = str_remove(state, "\\("),
         state = str_remove(state, "\\)")) |> 
  mutate(seat = str_squish(str_remove(seat, "\\(\\w{1,3}\\)"))) |> 
  mutate(remarks = str_extract(incumbent, "v \\w{3}"),
         remarks = if_else(is.na(remarks), str_extract(margin, "v \\w{3}"), remarks)) |> 
  mutate(margin = str_remove(margin, "v \\w{3}")) |> 
  mutate(incumbent = str_remove(incumbent, "v \\w{3}")) |> 
  mutate(party_against = case_when(incumbent == "ALP" ~ "LNP",
                                   incumbent %in% c("LIB", "NAT", "LNP") ~ "ALP",
                                   TRUE ~ str_squish(str_replace(remarks, "[v\\+]", "")))) |> 
  mutate(margin = as.numeric(margin) - 50) |> 
  rename(division = seat)
  

save(oz_pendulum_2019, file = "pkg/data/oz_pendulum_2019.rda")


rm(gov_seats, opp_seats, page, page_data)


# Original from Peter Ellis
# 
# redist_page <- read_html("https://www.abc.net.au/news/elections/federal-redistribution-2018/")
# redist_data <- html_table(redist_page)[[2]] 
# 
# 
# 
# coal_seats <- redist_data[2:74, 1:3]
# alp_seats <- redist_data[2:73, 4:6]
# oth_seats <- redist_data[75:80, 4:6]
# 
# names(coal_seats) <- names(alp_seats) <- names(oth_seats) <- c("state", "division", "var")
# 
# oz_pendulum_2019 <- rbind(coal_seats, alp_seats, oth_seats) %>%
#   as_tibble() %>%
#   separate(var, into = c("incumbent", "margin"), sep = " ") %>%
#   mutate(remarks = str_extract(division, "\\(.+\\)"),
#          remarks = gsub("[\\(\\)]", "", remarks),
#          division = str_squish(gsub("\\(.+\\)", "", division)),
#          party_against = case_when(
#            incumbent == "ALP"              ~ "Lib/Nat",
#            incumbent %in% c("LIB", "NAT", "LNP")  ~ "ALP",
#            TRUE                            ~ str_squish(gsub("[v\\+]", "", remarks))
#          )) %>%
#   select(state, division, incumbent, margin, party_against, remarks) %>%
#   mutate(margin = as.numeric(margin))
# 
# save(oz_pendulum_2019, file = "pkg/data/oz_pendulum_2019.rda")
