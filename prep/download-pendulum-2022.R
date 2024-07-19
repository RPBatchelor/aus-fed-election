


url <- "https://en.wikipedia.org/wiki/Pre-election_pendulum_for_the_2022_Australian_federal_election"
page <- read_html(url)
page_data <- html_table(page)


gov_seats <- page_data[[2]]
opp_seats <- page_data[[3]]


names <- c("seat", "state", "member", "incumbent", "margin")
names(gov_seats) <- names(opp_seats) <- names


new_pendulum_2022 <- bind_rows(gov_seats, opp_seats) |> 
  as_tibble() |> 
  slice(-(1:2)) |>
  mutate(margin = suppressWarnings(as.numeric(margin))) |> 
  drop_na(margin) |> 
  mutate(remarks = str_extract(incumbent, "vs. \\w{3}"),
         remarks = if_else(is.na(remarks), str_extract(margin, "vs. \\w{3}"), remarks)) |> 
  mutate(incumbent = str_remove(incumbent, "vs. \\w{3}"),
         incumbent = str_remove(incumbent, "\\(b/e\\)"),
         incumbent = str_remove(incumbent, "vs PHON")) |>
  mutate(party_against = case_when(incumbent == "ALP" ~ "LNP",
                                   incumbent %in% c("LIB", "NAT", "LNP") ~ "ALP",
                                   TRUE ~ str_squish(str_replace(remarks, "vs. ", ""))))




save(new_pendulum_2022, file = "pkg/data/oz_pendulum_2022.rda")
