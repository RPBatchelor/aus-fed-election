

#--------2016 Data pack-------------------------------------------

url_2016 <- "https://www.abs.gov.au/census/find-census-data/datapacks/download/2016_GCP_CED_for_AUS_short-header.zip"
file_location <- "raw-data/census-datapack-2016.zip"


download_if_fresh(url_2016, file_location)
unzip(file_location, exdir = "raw-data/")

metadata <- readxl::read_xlsx("raw-data/Metadata/2016Census_geog_desc_1st_and_2nd_release.xlsx",
                              sheet = "2016_ASGS_Non-ABS_Structures") |> 
  clean_names() |> 
  filter(asgs_structure == "CED")


raw_data <- read_csv("raw-data/2016 Census GCP Commonwealth Electoral Divisions for AUST/2016Census_G01_AUS_CED.csv")

census_2016 <- raw_data |> 
  clean_names() |> 
  pivot_longer(cols = !ced_code_2016,
               names_to = "variable_raw",
               values_to = "value") |> 
  left_join(metadata, by = c("ced_code_2016" = "census_code_2016")) |> 
  mutate(sex = str_sub(variable_raw, -1, -1),
         sex = case_when(sex == "m" ~ "male",
                         sex == "f" ~ "female",
                         sex == "p" ~ "persons",
                         T ~ "check")) |> 
  mutate(variable = case_when(str_detect(variable_raw, "age_\\d") ~ "age_group",
                              str_detect(variable_raw, "tot_p") ~ "total_persons",
                              str_detect(variable_raw, "birthplace") ~ "birthplace",
                              str_detect(variable_raw, "lang_spoken_home_eng_only") ~ "only_english_spoken_home",
                              str_detect(variable_raw, "high_yr_schl") ~ "high_school_equiv_year",
                              str_detect(variable_raw, "indig") ~ "indigenous")) |> 
  mutate(variable = if_else(str_detect(variable_raw, "indigenous_p_tot"),
                            "indigenous", variable)) |> 
  mutate(sub_age_grp = if_else(variable == "age_group", 
                                variable_raw, NA),
         sub_age_grp = str_remove(sub_age_grp, "age_"),
         sub_age_grp = str_remove(sub_age_grp, "_yr_\\w"),
         sub_age_grp = str_remove(sub_age_grp, "ov_\\w"),
         sub_age_grp = str_replace(sub_age_grp, "_", "-"),
         sub_age_grp = if_else(sub_age_grp == "85", 
                                "85 and over",
                               sub_age_grp)) |> 
  mutate(sub_birthplace = if_else(variable == "birthplace",
                                  variable_raw, NA),
         sub_birthplace = str_remove(sub_birthplace, "birthplace_"),
         sub_birthplace = str_remove(sub_birthplace, "_\\w")) |> 
  mutate(sub_indig = if_else(variable == "indigenous",
                             variable_raw, NA),
         sub_indig = if_else(str_detect(sub_indig, "aboriginal"),
                             "aboriginal", sub_indig),
         sub_indig = if_else(str_detect(sub_indig, "indigenous"),
                             "total indigenous", sub_indig),
         sub_indig = if_else(str_detect(sub_indig, "torres_strait"),
                             "torres strait", sub_indig),
         sub_indig = if_else(str_detect(sub_indig, "indig_bth"),
                             "abor and torres str", sub_indig)) |> 
  mutate(school_equiv = if_else(variable == "high_school_equiv_year",
                                variable_raw, NA),
         school_equiv = case_when(str_detect(school_equiv, "12") ~ "year 12",
                                  str_detect(school_equiv, "11") ~ "year 11",
                                  str_detect(school_equiv, "10") ~ "year 10",
                                  str_detect(school_equiv, "9") ~ "year 9",
                                  str_detect(school_equiv, "8") ~ "year 8 below",
                                  str_detect(school_equiv, "d_n_g") ~ "did not graduate",
                                  T ~ "check"))



# Non-voting young people
d1 <- census_2016 |> 
  filter(sex == "persons",
         variable == "age_group") |> 
  group_by(ced_code_2016) |> 
  summarise(nv_young_people = sum(value[sub_age_grp %in% c("0-4", "5-14", "14-19")]) / sum(value),
            v_young_people = sum(value[sub_age_grp %in% c("20-24", "25-34")]) / sum(value),
            v_mid_people = sum(value[sub_age_grp %in% c("35-44", "45-54", "55-64")]) / sum(value),
            v_old_people = sum(value[sub_age_grp %in% c("65-74", "75-84", "85 and over")]) / sum(value))


# Proportion of people born in Australia
d2 <- census_2016 |> 
  filter(sex == "persons",
         variable == "birthplace") |> 
  group_by(ced_code_2016) |> 
  summarise(born_aust = sum(value[sub_birthplace == "australia"]) / sum(value))



# Proportion indigenous
d3 <- census_2016 |> 
  filter(sex == "persons",
         variable %in% c("total_persons", "indigenous")) |> 
  group_by(ced_code_2016) |> 
  summarise(indigenous = sum(value[sub_indig == "total indigenous"], na.rm = T) / 
             sum(value[variable == "total_persons"], na.rm = T))



# Adults with max schooling less than year 10
d5 <- census_2016 |> 
  filter(sex == "persons",
         variable == "high_school_equiv_year") |> 
  group_by(ced_code_2016) |> 
  summarise(adults_less_than_10_school = sum(value[school_equiv %in% c("year 9",
                                                                       "year 8 below",
                                                                       "did not graduate")]) / 
                                               sum(value))



# Only english spoken at home
d7 <- census_2016 |> 
  filter(sex == "persons",
         variable %in% c("only_english_spoken_home",
                           "total_persons")) |> 
  group_by(ced_code_2016) |> 
  summarise(only_english_spoken_home = sum(value[variable == "only_english_spoken_home"], na.rm = T) /
              sum(value[variable == "total_persons"], na.rm = T))




# G02 table has median rent and income data

raw_data <- read_csv("raw-data/2016 Census GCP Commonwealth Electoral Divisions for AUST/2016Census_G02_AUS_CED.csv")

census_2016 <- raw_data |> 
  clean_names() |> 
  pivot_longer(cols = !ced_code_2016,
               names_to = "variable_raw",
               values_to = "value") |> 
  left_join(metadata, by = c("ced_code_2016" = "census_code_2016")) 


# median rent

d4 <- census_2016 |> 
  filter(variable_raw == "median_rent_weekly") |> 
  select(ced_code_2016, value) |> 
  rename(median_rent_wkly = value)


# Median total personal income

d6 <- census_2016 |> 
  filter(variable_raw == "median_tot_prsnl_inc_weekly") |> 
  select(ced_code_2016, value) |> 
  rename(median_total_personal_income = value)






div_census_2016 <- d1 |> 
  left_join(d2, by = "ced_code_2016") |> 
  left_join(d3, by = "ced_code_2016") |> 
  left_join(d4, by = "ced_code_2016") |> 
  left_join(d5, by = "ced_code_2016") |> 
  left_join(d6, by = "ced_code_2016") |> 
  left_join(d7, by = "ced_code_2016") |>
  left_join(metadata, by = c("ced_code_2016" = "census_code_2016")) |> 
  select(-c(asgs_structure, area_sqkm, asgs_code_2016))




save(div_census_2016, file = "pkg/data/div_census_2016.rda")



url_2021 <- "https://www.abs.gov.au/census/find-census-data/datapacks/download/2021_GCP_CED_for_AUS_short-header.zip"