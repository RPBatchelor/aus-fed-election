


library(tidyverse)
library(janitor)
library(scales)
library(rstan)
library(rvest)
library(pscl)
library(scales)
library(extrafont)
library(svglite)
library(devtools)
library(knitr)
library(snakecase)
library(testthat)
library(RColorBrewer)
library(clipr)
library(plotly)

library(frs)
library(ozfedelect)


source("R/run_all_r_scripts.R")
run_all_r_scripts("R", cleanup = FALSE)


myfont <- "Aptos"
main_font <- "Aptos" # Roboto
heading_font <- "Sarala"


theme_set(theme_light(base_family = main_font) + 
            theme(legend.position = "bottom") +
            theme(plot.caption = element_text(colour = "grey50"),
                  strip.text = element_text(size = rel(1), face = "bold"),
                  plot.title = element_text(family = heading_font),
                  plot.subtitle = element_text(family = heading_font, colour = "grey50"))
) 

update_geom_defaults("text", list(family = main_font))


election_dates_and_results <- tibble(
  election = c("2007 Election",
               "2010 Election",
               "2013 Election",
               "2016 Election",
               "2019 Election",
               "2022 Election"),
  election_date = c(as.Date("2007-11-24"),
                    as.Date("2010-08-21"),
                    as.Date("2013-09-07"),
                    as.Date("2016-07-02"),
                    as.Date("2019-05-18"),
                    as.Date("2022-05-21")),
  election_winner = c("ALP",
                      "ALP",
                      "Lib/Nat",
                      "Lib/Nat",
                      "Lib/Nat",
                      "ALP")
)




