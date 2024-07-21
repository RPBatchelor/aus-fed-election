


source("setup.R")
source("prep/define-colours.R")

#=====================Building package====================

# Download and clean polling data from Wikipedia:
source("prep/collect_polls.R")

# The divisions and their margins for the 2019 election
source("prep/download-pendulum-2019.R")

# The divisions and their margins for the 2022 election
source("prep/download-pendulum-2022.R")


# More official sources
source("prep/download-2001-and-earlier.R")
source("prep/results-2pp-by-division.R")
source("prep/download-import-boundaries.R")
# source("prep/census-2016-summary-data.R")
source("prep/abs_census_datapack_prep_2016.R")
source("prep/abs_census_datapack_prep_2021.R")


document("pkg")
check("pkg")



#==============Modelling==================
source("model-2pp/model-2pp.R")

source("model-2pp/model-interpretation.R")
