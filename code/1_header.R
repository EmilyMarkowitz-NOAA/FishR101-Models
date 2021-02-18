# Lesson 8: Modling
# Created by: Emily Markowitz
# Contact: Emily.Markowitz@noaa.gov
# Created: 2020-12-18
# Modified: 2021-02-17

# packages ----------------------------------------------------------------

library(tidyverse)
# install.packages("broom")
library(broom)
# install.packages("modeldata")
library(modeldata) # This is also loaded by the tidymodels package
# install.packages("modelr")
library(modelr)
# install.packages("tidymv")
library(tidymv)
# install.packages("recipies")
# library(recipies) # not covered here but you should check it out!
# install.packages("DAAG")
library(DAAG) # for orings dataset


library(here)
library(janitor)

# directories --------------------------------------------------------------------

source(here("functions", "file_folders.R"))


# download data --------------------------------------------------------------------
# Anscombe's Quartet of ‘Identical’ Simple Linear Regressions
#?datasets::anscombe
sim1 <- datasets::anscombe

orings<-DAAG::orings

# look at your data -------------------------------------------------------


