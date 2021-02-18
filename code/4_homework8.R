# Lesson 8 Homework: Modling
# Created by: 
# Contact: 
# Created: 
# Modified: 

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
# data --------------------------------------------------------------------

# our favorite data set!
EBS <- read_csv(here::here("data", "ebs_2017-2018.csv"))
EBS <- janitor::clean_names(EBS)
head(EBS)
str(EBS)

haul_catch <- read_csv(here::here("data", "haul_catch.csv"))
haul_catch <- janitor::clean_names(haul_catch)
head(EBS)
str(EBS)


# tasks -------------------------------------------------------------------

# Be creative! 
# Make a lm(), glm(), and gam() using either of these datasets to answer a 
# question you have about the data. Be prepared to share your cool code with 
# the class! 

