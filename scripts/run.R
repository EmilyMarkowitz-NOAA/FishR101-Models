
#' ---
#' title: FishR101 - Jan-Feb 2021
#' purpose: run all of the scripts and R markdown scripts
#' author: emily.markowitz AT noaa.gov
#' start date: 2020-10
#' ---


######START#######

# Always start with a clean state by removing everything in your environment!

######***Libraries#########
library(here)

#######***LOAD PROJECT LIBRARIES AND FUNCTIONS#############
#Functions specific to this section
# source("./scripts/functions.R")

#######***LOAD PROJECT Data#############
#Data specific to this section
# source(paste0(dir.scripts, "/dataDownload.r"))
# source("./scripts/data.R")

######MAKE OUTPUTS########
rmarkdown::render(here("scripts","Lesson.Rmd"), 
                  output_dir = here("lesson"), 
                  output_file = "Lesson.pdf")

rmarkdown::render(here("scripts","Homework.Rmd"), 
                  output_dir = here("lesson"), 
                  output_file = "Homework.pdf")

rmarkdown::render(here("scripts","Homework-Answers.Rmd"), 
                  output_dir = here("lesson"), 
                  output_file = "Homework-Answers.pdf")

rmarkdown::render(here("scripts","presentation.Rpres"), 
                  output_dir = here("lesson"), 
                  output_file = "Presentation.html")
