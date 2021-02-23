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
# install.packages("recipes")
# library(recipes) # not covered here but you should check it out!
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
head(haul_catch)
str(haul_catch)


# tasks -------------------------------------------------------------------

# Be creative! 
# Make a lm(), glm(), and gam() using either of these datasets to answer a 
# question you have about the data. Be prepared to share your cool code with 
# the class! 

# 1. Set up data ----------------

# Here I am going to play with data from 2016-2018 for YFS. 
# I want to create models to solve for wtcpue. 
# Note that there are not a lot of years in this dataset and there 
# could be more, but... oh well, that will certainly mess with the 
# real potiential of this model. Good for a quick example. 

dat <- EBS %>%
  dplyr::filter(common == "yellowfin sole") %>% 
  dplyr::select(year, latitude, longitude, 
                wtcpue, bot_depth, bot_temp) %>% 
  mutate(year = as.integer(year)) %>%
  data.frame() 


# 2. Basic Data Exploration -----------

# What varibles are coorelated with each other?

# The fast way of what I showed you in lecture to find out what is correlated
corr_res <- map(dat %>% 
                  select(-wtcpue), 
                cor.test, y = dat$wtcpue)

corr_res %>% 
  map_dfr(tidy, .id = "predictor")
# Cool spatio-temporal variables are really important for predicting YFS wtcpue in this data. Good. To. Know. 

# Let's check this out visually:
library(cowplot)
# Let's just see what the data looks like:
plotlist_p = map(dat, ~dat %>% 
                   ggplot(aes(x = .x, y = wtcpue)) + 
                   geom_point())
plot_grid(plotlist = plotlist_p, labels = names(plotlist_p))

# Let's check for data normality
plotlist_d = map(dat, ~dat %>% # here I would remove wtcpue because we want to know the distribution
                   ggplot(aes(x = .x)) + 
                   geom_density())
plot_grid(plotlist = plotlist_d, labels = names(plotlist_d))

# 3. lm() models --------------- 

# Quickly, I am going to show you all of the combinations using the purrr::map() again:

lm_mods <- map(dat, ~lm(dat$wtcpue ~ .x, data = dat) %>% 
                 broom::tidy())

lm_mods 
# The best model looks to be the one with longitude!
  
# Another way of looking at this:

# p-values
dat %>% 
  map(~lm(dat$wtcpue ~ .x, data = dat)) %>% 
  map(summary) %>% 
  map(c("coefficients")) %>% 
  map_dbl(8)

# r2
dat %>% 
  map(~lm(dat$wtcpue ~ .x, data = dat)) %>% 
  map(summary) %>% 
  map(c("r.squared")) %>%
  unlist()

# 4. glm() models --------------- 

glm_fit1 <- glm(wtcpue ~ longitude, 
                family = gaussian(link = "identity"), # same as an lm()
                # family = "gaussian", # *same as line above
                data = dat)

glm_fit2 <- glm(wtcpue ~ longitude, 
                family = Gamma(), 
                data = dat)

glm_fit3 <- glm(wtcpue ~ longitude + latitude, 
                family = gaussian(link = "identity"), # same as an lm()
                # family = "gaussian", # *same as line above
                data = dat)

glm_fit4 <- glm(wtcpue ~ longitude + latitude, 
                family = Gamma(), 
                data = dat)

glm_fit5 <- glm(wtcpue ~ longitude + latitude + year, 
                family = gaussian(link = "identity"), # same as an lm()
                # family = "gaussian", # *same as line above
                data = dat)

glm_fit6 <- glm(wtcpue ~ longitude + latitude + year, 
                family = Gamma(), 
                data = dat)

glm_fit7 <- glm(wtcpue ~ longitude + latitude + year + bot_temp, 
                family = gaussian(link = "identity"), # same as an lm()
                # family = "gaussian", # *same as line above
                data = dat)

glm_fit8 <- glm(wtcpue ~ longitude + latitude + year + bot_temp, 
                family = Gamma(), 
                data = dat)

AIC(glm_fit1, glm_fit2, glm_fit3, glm_fit4, 
    glm_fit5, glm_fit6, glm_fit7, glm_fit8)
# Model 6 has the lowest AIC and is the most parsimonious!
# bot_temp did not improve the model at all here, so why include it?
# AIC is not the only metric to consider here, but I'll let you read up on that!
# we can see that this model has room for improvement from looking at the plots: 
plot(glm_fit6)

# Now let's predict our outputs

# make up x
pred<-data.frame("longitude" = rnorm(n = 30, 
                                     mean = mean(dat$longitude), 
                                     sd = sd(dat$longitude)), 
                 "latitude" = rnorm(n = 30, 
                                    mean = mean(dat$latitude), 
                                    sd = sd(dat$latitude)), 
                 "year" = rep_len(x = c(2016, 2017, 2018), 
                                  length.out = 10))
# predict y with your equation
pred$x<-predict(object = glm_fit6, 
                      newdata = pred, 
                      type = "response")
pred

# 5. gam() models --------------- 
library(mgcv)

# Create our gam models
gam_fit1 <- gam(
  wtcpue ~ s(longitude), 
  data = dat
)

gam_fit2 <- gam(
  wtcpue ~ s(longitude),
  family = Gamma(link=log), 
  data = dat
)

gam_fit3 <- gam(
  wtcpue ~ s(longitude) + s(latitude),
  data = dat
)

gam_fit4 <- gam(
  wtcpue ~ s(longitude) + s(latitude),
  family = Gamma(link=log), 
  data = dat
)

gam_fit5 <- gam(
  wtcpue ~ s(longitude) + s(latitude) + s(year, k = 2),
  data = dat
)

gam_fit6 <- gam(
  wtcpue ~ s(longitude) + s(latitude) + s(year, k = 2),
  family = Gamma(link=log), 
  data = dat
)


gam_fit7 <- gam(
  wtcpue ~ s(longitude, latitude, year),
  data = dat
)

gam_fit8 <- gam(
  wtcpue ~ s(longitude, latitude, year), 
  family = Gamma(link=log), 
  data = dat
)


AIC(gam_fit1, gam_fit2, gam_fit3, gam_fit4, 
    gam_fit5, gam_fit6, gam_fit7, gam_fit8)
# Model 8 has the lowest AIC! 
# by explicityly making a spatio-temporal term (as opposed to assessing 
# each sepeately) we were able to obtain a better model
# Again, AIC is not the only metric to consider here, but I'll let you read up on that!


# crazy, just for giggles (aka an abridged model I am playing with in real life!)
gam_fit9 <- gam(
  wtcpue ~ year + # a linear variable
    s(longitude, latitude, bs = c('ts'), k = 379) + # ts = tensor spline, k = knots, here the number of stations (?)
    s(longitude, latitude,bs=c('ts'),k=50, by=year, id=1), # the above but with a by year term
  family = Gamma(link=log), 
  data = dat
)

# Will this more developed model be better than our gam_fit8?
AIC(gam_fit8, gam_fit9)
# Our new gam_fit9 is just that much better than our gam_fit8! 

