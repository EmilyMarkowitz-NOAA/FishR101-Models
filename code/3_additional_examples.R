# Lesson 8: Modling
# Created by: Emily Markowitz
# Contact: Emily.Markowitz@noaa.gov
# Created: 2020-12-18
# Modified: 2021-02-17

# packages ----------------------------------------------------------------


# directories --------------------------------------------------------------------

# download data --------------------------------------------------------------------

# Example 1: Crickets (borrowed from tmrw.org)------------------------

# To demonstrate these fundamentals, let’s use experimental data from McDonald (2009), by way of Mangiafico (2015), on the relationship between the ambient temperature and the rate of cricket chirps per minute. Data were collected for two species: O. exclamationis and O. niveus. The data are contained in a data frame called crickets with a total of 31 data points. These data are shown here in a ggplot2 graph.


# What does the data look like?

data(crickets, package = "modeldata")
names(crickets)
#> [1] "species" "temp"    "rate"

# Plot the temperature on the x-axis, the chirp rate on the y-axis. The plot
# elements will be colored differently for each species:
ggplot(crickets, aes(x = temp, y = rate, col = species)) + 
  # Plot points for each data point and color by species
  geom_point() + 
  # Show a simple linear model fit created separately for each species:
  geom_smooth(method = lm, se = FALSE) + 
  labs(x = "Temperature (C)", y = "Chirp Rate (per minute)")

# To fit an ordinary linear model in R, the lm() function is commonly used. 
# The important arguments to this function are a model formula and a data frame 
# that contains the data. The formula is symbolic.

# There are a couple of combinations of this that we could try...

# rate ~ temp
# rate ~ temp + time
# rate ~ temp + species


# This formula would symbolically represent that temperature and time or species should be added as separate main effects to the model (NOT that they are 'added' together). A main effect is a model term that contains a single predictor variable.

# Oh wait! What do we notice? Species is NOT a numeric! To accommodate this structure, an interaction term can be added to the model. This can be specified in a few different ways, and the most basic uses the colon:

# rate ~ temp + species + temp:species

# A shortcut can be used to expand all interactions containing
# interactions with two variables:
rate ~ (temp + species)^2

# Another shortcut to expand factors to include all possible
# interactions (equivalent for this example):
rate ~ (temp * species)^2

interaction_fit <-  lm(rate ~ (temp + species)^2, 
                       data = crickets) 
interaction_fit


# Place two plots next to one another:
par(mfrow = c(1, 2))

# Show residuals vs predicted values:
plot(interaction_fit, which = 1)

# A normal quantile plot on the residuals:
plot(interaction_fit, which = 2)

# Our next order of business with the crickets is to assess if the inclusion of the interaction term is necessary. The most appropriate approach for this model is to re-compute the model without the interaction term and use the anova() method.

main_effect_fit <-  lm(rate ~ temp + species, data = crickets) 

# Compare the two:
anova(main_effect_fit, interaction_fit)

# a p-value of 0.25 implies that there is a lack of evidence that the interaction term is needed by the model (some might call this the alt. hyp.). 

# For this reason, we will conduct further analysis on the model without the interaction.

summary(main_effect_fit)

# How to read this: 
# The chirp rate for each species increases by 3.6 chirps as the temperature increases by a single degree. This term shows strong statistical significance as evidenced by the p-value. The species term has a value of -10.07. This indicates that, across all temperature values, O. niveus has a chirp rate that is about 10 fewer chirps per minute that O. exclamationis. Similar to the temperature term, the species effect is associated with a very small p-value.

# Upon further analysis, you'll find that the intercept value is a bit wonky(at 0 C there are negative chirps per minute for both species), but for the sake of the example, we'll call this a win.  

# If we needed to estimate the chirp rate at a temperature that was not observed in the experiment, we could use the predict() method. It takes the model object and a data frame of new values for prediction. For example, the model estimates the chirp rate for O. exclamationis for temperatures between 15 C and 20 C can be computed via:

new_values <- data.frame(species = "O. exclamationis", temp = 15:20)

predict(main_effect_fit, new_values)  

# Example 2: How to find correlated data -------

# Let's see what variables are the most correleated to each other

names(mpg)

# to systematically find the correlation between variables (columns of the mtcars dataset, I am going to use a for loop)

# A tidyr way of doing what I am about to do uses the {purrr} package, but I didn't introduce that in the loops lecture so it's just bonus material here!
# corr_res <- map(mtcars %>% select(-mpg), cor.test, y = mtcars$mpg)

# Alternatively, recall, without this loop we would need to write out each correlation... [GROSS]
# corr.cyl <-  cor.test(x = mtcars$cyl, 
#                   y = mtcars$mpg)
# corr.disp <-  cor.test(x = mtcars$disp, 
#                       y = mtcars$mpg)
# ...
# corr.carb <-  cor.test(x = mtcars$carb, 
#                       y = mtcars$mpg)

# the first column is mpg (our y), so we don't want to assess that against itself - anyway we already know that mpg vs mpg would be a perfect correlation!

corr_res <- list() # lists are scary but work with me because we need it for the next step
for (i in 2:ncol(mtcars)) {  
  corr_res <-  c(corr_res, 
                 list(cor.test(x = mtcars[,i], 
                                 y = mtcars$mpg) ) )
  names(corr_res)[length(corr_res)]<-paste0("$", names(mtcars)[i]) # the dollar sign ($) is also expected by the next step. Here, it is just letting us know that this data came from the ith column
}

corr_res # our outputs
# ok, a little hard to read, so how can we make this easier?

broom::tidy(corr_res[[1]])
broom::tidy(corr_res$`$cyl`) # same thing
# cool, but this shows only one test result. It would be nice to see all of these next to eachother, right? Sounds like a great time to use another loop!

# The tidyr way to do the following we did not cover in class:
# corr_res %>% # Convert each to a tidy format; `map_dfr()` stacks the data frames 
# map_dfr(tidy, .id = "predictor")

# Likely a cleaner/tidyr way to do this, but oh well, this works!
corr_res_broom<-c() # create an empty object to put content in
for (i in 1:length(corr_res)) {
  corr_res_broom <- rbind.data.frame(corr_res_broom,
                                     broom::tidy(corr_res[[i]])) # comes in tibble
}
corr_res_broom <- 
  corr_res_broom %>% 
  add_column("predictor" = names(corr_res)) # add the predictor column for the next step and for good record keeping

# notice how the tibble so nicely puts negative numbers in a different color (here red) than possitive numbers. So nice and easy to see!

# Sounds like a great opprotunity to plot and 'see' what this really looks like!

corr_res_broom %>% 
  ggplot(aes(x = fct_reorder(predictor, estimate))) + 
  geom_point(aes(y = estimate)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1) +
  labs(x = NULL, y = "Correlation with mpg")

# Who is the most correlated with mpg? Least?

# Now we can use the same methods we used before in script 2 to assess the data with the new found information. 

