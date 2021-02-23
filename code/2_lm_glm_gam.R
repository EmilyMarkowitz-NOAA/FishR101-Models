# Lesson 8: Modling
# Created by: Emily Markowitz
# Contact: Emily.Markowitz@noaa.gov
# Created: 2020-12-18
# Modified: 2021-02-17




# packages ----------------------------------------------------------------


# directories --------------------------------------------------------------------

# download data --------------------------------------------------------------------

# Check out models ------------------

# *** [General] Linear Models ---------------------------------------------- 
# ****** Truly linear -------------

# Example data
# Anscombe's Quartet of ‘Identical’ Simple Linear Regressions
#?datasets::anscombe
sim1 <- datasets::anscombe
sim1

# Let's check out y1 vs x1
ggplot(sim1, aes(x = x1, y = y1)) + 
  geom_point()
# Neat! Looks like these might be correlated...

# Let's also cover our assumptions and make sure that this is actually normal: 
ggplot(sim1, aes(x = x1)) + 
  geom_density()
# Look pretty dang bell shaped to me! Normal - yes!

cor.test(x = sim1$x1, y = sim1$y1)
# Woo! With a p-value = 0.00217, I would say so!

# Now let's see this in a linear model

formula <- y1 ~ x1
sim1_lm <- lm(formula, data = sim1)
sim1_lm <- lm(y1 ~ x1, data = sim1) # same thing, without using the formula object
sim1_lm

# This tells us about the equation, but we want to know more than that - we want to know about the correlation, what the data looks like,.. all that! We can do that with the summary() function:

summary(sim1_lm)
# Super! Not only did we get the same value from our corr_test(), but all of these other fun metrics (t-test, standard error, etc.)

# Do you also find this hard to read? Me too! The {broom} package has a nifty function called tidy() that can be a great way for cleaning this up to be a little less verbose

broom::tidy(sim1_lm)
# Phew, cool, this is soooo much easier to read! 


# Also alternatively, let's 'see' how well our model does with data
# plot() is a base R function for plotting, but it does a particularly nice automated thing with models:

plot(sim1_lm)
# We won't get into the details on what these plots mean, but they are important diagnostic plots. 
# For further inquiry, check out: https://data.library.virginia.edu/diagnostic-plots/

# So... what is the final equation after all of this work? For the purposes of what we are doing here, I am going to share with you a funciton widely shared on stackoverflow to add the equation to plots (with some small modifications)

# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

lm_eqn <- function(df){
  m <- lm(y1 ~ x1, df);
  eq <- substitute(italic(y1) == a + b %.% italic(x1)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}



p <- ggplot(sim1, aes(x = x1, y = y1)) +
  geom_point() +
  stat_smooth(method = "lm", 
              se = FALSE,
              formula = y ~ x) + # we use y ~ x because we redefined y1 and x1 in the ggplot() line
  geom_text(x = 9, y = 10, # the center point of where this text should go
            label = lm_eqn(sim1), parse = TRUE) +
  theme_bw()

p

# Now let's predict our outputs

pred<-data.frame("x1" = rnorm(n = 10, 
                        mean = mean(sim1$x1), 
                        sd = sd(sim1$x1)))
pred$y1<-predict(object = sim1_lm, 
        newdata = pred)

p + geom_point(data = pred, 
               mapping = aes(x = x1, y = y1), 
               color = "red")
# Essentially, by predicting it will just tell you where these values appear along the line of your linear model


# ****** Polynomial ---------------

ggplot(sim1, aes(x = x2, y = y2)) + 
  geom_point()
# Woah! A totally different curve. Perhaps a polynomial?! Looks familiar and correlated

# Let's also cover our assumptions and make sure that this is actually normal: 
ggplot(sim1, aes(x = x2)) + 
  geom_density()
# Yep! Look pretty dang bell shaped to me! Normal - yes!

cor.test(x = sim1$x2, y = sim1$y2)
# Woo! With a p-value = 0.002179, I would say so!

# Now let's see this in a linear model

sim1_lm_poly <- lm(y2 ~ x2, data = sim1) # same thing, without using the formula object
broom::tidy(sim1_lm_poly)
plot(sim1_lm_poly) # click through 4 times!
# These do not look good!


ggplot(sim1, aes(x = x2, y = y2)) +
  geom_point() +
  stat_smooth(method = "lm", 
              se = FALSE,
              formula = y ~ x) +
  theme_bw()
# And... that looks terrible. We can do better (but actually!)

# Using the poly()
sim1_lm_poly <- lm(y2 ~ poly(x2, 2), data = sim1) # same thing, without using the formula object
broom::tidy(sim1_lm_poly)
plot(sim1_lm_poly)  # click through 4 times!
# These look so much better!

ggplot(sim1, aes(x = x2, y = y2)) +
  geom_point() +
  stat_smooth(method = "lm", 
              se = FALSE,
              formula = y ~ poly(x,2)) +
  theme_bw()
# WOW! So much better! It fits each point perfectly, this must be the model!

# Another handy function from stackoverflow with some modifications... (don't worry about the details - as you get more versed, there are some smarter ways of doing this)
lm_poly2_eqn = function(df){
  m=lm(y2 ~ poly(x2, 2), df)#2nd degree polynomial
  eq <- substitute(italic(y2) == a + b %.% italic(x2)*","~~italic(r)^2~"="~r2,
                   list(a = format(coef(m)[1], digits = 2),
                        b = format(coef(m)[2], digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))
}



p <- ggplot(sim1, aes(x = x2, y = y2)) +
  geom_point() +
  stat_smooth(method = "lm", 
              se = FALSE,
              formula = y ~ poly(x,2)) + # we use y ~ x because we redefined y2 and x2 in the ggplot() line
  geom_text(x = 9, y = 9, # the center point of where this text should go
            label = lm_poly2_eqn(sim1), parse = TRUE) + # print formula on plot
  theme_bw()

p

# Now let's predict our outputs

pred<-data.frame("x2" = rnorm(n = 10, 
                              mean = mean(sim1$x1), 
                              sd = sd(sim1$x1)))
pred$y2<-predict(object = sim1_lm_poly, 
                 newdata = pred)

p + geom_point(data = pred, 
               mapping = aes(x = x2, y = y2), 
               color = "red")

# Nice!

# Essentially, by predicting it will just tell you where these values appear along the line of your linear model

# *** Genearlized Linear Models ---------------------------------------------- 

# Using the same Example data
# Anscombe's Quartet of ‘Identical’ Simple Linear Regressions
#?datasets::anscombe
# sim1 <- datasets::anscombe

# ****** "gaussian" = fancy lm() -------------

# Quick Note:
# glm's are the next step up from lm's. For example, we can get the same exact answer even in a glm by using the 'gaussian' family (aka normally distributed)


lm(y1 ~ x1, data = sim1)
glm(y1 ~ x1, family = "gaussian", data = sim1) # just adds a few more variables in the output

broom::tidy(lm(y1 ~ x1, data = sim1))
broom::tidy(glm(y1 ~ x1, family = "gaussian", data = sim1))
# Viola! The same! This would work for the polynomial as well. 

# ****** Non-Linear data ---------------

# So now let's check out some non-linear data
# There is a good example in the orings dataset
str(orings) # so you can get a sense of the data
# The orings dataset consists of the number of damaged o-rings on launches of the Challenger space shuttle. On each launch there were six rings. Following each launch the rings were inspected.

# We can create a glm of events (oring failures) from a number of trials for a given temperature.

# Let's check out Erosion vs Temperature
ggplot(data = orings, 
       mapping = aes(x = Erosion, y = Temperature)) + 
  geom_point()
# Woah! A totally different... curve? Looks familiar and correlated, but certainly not linear. 


# First: a density plot of damage to o-rings - is it normal? 
ggplot(data = orings, mapping = aes(x = Erosion )) + 
  geom_density()  
# That would be a solid NO - glad we are learning about glms! Let's take a deeper dive why it looks like that. 

orings$Erosion
# Oooh its categorical numbers from 0 and 6! binomial data! Makes sense!

# Just so we get an idea of what we are dealing with, how many of each are there in the data?
orings %>%
  group_by(Erosion) %>%
  count()

# Let's build our glm
# each record contains the result of 6 trials
oring_glm_damg_temp <- glm(cbind(Erosion, 6 - Erosion) ~ Temperature, # formula
                           data = orings, # data
                           family = "binomial") # family

broom::tidy(oring_glm_damg_temp) 
plot(oring_glm_damg_temp)  # click through 4 times!

# and see how well this model runs!
# I am fine with these p values and r^2 in our overly simplified example. Next!

# So... what is the final equation after all of this work? For the purposes of what we are doing here, I am going to share with you a funciton widely shared on stackoverflow to add the equation to plots (with some small modifications)

# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

glm_eqn <- function(df){
  m <- glm(cbind(Erosion, 6 - Erosion) ~ Temperature, family = "binomial", data = df);
  eq <- substitute(italic(Temperature) == a + b %.% italic(Erosion), 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2)))
  as.character(as.expression(eq));
}



p <- ggplot(data = orings, 
            mapping = aes(x = Erosion, y = Temperature)) + 
  geom_point() +
  stat_smooth(method = "glm", 
              formula = y ~ x) + # we use y ~ x because we redefined y1 and x1 in the ggplot() line
  geom_text(x = 1.5, y = 80, # the center point of where this text should go
            label = glm_eqn(orings), parse = TRUE) +
  theme_bw()

p

# Now let's predict our outputs

# make up x
pred<-data.frame("Temperature" = seq(from = min(orings$Temperature), 
                                     to = max(orings$Temperature), 
                                     by = 1))
# predict y
pred$Erosion<-predict(object = oring_glm_damg_temp, 
                 newdata = pred, 
                 type = "response")
pred
p + geom_point(data = pred, 
               mapping = aes(y = Temperature, x = Erosion), 
               color = "red")
# Not a perfect match, but not a bad start! Further analysis could make this match better. This model is heavily weighted by where the most points are (at 0). 


# To see the full example (which was heavily modified for this example), check out https://www.r-bloggers.com/2014/10/an-intro-to-models-and-generalized-linear-models-in-r/)


# *** Generalized Additive Models (GAMs) --------------

# For GAMs, we use the mgcv package. This is not built into base R the same way that lm() and glm() are. 
library(mgcv)

# And for these examples, we will use the mpg dataset

# ****** 1: Simplest model with 1 continuous variable -------

# data
str(mpg)

# Pick our formula
fm <- hwy ~ s(displ)
# Here, we've only applied simple smoothers to the values x1, x2, and x3
# If you check out ?s, you will see that there are a million and a half ways to customize within the smoother

# Create our gam model
mpg_gam1 <- gam(
  fm, 
  data = mpg
)
# Run a simple model with the default (gaussian/normal) distribution

summary(mpg_gam1)

broom::tidy(mpg_gam1)

plot(mpg_gam1,pages=1,residuals=TRUE)  ## show partial residuals
plot(mpg_gam1,pages=1,seWithMean=TRUE) ## `with intercept' CIs
## run some basic model checks, including checking
## smoothing basis dimensions...
gam.check(mpg_gam1)

library(tidymv)
mpg_gam1_pred <- predict_gam(mpg_gam1)


mpg_gam1_pred %>%
  ggplot(aes(displ, fit)) +
  geom_smooth() +
  theme_bw()


# ****** 2: 2 continous variables --------------

# Data
str(mpg)

# To add a variable, simply "+" it in the equation and add the new variable in the s()
fm <- hwy ~ s(displ) + s(cty)

mpg_gam2 <- gam(
  formula = fm, 
  data = mpg
)

summary(mpg_gam2)
broom::tidy(mpg_gam2)

plot(mpg_gam2,pages=1,residuals=TRUE)  ## show partial residuals
plot(mpg_gam2,pages=1,seWithMean=TRUE) ## `with intercept' CIs
## run some basic model checks, including checking
## smoothing basis dimensions...
gam.check(mpg_gam2)

library(tidymv)
mpg_gam2_pred <- predict_gam(mpg_gam2)

mpg_gam2_pred %>%
  ggplot() +
  geom_smooth(aes(displ, fit)) + 
  geom_smooth(aes(cty, fit)) + 
  # geom_text(x = 1.5, y = 80, # the center point of where this text should go
  #           label = gam_eqn(orings), parse = TRUE) +
  theme_bw()

# ****** 3: Have a linear term (without s() smoother) --------

# data
str(mpg)

# To add a linear variable, simply "+" it in the equation and don't add the variable in the s() smoother
fm <- hwy ~ s(displ) + cty
# However we rarely make continuous variables linear in gams because if a realtionship is really linear, a snoother will force the shape

mpg_gam3 <- gam(
  formula = fm,
  data = mpg
)

summary(mpg_gam3)

broom::tidy(mpg_gam3)

plot(mpg_gam3,pages=1,residuals=TRUE)  ## show partial residuals
plot(mpg_gam3,pages=1,seWithMean=TRUE) ## `with intercept' CIs
## run some basic model checks, including checking
## smoothing basis dimensions...
gam.check(mpg_gam3)


library(tidymv)
mpg_gam3_pred <- predict_gam(mpg_gam3)


# ****** 4: Adding categorical variables --------

# data
str(mpg)
mpg$fl <- mpg$fl %>% as.factor
# linear variables can be useful for categorical data. simply "+" it in the equation and don't add the variable in the s() smoother
fm <- hwy ~ s(displ) + s(cty) + fl # fuel type
# Here, the gam model creates a seperate model for each fixed effect in the categorical data
mpg_gam4 <- gam(
  formula = fm,
  data = mpg
)
summary(mpg_gam4)
broom::tidy(mpg_gam4)

# Alternatively (though slightly different), we can add a varying intercept that will replicate of the smooth is produced for each factor level
fm <- hwy ~ s(displ) + s(cty, by  = fl) + fl # fuel type
mpg_gam4 <- gam(
  formula = fm,
  data = mpg
)
summary(mpg_gam4)
broom::tidy(mpg_gam4)

plot(mpg_gam4,pages=1,residuals=TRUE)  ## show partial residuals
plot(mpg_gam4,pages=1,seWithMean=TRUE) ## `with intercept' CIs
## run some basic model checks, including checking
## smoothing basis dimensions...
gam.check(mpg_gam4)

library(tidymv)
mpg_gam4_pred <- predict_gam(mpg_gam4)

# ****** 5: Setting a family type --------

# data
str(mpg)

# We're going to go back to our formula from the first gam
fm <- hwy ~ s(displ)

# up to now, we've just been playing with gaussian family GAM models. In a quick example, I'll add a family = Gamma(link = log) in so you can see it in action. 

mpg_gam5 <- gam(
  formula = fm,
  family = Gamma(link=log),
  data = mpg
)
summary(mpg_gam5)
broom::tidy(mpg_gam5)

plot(mpg_gam5,pages=1,residuals=TRUE)  ## show partial residuals
plot(mpg_gam5,pages=1,seWithMean=TRUE) ## `with intercept' CIs
## run some basic model checks, including checking
## smoothing basis dimensions...
gam.check(mpg_gam5)

library(tidymv)
mpg_gam5_pred <- predict_gam(mpg_gam5)




# ****** Compare Models ------------
# Which model is WORST? The one with the higher AIC!
# To compare models using AIC, you need to calculate the AIC of each model. If a model is more than 2 AIC units lower than another, then it is considered significantly better than that model.
AIC(mpg_gam1, mpg_gam2, mpg_gam3, mpg_gam4, mpg_gam5)
# ...and we have a LOSER! Congrats model 1, sometimes simple is good and it is really easy to try really complicated things that are not helpful. Even though adding in the gamma distribution yielded similar model results, it was not better than (or even equal to) the first simple model. 


