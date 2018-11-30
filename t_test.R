library(weights)
library(tidyverse)
library(broom)
library(janitor)
library(infer)
library(survey)

# http://www.sthda.com/english/wiki/two-proportions-z-test-in-r
# https://onlinecourses.science.psu.edu/stat100/node/57/

# calculate t.test for difference in proportions
# https://stattrek.com/estimation/difference-in-proportions.aspx


# for continuous values
# t.test w/ var.equal = TRUE matches OLS output (have not found manual formula for equal variance that will match yet)
# t.test w/ var.equal = FALSE matches manual method

# for proportions
# OLS matches manual which matches t.test w/ var.equal = TRUE

# still need to compare weighted OLS with wtd.t.test


# inspect
head(diamonds)
diamonds %>% count(cut)

diamonds %>% 
        filter(cut %in% c("Fair", "Ideal")) %>% 
        group_by(cut) %>% summarize(mean_price = mean(price))

diamonds %>% 
        filter(cut %in% c("Fair", "Ideal")) %>% 
        group_by(cut) %>% summarize(mean_price = mean(price)) %>% 
        spread(key = cut, value = mean_price) %>% summarize(avg_price_diff = Fair - Ideal)

# create two sample vectors
fair <- diamonds %>% filter(cut == "Fair") 
ideal <- diamonds %>% filter(cut == "Ideal")


#######################


# manually calcuate two-sample t-stat - "basic practice of stats" pg 476
# note: manually gets same avg diff as ols, but se, t_score, conf.int, and p-value are VERY slightly off
# manually gets same conf.int and everything as t.test though

# get average
x1 <- fair %>% summarize(avg_price = mean(price)) %>% pull(avg_price)
x1

x2 <- ideal %>% summarize(avg_price = mean(price)) %>% pull(avg_price)
x2

# get diff in avg price
x_diff <- x1 - x2
x_diff

# get n
n1 <- fair %>% nrow()
n1

n2 <- ideal %>% nrow()
n2

# get standard deviation
s1 <- fair %>% summarize(price_sd = sd(price)) %>% pull(price_sd)
s1

s2 <- ideal %>% summarize(price_sd = sd(price)) %>% pull(price_sd)
s2

# get se for two-sample t-test
# https://stattrek.com/hypothesis-test/difference-in-means.aspx
se <- sqrt( (s1^2 / n1) + (s2^2 / n2) )
se

# get t_score
t_score <- x_diff / se
t_score

# degrees of freedom
# https://stattrek.com/hypothesis-test/difference-in-means.aspx
df <- ( (s1^2/n1 + s2^2/n2)^2 ) / ( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
df

# get critical_t_value
critical_t_value <- qt(p = .975, df = df)
critical_t_value

# get conf.int
# http://onlinestatbook.com/2/estimation/difference_means.html
x_diff + (critical_t_value * se)
x_diff - (critical_t_value * se)



# get p_value
p_value <- 2 * pt(q = -abs(t_score), df = df)
p_value





##########################################################################################


# using t.test
fair_price <- fair %>% pull(price)
ideal_price <- ideal %>% pull(price)

# t.test with var.equal = FALSE matches the manual calculation
t.test(x = fair_price, y = ideal_price, alternative = "two.sided")
t.test(x = fair_price, y = ideal_price, alternative = "two.sided") %>% tidy()
t.test(x = fair_price, y = ideal_price, alternative = "two.sided") %>% tidy() %>% pull(p.value)

# if var.equal = TRUE, then t.test matches ols output, including standard error and p-value
t_test_output <- t.test(x = fair_price, y = ideal_price, alternative = "two.sided", var.equal = TRUE) %>% tidy()
t_test_output
t_test_se <- ((t_test_output$estimate1 - t_test_output$estimate2) - t_test_output$conf.low) / 1.96
t_test_se


#########################


# using ols
ols <- fair %>% bind_rows(., ideal) %>% 
        mutate(fair_dummy = ifelse(cut == "Fair", 1, 0)) %>%
        lm(formula = price ~ fair_dummy, data = .) 

ols %>% summary()
ols %>% tidy()

# get conf int 
ols_se <- ols %>% tidy() %>% filter(term == "fair_dummy") %>% pull(std.error)
ols_se

x_diff + (critical_t_value * ols_se)
x_diff - (critical_t_value * ols_se)


######################


# mimicing example in basic practice of statistics textbook
x1 <- 525.751
x2 <- 373.269
s1 <- 107.121
s2 <- 67.498
n1 <- 10
n2 <- 10

se <- sqrt( (s1^2 / n1) + (s2^2 / n2) )
se

x_diff <- x1 - x2 
x_diff / se

t_score <- (x1 - x2) / sqrt( (s1^2 / n1) + (s2^2 / n2) )
t_score

# note the example uses 90% conf int
# note there are two methods to calculate degrees of freedom (see text)
# simple non-software method is to take lower of n1 and n2
# note that software will use different calculation for degrees of freedom, which can be decimal
critical_t_value <- qt(p = .95, df = 9)
critical_t_value

x_diff + critical_t_value * se
x_diff - critical_t_value * se


###########################################################################################################


# weighted t.test for means

# create data
sample_1 <- c(1,1,1,1,1,1,2,2,2,3,3,3,4,4)
sample_1

sample_2 <- rev(sample_1) + 1
sample_2

# add alternate version of sample_1 to see how NA values are handled
# note NA values are just dropped
# sample_1 <- c(1,1,NA,1,1,1,2,2,2,NA,3,3,NA,4)

weight_1 <- c(.5,.5,.5,.5,.5,1,1,1,1,2,2,2,2,2)
weight_1_null <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)
weight_2_null <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)
weight_3_null <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)

data <- tibble(values = c(sample_1, sample_2), weight = c(weight_1, weight_2_null)) %>% 
        mutate(sample_origin = factor(c(rep("sample_1", times = length(sample_1)), rep("sample_2", times = length(sample_2))))) 
data


######################


# t_test with infer
data %>% t_test(formula = values ~ sample_origin, order = c("sample_1", "sample_2"))
data %>% group_by(sample_origin) %>% summarize(sample_mean = mean(values))

# t.test with base r
t.test(x = sample_1, y = sample_2, alternative = "two.sided") %>% tidy()

# unweighted wtd.t.test
# note that confindence interval is slightly off, but t_stat, p_value, diff in means, degrees freedom all match
summary <- wtd.t.test(x = sample_1, y = sample_2)
coefficients_tbl <- tibble(var_names = names(summary$coefficients), coefficients = summary$coefficients) %>% 
        spread(key = var_names, value = coefficients)
additional_tbl <- tibble(var_names = names(summary$additional), values = summary$additional) %>% 
        spread(key = var_names, value = values)

summary_tbl <- bind_cols(coefficients_tbl, additional_tbl)
summary_tbl
summary_tbl$Difference + 1.96 * summary_tbl$`Std. Err`


########################


# weighted t.test
# note that if weighty is unspecified, then weight arg is assumed to apply to both samples (seems an odd default)
# note that wtd.t.test with weights = 1 is same as unweighted t_tests
# mean1 = FALSE enforces assumption that the true number of obs in sample = sum of weights, instead of the weights being sampling weights
wtd.t.test(x = sample_1, y = sample_2, weight = weight_1_null, weighty = weight_2_null, mean1 = FALSE)
wtd.t.test(x = sample_1, y = sample_2, weight = weight_1_null, weighty = weight_2_null)

wtd.t.test(x = sample_1, y = sample_2, weight = weight_1, weighty = weight_2_null, mean1 = FALSE)
wtd.t.test(x = sample_1, y = sample_2, weight = weight_1, weighty = weight_2_null)


############################################################################################################
############################################################################################################


# another example of weighted t_test comparing wtd.t.test with svyttest (they give extremely close results)
# create two sample vectors
x <- diamonds %>% filter(cut == "Fair") %>% select(price)
y <- diamonds %>% filter(cut == "Ideal") %>% select(price)

# x_weights <- sample(x = 1, size = nrow(x), replace = TRUE)
x_weights <- sample(x = seq(from = 1, to = 2, by = .1), size = x %>% nrow(), replace = TRUE)

# y_weights <- sample(x = 1, size = nrow(y), replace = TRUE)
y_weights <- sample(x = seq(from = 1, to = 2, by = .1), size = y %>% nrow(), replace = TRUE)


x <- x %>% mutate(x_weights = x_weights, group = "x")
y <- y %>% mutate(y_weights = y_weights, group = "y")

glimpse(x)
glimpse(y)

# combine x and y
data <- tibble(group = c(x$group, y$group), values = c(x$price, y$price), weights = c(x$x_weights, y$y_weights))
glimpse(data)


####################


# create survey design
design <- svydesign(id = ~1, weights = ~weights, data = data)
design


####################


# run svyttest
svyttest(formula = values ~ group, design = design)


#####################


# compare to t.test
# save difference, extremely close conf int, t stat, and extremely close pvalue
fair_price <- fair %>% pull(price)
ideal_price <- ideal %>% pull(price)

t.test(x = fair_price, y = ideal_price, alternative = "two.sided") %>% tidy()
t.test(x = fair_price, y = ideal_price, alternative = "two.sided", var.equal = TRUE) %>% tidy()

wtd.t.test(x = fair_price, y = ideal_price)
wtd.t.test(x = x, y = y, weight = weights_x, weighty = weights_y, 
           mean1 = weights_are_statistical_not_sample_size, samedata = FALSE) 

ols <- fair %>% bind_rows(., ideal) %>% 
        mutate(fair_dummy = ifelse(cut == "Fair", 1, 0)) %>%
        lm(formula = price ~ fair_dummy, data = .) 

ols %>% summary()
ols %>% tidy()

# get conf int 
ols_se <- ols %>% tidy() %>% filter(term == "fair_dummy") %>% pull(std.error)
ols_se

x_diff + (critical_t_value * ols_se)
x_diff - (critical_t_value * ols_se)

#########################


# compare to wtd.t.test
# note that the mean1 argument must be TRUE (the default) to get same output as t.test and svyttest, 
# if mean1 = FALSE, it treats weights as though they are additional observations, giving much higher t_stat, but really they're just sampling weights

# note that wtd.t.test gives same info as svyttest and t.test for unweighted samples
# but for weighted samples, it does give slightly different t_stat and conf_int than svyttest, but probably negligible

# note that for wtd.t.test to match svyttest for weighted sample, must use diff_in_means +/- 2 * std_error to get conf int
# but for wtd.t.test to match t.test for unweighted sample, must use diff_in_means +/- 1.96 * std_error to get conf int
summary <- wtd.t.test(x = data %>% filter(group == "x") %>% pull(values), 
                      y = data %>% filter(group == "y") %>% pull(values), 
                      weight = data %>% filter(group == "x") %>% pull(weights), 
                      weighty = data %>% filter(group == "y") %>% pull(weights), samedata = FALSE)
summary

coefficients_tbl <- tibble(var_names = names(summary$coefficients), coefficients = summary$coefficients) %>% 
        spread(key = var_names, value = coefficients)
additional_tbl <- tibble(var_names = names(summary$additional), values = summary$additional) %>% 
        spread(key = var_names, value = values)

summary_tbl <- bind_cols(coefficients_tbl, additional_tbl) %>% rename(degrees_of_freedom = "df",
                                                                      p_value = "p.value", t_value = "t.value", diff_in_means = "Difference",
                                                                      mean_x = "Mean.x", mean_y = "Mean.y", std_error = "Std. Err")
summary_tbl <- summary_tbl %>% mutate(conf_int_lower = diff_in_means - (2 * std_error),
                                      conf_int_upper = diff_in_means + (2 * std_error))
# summary_tbl <- summary_tbl %>% mutate(conf_int_lower = diff_in_means - (1.96 * std_error),
#                                       conf_int_upper = diff_in_means + (1.96 * std_error))
summary_tbl


# only use this if sum(weights) represents actual sample size, and not just sum of statistical sampling weights added as part of survey design 
wtd.t.test(x = data %>% filter(group == "x") %>% pull(values), 
           y = data %>% filter(group == "y") %>% pull(values), 
           weight = data %>% filter(group == "x") %>% pull(weights), 
           weighty = data %>% filter(group == "y") %>% pull(weights), 
           mean1 = FALSE, samedata = FALSE) 



###################


##########################################################################################################
##########################################################################################################


# compare t.test with ols and wtd.t.test

# unweighted
# ols matches t.test with var.equal = TRUE
# wtd.t.test matches t.test with var.equal = FALSE

fair_price <- fair %>% pull(price)
fair_weights <- sample(x = seq(from = .5, to = 1, by = .1), size = length(fair_price), replace = TRUE)
ideal_price <- ideal %>% pull(price)
ideal_weights <- sample(x = seq(from = .5, to = 1, by = .1), size = length(ideal_price), replace = TRUE)

# t.test
t.test(x = fair_price, y = ideal_price, alternative = "two.sided") %>% tidy()
t.test(x = fair_price, y = ideal_price, alternative = "two.sided", var.equal = TRUE) %>% tidy()

# wtd.t.test
wtd.t.test(x = fair_price, y = ideal_price)

# ols
ols_unweighted <- fair %>% bind_rows(., ideal) %>% 
        mutate(fair_dummy = ifelse(cut == "Fair", 1, 0)) %>%
        lm(formula = price ~ fair_dummy, data = .) %>% tidy()
ols_unweighted

# get conf int 
ols_se <- ols_unweighted %>% filter(term == "fair_dummy") %>% pull(std.error)
ols_se

x_diff <- ols_unweighted %>% filter(term == "fair_dummy") %>% pull(estimate)

# note that 1.96 as critical_value matches t.test with var.equal = TRUE
x_diff + (1.96 * ols_se)
x_diff - (1.96 * ols_se)

# but 2 as critical_value is more conservative for small sample size
x_diff + (2 * ols_se)
x_diff - (2 * ols_se)


##################


# weighted
# as with unweighted, wtd.t.test has a baked in use of var.equal = FALSE, but the results are comparable

# wtd.t.test
wtd.t.test(x = fair_price, y = ideal_price, weight = fair_weights, weighty = ideal_weights)

# ols
ols_weighted <- fair %>% bind_rows(., ideal) %>% 
        mutate(fair_dummy = ifelse(cut == "Fair", 1, 0),
               weights = c(fair_weights, ideal_weights)) %>%
        lm(formula = price ~ fair_dummy, data = ., weights = weights) %>% tidy()
ols_weighted 

# get conf int 
ols_se <- ols_weighted %>% filter(term == "fair_dummy") %>% pull(std.error)
ols_se

x_diff <- ols_unweighted %>% filter(term == "fair_dummy") %>% pull(estimate)

# note that 1.96 as critical_value matches t.test with var.equal = TRUE
x_diff + (2 * ols_se)
x_diff - (2 * ols_se)



###########################################################################################################
###########################################################################################################
###########################################################################################################


# t.test for significant difference in proportion

diamonds %>% nrow()

high_price <- diamonds %>% filter(price >= mean(price)) %>% mutate(fair_dummy = ifelse(cut == "Fair", 1, 0))
high_price %>% nrow()

low_price <- diamonds %>% filter(price < mean(price)) %>% mutate(fair_dummy = ifelse(cut == "Fair", 1, 0))
low_price %>% nrow()

# inspect
high_price %>% tabyl(fair_dummy)
low_price %>% tabyl(fair_dummy)


##############################


# per "Basic Practice of Statistics" textbook pg 532
# also see http://www.r-tutor.com/elementary-statistics/hypothesis-testing/two-tailed-test-population-proportion

# this is the same values as ols and prop.test without yates continuity correction (see below)

# start by pooling samples to get overall "pooled sample proportion"
pooled_p <- diamonds %>% mutate(fair_dummy = ifelse(cut == "Fair", 1, 0)) %>% summarize(prop_fair = sum(fair_dummy) / n()) %>% pull(prop_fair)
pooled_p

# get n for each sample
n1 <- high_price %>% nrow()
n1

n2 <- low_price %>% nrow()
n2

# get p for each sample
p1 <- high_price %>% summarize(prop_fair = sum(fair_dummy) / n()) %>% pull(prop_fair)
p1

p2 <- low_price %>% summarize(prop_fair = sum(fair_dummy) / n()) %>% pull(prop_fair)
p2

# get p_diff
p_diff <- p1 - p2
p_diff

# then use pooled_p in the two-sample standard error formula instead of p1-p2,  
# this gives a t-score that has the standard normal distribution when null hypothesis is true
se <- sqrt( (pooled_p * (1 - pooled_p)) * ((1 / n1) + (1 / n2)) )
se

# get t-score
t_score <- p_diff / se
t_score

# get p-value for t-score
p_value <- 2 * pt(q = -abs(t_score), df = n1 + n2 - 2)
p_value

# get critical_t_value 
critical_t_value <- qt(p = .975, df = nrow(high_price) + nrow(low_price) - 2)
critical_t_value

# get conf.int
p_diff + (critical_t_value * se)
p_diff - (critical_t_value * se)


#############################################################################


# using ols
# get same t_score, p_value, conf int, and standard error as manual calculation
# get same p_value as prop.test without yates continuity correction (see below), 
# but different statistic (chi-squared i think), and very slightly different conf int (could just be rounding)
ols <- diamonds %>% mutate(high_price = ifelse(price > mean(price), 1, 0),
                             fair_dummy = ifelse(cut == "Fair", 1, 0)) %>%
        lm(formula = fair_dummy ~ high_price, data = .)

ols %>% summary()

# note that intercept is the proportion of diamonds that are fair when high_price is turned off
# and the coefficient on high_price shows the jump in fair proportion when high_price is turned on 
# the coefficent can also be interpreted as the difference in fair proportion 
# between the reference category of high_price = 0, and when it's turned on to high_price = 1
ols %>% tidy()

# get critical_t_value 
critical_t_value <- qt(p = .975, df = nrow(diamonds) - 2)
critical_t_value

# get conf int 
ols_se <- ols %>% tidy() %>% filter(term == "high_price") %>% pull(std.error)
ols_se

p_diff + (critical_t_value * ols_se)
p_diff - (critical_t_value * ols_se)

# calculate standard error manually using formula given at 
# https://stattrek.com/regression/slope-test.aspx
# https://www.statisticshowto.datasciencecentral.com/find-standard-error-regression-slope/
# sqrt [ Σ(yi - ŷi)2 / (n - 2) ] / sqrt [ Σ(xi - x)2 ]
diamond_data_for_ols <- diamonds %>% mutate(high_price = ifelse(price > mean(price), 1, 0),
                                            fair_dummy = ifelse(cut == "Fair", 1, 0))
diamond_data_for_ols_with_pred <- predict(object = ols, newdata = diamond_data_for_ols) %>% tibble(predicted_fair = .) %>% 
        bind_cols(diamond_data_for_ols, .)
diamond_data_for_ols_with_pred
diamond_data_for_ols_with_pred %>% tabyl(predicted_fair, high_price)

diamond_data_for_ols_with_pred %>% mutate(squared_fair_dummy_error = (fair_dummy - predicted_fair)^2,
                                          squared_high_price_actual_diff_from_mean = high_price - mean(high_price)) %>%
        summarize(se = sqrt( (sum(squared_fair_dummy_error) / 53938) / sqrt( sum(squared_high_price_actual_diff_from_mean) ) )) 

# high_price_mean <- diamonds %>% mutate(high_price = ifelse(price > mean(price), 1, 0),
#                     fair_dummy = ifelse(cut == "Fair", 1, 0)) %>% summarize(high_price_mean = mean(high_price)) %>% pull(high_price_mean)
# high_price_mean
# 
# fair_dummy_mean <- diamonds %>% mutate(high_price = ifelse(price > mean(price), 1, 0),
#                                        fair_dummy = ifelse(cut == "Fair", 1, 0)) %>% summarize(fair_dummy_mean = mean(fair_dummy)) %>% pull(fair_dummy_mean)
# fair_dummy_mean
# 
# diamonds %>% mutate(high_price = ifelse(price > mean(price), 1, 0),
#                     fair_dummy = ifelse(cut == "Fair", 1, 0), high_price_diff_from_mean = high_price)

#############################################################################


# using prop.test to test significant difference in proportions
# http://www.sthda.com/english/wiki/two-proportions-z-test-in-r

x1 <- high_price %>% filter(fair_dummy == 1) %>% nrow()
x1
n1 <- high_price %>% nrow()
n1

x2 <- low_price %>% filter(fair_dummy == 1) %>% nrow()
x2
n2 <- low_price %>% nrow()
n2

# without yates continuity correction  - note this is same p_value as ols/manual, but slightly different conf_int (probably because chi-sq. stat?)
# sthda.com says "yates correction is really important if either the expected successes or failures is < 5"
# although multiple sources say it's common advice to not use yates correction at all anymore because it overcorrects, despite base r prop.test default
# https://www.statisticshowto.datasciencecentral.com/what-is-the-yates-correction/
prop.test(x = c(x1, x2), n = c(n1, n2), alternative = "two.sided", correct = FALSE)
prop.test(x = c(x1, x2), n = c(n1, n2), alternative = "two.sided", correct = FALSE) %>% tidy()

# with yates continuity correction
prop.test(x = c(x1, x2), n = c(n1, n2), alternative = "two.sided")
prop.test(x = c(x1, x2), n = c(n1, n2), alternative = "two.sided") %>% tidy()

# comparison with t.test for means
# note that statistic used is completely different, p-value is slightly lower for prop.test, but conf int looks the same
t.test(x = high_price %>% pull(fair_dummy), y = low_price %>% pull(fair_dummy), alternative = "two.sided") %>% tidy()

# t.test with var.equal gets same p_value, stat, conf_int as ols and manual
t.test(x = high_price %>% pull(fair_dummy), y = low_price %>% pull(fair_dummy), alternative = "two.sided", var.equal = TRUE) %>% tidy()


#############################################################################################


# weighted prop.test
# probably improper weightings though, since the weighted n is artificially increasing sample size
high_price <- diamonds %>% filter(price >= mean(price)) %>% mutate(fair_dummy = ifelse(cut == "Fair", 1, 0))
low_price <- diamonds %>% filter(price < mean(price)) %>% mutate(fair_dummy = ifelse(cut == "Fair", 1, 0))

high_price <- high_price %>% mutate(weights = sample(x = seq(from = 1, to = 2, by = .1), size = nrow(.), replace = TRUE))
low_price <- low_price %>% mutate(weights = sample(x = seq(from = 1, to = 2, by = .1), size = nrow(.), replace = TRUE))

x1 <- high_price %>% filter(fair_dummy == 1) %>% summarize(x1 = sum(weights)) %>% pull(x1)
x1
n1 <- high_price %>% summarize(n1 = sum(weights)) %>% pull(n1)
n1

x2 <- low_price %>% filter(fair_dummy == 1) %>% summarize(x1 = sum(weights)) %>% pull(x1)
x2
n2 <- low_price %>% summarize(n1 = sum(weights)) %>% pull(n1)
n2

# without yates continuity correction (default) - note this is same as ols and manual
# sthda.com says "yates correction is really important if either the expected successes or failures is < 5"
# although multiple sources say it's common advice to not use yates correction at all anymore because it overcorrects, despite base r prop.test default
# https://www.statisticshowto.datasciencecentral.com/what-is-the-yates-correction/
prop.test(x = c(x1, x2), n = c(n1, n2), alternative = "two.sided", correct = FALSE)
prop.test(x = c(x1, x2), n = c(n1, n2), alternative = "two.sided", correct = FALSE) %>% tidy()
.0328 - .0279


######################


# compare to wtd.t.test
summary <- wtd.t.test(x = high_price %>% pull(fair_dummy), 
                      y = low_price %>% pull(fair_dummy), 
                      weight = high_price %>% pull(weights), 
                      weighty = low_price %>% pull(weights), 
                      samedata = FALSE) 

# create tidy summary_tbl with results
coefficients_tbl <- tibble(var_names = names(summary$coefficients), coefficients = summary$coefficients) %>% 
        spread(key = var_names, value = coefficients)
additional_tbl <- tibble(var_names = names(summary$additional), values = summary$additional) %>% 
        spread(key = var_names, value = values)
summary_tbl <- bind_cols(coefficients_tbl, additional_tbl) %>% rename(degrees_of_freedom = "df",
                                                                      p_value = "p.value", t_value = "t.value", diff_in_means = "Difference",
                                                                      mean_x = "Mean.x", mean_y = "Mean.y", std_error = "Std. Err")
summary_tbl <- summary_tbl %>% mutate(conf_int_lower = diff_in_means - (1.96 * std_error),
                                      conf_int_upper = diff_in_means + (1.96 * std_error))
summary_tbl


#################


# compare to svyttest
# http://r-survey.r-forge.r-project.org/survey/html/svyttest.html

# combine into single tbl
high_price <- high_price %>% mutate(group = "high_price")
low_price <- low_price %>% mutate(group = "low_price")
data <- bind_rows(high_price, low_price)
data
data %>% count(group)
data %>% group_by(group) %>% summarize(avg_fair_dummy = mean(fair_dummy))

# create survey design
design <- svydesign(id = ~1, weights = ~weights, data = data)
design

output <- svyttest(formula = fair_dummy ~ group, design = design)
output


###########################


# weighted prop.test by hand
# gets extremely close to wtd.t.test on p_value, t_value, and conf_int
# also gets very close to svyttest

# combine into single tbl
data <- bind_rows(high_price, low_price)
data
data %>% count(group)
data %>% group_by(group) %>% summarize(avg_fair_dummy = mean(fair_dummy))

# start by pooling samples to get overall "pooled sample proportion"
pooled_p <- data %>% mutate(fair_dummy_weighted = fair_dummy * weights) %>% 
        summarize(fair_dummy_weighted_mean = sum(fair_dummy_weighted) / sum(weights)) %>% pull(fair_dummy_weighted_mean)
pooled_p

# get n for each sample
n1 <- data %>% filter(group == "high_price") %>% nrow()
n1

n2 <- data %>% filter(group == "low_price") %>% nrow()
n2

# get p for each sample
p1 <- data %>% filter(group == "high_price") %>% mutate(fair_dummy_weighted = fair_dummy * weights) %>% 
        summarize(fair_dummy_weighted_mean = sum(fair_dummy_weighted) / sum(weights)) %>% pull(fair_dummy_weighted_mean)
p1

p2 <- data %>% filter(group == "low_price") %>% mutate(fair_dummy_weighted = fair_dummy * weights) %>% 
        summarize(fair_dummy_weighted_mean = sum(fair_dummy_weighted) / sum(weights)) %>% pull(fair_dummy_weighted_mean)
p2

# get p_diff
p_diff <- p1 - p2
p_diff

# then use pooled_p in the two-sample standard error formula instead of p1-p2,  
# this gives a t-score that has the standard normal distribution when null hypothesis is true
se <- sqrt( (pooled_p * (1 - pooled_p)) * ((1 / n1) + (1 / n2)) )
se

# get t-score
t_score <- p_diff / se
t_score

# get p-value for t-score
p_value <- 2 * pt(q = -abs(t_score), df = n1 + n2 - 2)
p_value

# get critical_t_value 
critical_t_value <- qt(p = .975, df = nrow(high_price) + nrow(low_price) - 2)
critical_t_value

# get conf.int
p_diff + (critical_t_value * se)
p_diff - (critical_t_value * se)


#############################################################################################


# mimicing example in "basic practice of stats" textbook

# without yates continuity correction (default) 
prop.test(x = c(91, 117), n = c(149, 236), alternative = "two.sided")

# with yates continuity correction - this matches textbook
prop.test(x = c(91, 117), n = c(149, 236), alternative = "two.sided", correct = FALSE) 


###########################################################################################
###########################################################################################
###########################################################################################


# manual example of welch t.test

# https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha

# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the same sizes
# m0: the null value for the difference in means to be tested for. Default is 0. 
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
t.test2 <- function(m1,m2,s1,s2,n1,n2,m0=0,equal.variance=FALSE)
{
        if( equal.variance==FALSE ) 
        {
                se <- sqrt( (s1^2/n1) + (s2^2/n2) )
                # welch-satterthwaite df
                df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
        } else
        {
                # pooled standard deviation, scaled by the sample sizes
                se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
                df <- n1+n2-2
        }      
        t <- (m1-m2-m0)/se 
        dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
        names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
        return(dat) 
}
x1 = rnorm(100)
x2 = rnorm(200) 
# you'll find this output agrees with that of t.test when you input x1,x2
t.test2( mean(x1), mean(x2), sd(x1), sd(x2), 100, 200)
t.test(x = x1, y = x2)


