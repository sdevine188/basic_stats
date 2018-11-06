library(tidyverse)
library(tabyl)

# http://www.sthda.com/english/wiki/two-proportions-z-test-in-r
# https://onlinecourses.science.psu.edu/stat100/node/57/

# calculate t.test for difference in proportions
# https://stattrek.com/estimation/difference-in-proportions.aspx

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
se <- sqrt( (s1^2 / n1) + (s2^2 / n2) )
se

# get t_score
t_score <- x_diff / se
t_score

# get critical_t_value
critical_t_value <- qt(p = .975, df = min(n1, n2))
critical_t_value

# get conf.int
x_diff + (critical_t_value * se)
x_diff - (critical_t_value * se)

# get p_value
p_value <- 2 * pt(q = -abs(t_score), df = n1 + n2 - 2)
p_value


######################


# mimicing example in textbook
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


##########################################################################################


# using t.test
# note that degrees of freedom using software is decimal, not simply lesser of n1 and n2
fair_price <- fair %>% pull(price)
ideal_price <- ideal %>% pull(price)

t.test(x = fair_price, y = ideal_price, alternative = "two.sided")
t.test(x = fair_price, y = ideal_price, alternative = "two.sided") %>% tidy()


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
p_diff - (critical_t_value * ols_se)


#############################################################################


# using ols
# get same as prop.test without yates continuity correction (see below)
ols <- diamonds %>% mutate(high_price = ifelse(price > mean(price), 1, 0),
                             fair_dummy = ifelse(cut == "Fair", 1, 0)) %>%
        lm(formula = fair_dummy ~ high_price, data = .)

ols %>% summary()

ols %>% tidy()

# get critical_t_value 
critical_t_value <- qt(p = .975, df = nrow(diamonds) - 2)
critical_t_value

# get conf int 
ols_se <- ols %>% tidy() %>% filter(term == "high_price") %>% pull(std.error)
ols_se

p_diff + (critical_t_value * ols_se)
p_diff - (critical_t_value * ols_se)


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

# without yates continuity correction (default) - note this is same as ols and manual
# sthda.com says "yates correction is really important if either the expected successes or failures is < 5"
prop.test(x = c(x1, x2), n = c(n1, n2), alternative = "two.sided", correct = FALSE)
prop.test(x = c(x1, x2), n = c(n1, n2), alternative = "two.sided", correct = FALSE) %>% tidy()

# with yates continuity correction
prop.test(x = c(x1, x2), n = c(n1, n2), alternative = "two.sided")
prop.test(x = c(x1, x2), n = c(n1, n2), alternative = "two.sided") %>% tidy()


###############


# mimicing example in "basic practice of stats" textbook

# without yates continuity correction (default) 
prop.test(x = c(91, 117), n = c(149, 236), alternative = "two.sided")

# with yates continuity correction - this matches textbook
prop.test(x = c(91, 117), n = c(149, 236), alternative = "two.sided", correct = FALSE) 





