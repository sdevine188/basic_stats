# # load get_wilson_conf_int()
# current_wd <- getwd()
# setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/basic_stats")
# source("get_wilson_conf_int.R")
# setwd(current_wd)

library(tidyverse)


# create get_wilson_conf_int function
get_wilson_conf_int <- function(n, p, z = 1.96) {

        conf_int_upper <- (1 / (2 * (n + z^2)) ) * ( (2 * n * p + z^2) + (z * sqrt(4 * n * p * (1 - p) + z^2) ) )
        conf_int_lower <- (1 / (2 * (n + z^2)) ) * ( (2 * n * p + z^2) - (z * sqrt(4 * n * p * (1 - p) + z^2) ) )
        return(tibble(conf_int_upper = conf_int_upper, conf_int_lower = conf_int_lower))
}


#####################################################################


# # test
# 
# # full example with mtcars with weights
# 
# # get diff in rate of cars which are manual in domain of 8-cyl cars, and the rate of cars with are manual overall (am = 1)
# head(mtcars)
# mtcars %>% group_by(cyl) %>% count()
# mtcars %>% group_by(cyl, am) %>% count()
# 
# mtcars_data <- mtcars %>% mutate(cyl8_dummy = ifelse(cyl == 8, 1, 0))
# head(mtcars_data)
# 
# # add weights and created weighted_obs
# mtcars_data <- mtcars_data %>% mutate(weights = case_when(gear == 4 ~ 1.22, gear == 3 ~ 1.04, gear == 5 ~ 1.11),
#                                       weighted_obs = am * weights)
# head(mtcars_data)
# 
# # get p_overall and p_domain as check
# p_overall <- mtcars_data %>% summarize(p_overall = sum(weighted_obs) / sum(weights))
# p_overall
# 
# p_domain <- mtcars_data %>% filter(cyl == 8) %>% summarize(p_weighted = sum(weighted_obs) / sum(weights))
# p_domain
# 
# # get wilson conf. int. for domain
# n1 <- mtcars_data %>% filter(cyl8_dummy == 1) %>% summarize(weight_sum = sum(weights)) %>% pull(weight_sum)
# n1
# x1 <- mtcars_data %>% filter(cyl8_dummy == 1, am == 1) %>% summarize(weight_sum = sum(weights)) %>% pull(weight_sum)
# x1
# p1 <- x1 / n1
# p1
# z <- 1.96
# 
# upper1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) + (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
# upper1
# 
# lower1 <- (1 / (2 * (n1 + z^2)) ) * ( (2 * n1 * p1 + z^2) - (z * sqrt(4 * n1 * p1 * (1 - p1) + z^2) ) )
# lower1
# 
# # get_wilson_conf_int
# get_wilson_conf_int(n = n1, p = p1)
