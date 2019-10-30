# load test_means()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("test_means.R")
# setwd(current_wd)

# need to detach dplyr, since Hmisc summarize function masks it if weights package is loaded after dplyr
# after detaching dplyr, reload tidyverse or dplyr so that instead dplyr's summarize masks Hmisc
detach_package <- function(pkg, character.only = FALSE)
{
        if(!character.only)
        {
                pkg <- deparse(substitute(pkg))
        }
        search_item <- paste("package", pkg, sep = ":")
        while(search_item %in% search())
        {
                detach(search_item, unload = TRUE, character.only = TRUE)
        }
}
detach("package:dplyr", unload=TRUE)


######################################################################


library(weights) # note that weights needs to be loaded before tidyverse, or else summarize() in Hmisc package masks tidyverse summarize()
library(tidyverse)
library(broom)
library(dplyr)

# note that unweighted t.test with var.equal = TRUE will match ols dummy t.test regression

# create test_means function to use wtd.t.test (numeric) or prop.test (proportion) to test difference in means
test_means <- function(x, y, weights_x = NULL, weights_y = NULL, 
                       weights_are_statistical_not_sample_size = TRUE, yates_correction = FALSE, var.equal = FALSE) {
        
        # check weights_x
        if(is.null(weights_x)) {
                weights_x <- rep(1, times = length(x))
        }
        
        if(!is.null(weights_x) & length(x) != length(weights_x)) {
               stop("weights_x needs to be a vector the same length as x")
        }
        
        # check weights_y
        if(is.null(weights_y)) {
                weights_y <- rep(1, times = length(y))
        }
        
        if(!is.null(weights_y) & length(y) != length(weights_y)) {
                stop("weights_y needs to be a vector the same length as y")
        }
        
        
        #########################################################################################
        
        
        # multiply x and y by 1, so that a dummy variable with TRUE/FALSE is converted to 0/1
        x <- x * 1
        y <- y * 1
        
        
        ##########################################################################################
        
        
        # if neither x or y has weights use t.test
        # note that unweighted t.test with var.equal = TRUE will match ols dummy t.test regression
        if(length(unique(weights_x)) == 1 & length(unique(weights_y)) == 1) {
                
                # get test_type
                test_type <- "t.test"
                
                # get flag for whether weights were used
                weights_x_used <- ifelse(length(unique(weights_x)) == 1, FALSE, TRUE)
                weights_y_used <- ifelse(length(unique(weights_y)) == 1, FALSE, TRUE)
                
                # call wtd.t.test
                summary <- t.test(x = x, y = y, alternative = "two.sided", var.equal = var.equal) %>% tidy()
                
                # if var.equal = TRUE, t.test output doesn't provide diff_in_means (called "estimate"), so create it manually
                if(var.equal == TRUE) {
                        summary <- summary %>% mutate(estimate = estimate1 - estimate2)
                }
                
                # create tidy summary_tbl with results
                summary_tbl <- summary %>% rename(p_value = "p.value", t_stat = "statistic", diff_in_means = "estimate", mean_x = "estimate1",
                                    mean_y = "estimate2", conf_int_upper = "conf.high", conf_int_lower = "conf.low") %>% 
                        mutate(std_error = (conf_int_upper - diff_in_means) / 2, test_type = test_type, 
                               weights_x_used = weights_x_used, weights_y_used = weights_y_used,
                               x_unweighted_count = length(x[!is.na(x)]), x_na_count = sum(is.na(x)),
                               y_unweighted_count = length(y[!is.na(y)]), y_na_count = sum(is.na(y)),
                               equal_variance = var.equal) %>%
                        select(x_unweighted_count, x_na_count, y_unweighted_count, y_na_count, 
                               mean_x, mean_y, diff_in_means, weights_x_used, weights_y_used, 
                               test_type, t_stat, p_value, std_error, equal_variance, conf_int_lower, conf_int_upper)
                return(summary_tbl)
        }
        
        
        ##########################################################################################
        
        
        # if x or y has weights other than 1, use wtd.t.test
        if(length(unique(weights_x)) != 1 | length(unique(weights_y)) != 1) {
                
                # get test_type
                test_type <- "wtd.t.test"
                
                # get flag for whether weights were used
                weights_x_used <- ifelse(length(unique(weights_x)) == 1, FALSE, TRUE)
                weights_y_used <- ifelse(length(unique(weights_y)) == 1, FALSE, TRUE)
                
                # call wtd.t.test
                summary <- wtd.t.test(x = x, y = y, weight = weights_x, weighty = weights_y, 
                                      mean1 = weights_are_statistical_not_sample_size, samedata = FALSE) 
                
                # create tidy summary_tbl with results
                coefficients_tbl <- tibble(var_names = names(summary$coefficients), coefficients = summary$coefficients) %>% 
                        spread(key = var_names, value = coefficients)
                additional_tbl <- tibble(var_names = names(summary$additional), values = summary$additional) %>% 
                        spread(key = var_names, value = values)
                summary_tbl <- bind_cols(coefficients_tbl, additional_tbl) %>% rename(p_value = "p.value", t_stat = "t.value", diff_in_means = "Difference",
                                                                                      mean_x = "Mean.x", mean_y = "Mean.y", std_error = "Std. Err") 
                
                # get conf_int
                summary_tbl <- summary_tbl %>% mutate(conf_int_lower = diff_in_means - (2 * std_error),
                                                      conf_int_upper = diff_in_means + (2 * std_error),
                                                      test_type = test_type, weights_x_used = weights_x_used, weights_y_used = weights_y_used,
                                                      x_unweighted_count = length(x[!is.na(x)]), x_na_count = sum(is.na(x)),
                                                      y_unweighted_count = length(y[!is.na(y)]), y_na_count = sum(is.na(y)),
                                                      equal_variance = FALSE) %>%
                        select(x_unweighted_count, x_na_count, y_unweighted_count, y_na_count, 
                               mean_x, mean_y, diff_in_means, weights_x_used, weights_y_used, 
                               test_type, t_stat, p_value, std_error, equal_variance, conf_int_lower, conf_int_upper)
                return(summary_tbl)
                }
        
}


################


# # test numeric values
# x <- diamonds %>% filter(cut == "Fair") %>% pull(price)
# x
# y <- diamonds %>% filter(cut == "Ideal") %>% pull(price)
# y
# weights_x <- sample(x = seq(from = 1, to = 2, by = .1), size = length(x), replace = TRUE)
# weights_y <- sample(x = seq(from = 1, to = 2, by = .1), size = length(y), replace = TRUE)
# var.equal = FALSE
# 
# # un-weighted w/ var.equal = FALSE
# test_means(x = x, y = y, weights_x = NULL, weights_y = NULL, var.equal = FALSE)
# t.test(x = x, y = y, alternative = "two.sided", var.equal = FALSE) %>% tidy()
# wtd.t.test(x = x, y = y, weight = NULL, weighty = NULL, samedata = FALSE)
# 
# # unweighted w var.equal = TRUE
# test_means(x = x, y = y, weights_x = NULL, weights_y = NULL, var.equal = TRUE)
# t.test(x = x, y = y, alternative = "two.sided", var.equal = TRUE) %>% tidy()
# 
# # weighted
# test_means(x = x, y = y, weights_x = weights_x, weights_y = weights_y)
# wtd.t.test(x = x, y = y, weight = weights_x, weighty = weights_y, samedata = FALSE)


