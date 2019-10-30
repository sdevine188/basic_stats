# # load test_means_for_vars()
# current_wd <- getwd()
# setwd("H:/R/helper_scripts")
# source("test_means_of_vars.R")
# setwd(current_wd)

library(tidyverse)
library(purrr)
library(rsample)
# library(caret)

# load test_means()
source("test_means.R")

# get attrition data
# data(attrition)
# glimpse(attrition)

# get attritted
# attritted <- attrition %>% filter(Attrition == "Yes")
# dim(attritted)

# not_attritted <- attrition %>% filter(Attrition == "No")
# dim(not_attritted)

# split data
# set.seed(123)
# in_attrition_1 <- createDataPartition(y = attrition %>% pull(Attrition), p = .5, list = FALSE)
# in_attrition_1
# attrition_1 <- attrition %>% filter(row_number() %in% in_attrition_1)
# dim(attrition_1)
# attrition_2 <- attrition %>% filter(!(row_number() %in% in_attrition_1))
# dim(attrition_2)

# note that unweighted t.test with var.equal = TRUE will match ols dummy t.test regression



#########################################################################


# create test_means_of_vars function
test_means_of_vars <- function(x_tbl, y_tbl, .x, weights_x = NULL, weights_y = NULL, var.equal = FALSE,
                               weights_are_statistical_not_sample_size = TRUE, yates_correction = FALSE) {
        
        # get current_variable_to_test_sym
        current_variable_to_test <- .x
        current_variable_to_test_sym <- sym(current_variable_to_test)
        
        # extract vectors of current_variable_to_test from x_tbl and y_tbl
        x <- x_tbl %>% pull(!!current_variable_to_test_sym)
        y <- y_tbl %>% pull(!!current_variable_to_test_sym)
        
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
        
        # call test_means
        output <- test_means(x = x, y = y, weights_x = weights_x, weights_y = weights_y, var.equal = var.equal,
                   weights_are_statistical_not_sample_size = weights_are_statistical_not_sample_size, 
                   yates_correction = yates_correction)
        
        # add name of current_variable_to_test
        output <- output %>% mutate(variable = current_variable_to_test) %>% select(variable, everything())
        return(output)
}

###############


# # test get_diff_in_means
# x_tbl <- attritted
# y_tbl <- not_attritted
# weights_x <- NULL
# weights_y <- NULL
# weights_x <- sample(x = seq(from = 1, to = 2, by = .1), size = nrow(x_tbl), replace = TRUE)
# weights_y <- sample(x = seq(from = 1, to = 2, by = .1), size = nrow(y_tbl), replace = TRUE)
# weights_are_statistical_not_sample_size = TRUE
# yates_correction = FALSE
# 
# current_variable_to_test <- "DistanceFromHome"
# variables_to_test <- c("Age", "DailyRate", "DistanceFromHome", "HourlyRate")
# 
# 
# map_dfr(.x = variables_to_test, .f = ~ test_means_of_vars(x_tbl = x_tbl, y_tbl = y_tbl, .x)) %>% data.frame()
# map_dfr(.x = variables_to_test, .f = ~ test_means_of_vars(x_tbl = x_tbl, y_tbl = y_tbl, .x, var.equal = TRUE)) %>% data.frame()
# 
# map_dfr(.x = variables_to_test, .f = ~ test_means_of_vars(x_tbl = x_tbl, y_tbl = y_tbl, .x,
#                                                           weights_x = weights_x, weights_y = weights_y))
# map_dfr(.x = variables_to_test, .f = ~ test_means_of_vars(x_tbl = x_tbl, y_tbl = y_tbl, .x, var.equal = TRUE,
#                                                           weights_x = weights_x, weights_y = weights_y))
# 
# # confirm error message that weights needs to be a vector, not a variable name
# map_dfr(.x = variables_to_test, .f = ~ test_means_of_vars(x_tbl = x_tbl, y_tbl = y_tbl, .x,
#                                                           weights_x = "weights_x", weights_y = "weights_y"))
        
        
        
        