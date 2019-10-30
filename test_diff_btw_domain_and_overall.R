# # load test_diff_btw_domain_and_overall()
# current_wd <- getwd()
# setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/basic_stats")
# source("test_diff_btw_domain_and_overall.R")
# setwd(current_wd)

library(tidyverse)

# load get_wilson_conf_int()
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/basic_stats")
source("get_wilson_conf_int.R")
setwd(current_wd)

# reference on estimate reliability/suppression criteria 
# http://health.utah.gov/opha/IBIShelp/DataSuppression.pdf


###############################


# create test_diff_btw_domain_and_overall_mapper function
test_diff_btw_domain_and_overall_mapper <- function(data, domain_var, current_domain_var_value, stat, stat_var,
                                weight_var, z, wilson_newcombe, strict_suppression, confidentiality_concerns, sample_or_population) {
        
        # handle arguments
        
        # get domain_var_sym
        domain_var_sym <- sym(domain_var)

        # get variable_sym
        stat_var_sym <- sym(stat_var)
        
        
        #################################################################################################
        
        
        # handle stat = "proportion" when wilson_newcombe = FALSE
        
        
        ################
        
        
        # handle stat = proportion when wilson_newcombe = TRUE
        if(stat == "proportion" & wilson_newcombe == TRUE) {
                

                # ensure the stat_var for proportion has only values of one, zero, or NA
                if(sum(!((data %>% count(!!stat_var_sym) %>% pull(!!stat_var_sym)) %in% c(0, 1, NA))) != 0) {
                        stop("When stat = 'proportion', the stat_var must be a dummy taking values of only zero, one, or NA.
                             At least one stat_var value is not in (0, 1, NA).")
                }
        
        
                ###############
        
        
                # get domain terms
                n_domain <- data %>% filter(!!domain_var_sym == current_domain_var_value) %>%
                        summarize(weighted_sum = sum(weight_var, na.rm = TRUE)) %>% pull(weighted_sum)
        
                p_domain <- data %>% filter(!!domain_var_sym == current_domain_var_value, !!stat_var_sym == 1) %>%
                        mutate(stat_var_weighted = !!stat_var_sym * weight_var) %>%
                        summarize(weighted_proportion = sum(stat_var_weighted, na.rm = TRUE) / n_domain) %>% pull(weighted_proportion)
        
        
                ###############
        
        
                # get other domain stats not actually needed for wilson/newcombe calculations, but just helpful stats to return w/ results
                n_domain_unweighted <- data %>% filter(!!domain_var_sym == current_domain_var_value) %>% nrow()
                x_domain_weighted <- data %>% filter(!!domain_var_sym == current_domain_var_value, !!stat_var_sym == 1) %>%
                        mutate(stat_var_weighted = !!stat_var_sym * weight_var) %>%
                        summarize(sum_x = sum(stat_var_weighted, na.rm = TRUE)) %>% pull(sum_x)
                x_domain_unweighted <- data %>% filter(!!domain_var_sym == current_domain_var_value, !!stat_var_sym == 1) %>% nrow()
                p_domain_unweighted <- x_domain_unweighted / n_domain_unweighted
        
        
                ###############
        
        
                # get complement terms
                n_complement <- data %>% filter(!(!!domain_var_sym == current_domain_var_value)) %>%
                        summarize(weighted_sum = sum(weight_var, na.rm = TRUE)) %>% pull(weighted_sum)
        
                p_complement <- data %>% filter(!(!!domain_var_sym == current_domain_var_value), !!stat_var_sym == 1) %>%
                        mutate(stat_var_weighted = !!stat_var_sym * weight_var) %>%
                        summarize(weighted_proportion = sum(stat_var_weighted, na.rm = TRUE) / n_complement) %>% pull(weighted_proportion)
        
        
                ##############
        
                # get diff_btw_domain_and_complement_p
                diff_btw_domain_and_complement_p <- p_domain - p_complement
        
                # get wilson confidence interval for domain and complement
                domain_p_wilson_conf_int <- get_wilson_conf_int(n = n_domain, p = p_domain, z = z)
                complement_p_wilson_conf_int <- get_wilson_conf_int(n = n_complement, p = p_complement, z = z)
        
                # extract domain/complement confidence intervals for use in newcombe formula below
                upper_domain <- domain_p_wilson_conf_int %>% pull(conf_int_upper)
                lower_domain <- domain_p_wilson_conf_int %>% pull(conf_int_lower)
                upper_complement <- complement_p_wilson_conf_int %>% pull(conf_int_upper)
                lower_complement <- complement_p_wilson_conf_int %>% pull(conf_int_lower)
                
                
                #################
                
                
                # handle suppression/confidentiality
                
                # get domain standard error, for use in coefficient of variation
                # note that se has to be calculated manually instead of using se() to account for asymmetric wilson conf_int
                standard_error_domain <- (upper_domain - lower_domain) / (1.96 * 2)
                
                # get domain coefficient of variation
                is_p_domain_the_min <- ifelse(p_domain <= (1 - p_domain), 1, 0)
                if(is_p_domain_the_min == 1) {
                        coeff_of_variation_domain <- standard_error_domain / (p_domain + .001)
                } 
                if(is_p_domain_the_min == 0){
                        coeff_of_variation_domain <- standard_error_domain / ((1 - p_domain) + .001)
                }
                
                # handle strict/minimum suppression
                if(strict_suppression == TRUE) {
                        
                        if(confidentiality_concerns == TRUE) {
                                
                                if(sample_or_population == "sample") {
                                        suppressed_flag <- ifelse(x_domain_unweighted < 20 | n_domain_unweighted < 100 | coeff_of_variation_domain > .3, 1, 0)
                                        unreliable_flag <- 0
                                }
                                
                                if(sample_or_population == "population") {
                                        suppressed_flag <- ifelse(x_domain_unweighted < 20 | n_domain_unweighted < 100 | coeff_of_variation_domain > .3, 1, 0)
                                        unreliable_flag <- 0
                                }
                        }
                        
                        if(confidentiality_concerns == FALSE) {
                                
                                if(sample_or_population == "sample") {
                                        suppressed_flag <- ifelse(x_domain_unweighted < 10 | coeff_of_variation_domain > .3, 1, 0)
                                        unreliable_flag <- 0
                                }
                                
                                if(sample_or_population == "population") {
                                        suppressed_flag <- ifelse(x_domain_unweighted < 20 | n_domain_unweighted < 100 | coeff_of_variation_domain > .3, 1, 0)
                                        unreliable_flag <- 0
                                }
                        }
                }
                
                if(strict_suppression == FALSE) {
                        
                        if(confidentiality_concerns == TRUE) {
                                
                                # note the minimum criteria are the same for both sample and population
                                suppressed_flag <- ifelse(x_domain_unweighted < 20 | n_domain_unweighted < 100 | coeff_of_variation_domain > .5, 1, 0)
                                unreliable_flag <- ifelse(x_domain_unweighted < 20 | n_domain_unweighted < 100 | 
                                                                  (coeff_of_variation_domain <= .5 & coeff_of_variation_domain > .3), 1, 0)
                                        
                        }
                        
                        if(confidentiality_concerns == FALSE) {
                               
                                # note the minimum criteria are the same for both sample and population
                                suppressed_flag <- ifelse(coeff_of_variation_domain > .5 | x_domain_unweighted < 10, 1, 0)
                                unreliable_flag <- ifelse((coeff_of_variation_domain <= .5 & coeff_of_variation_domain > .3) |
                                                                  (x_domain_unweighted >= 10 & x_domain_unweighted < 20), 1, 0)
                        }
                }
        
        
                ################
        
        
                # get newcombe confidence interval for difference in proportions between domain and complement
                newcombe_conf_int_lower_for_diff_btw_domain_and_complement_p <- (p_domain - p_complement) -
                        sqrt((p_domain - lower_domain)^2 + (upper_complement - p_complement)^2)
        
                newcombe_conf_int_upper_for_diff_btw_domain_and_complement_p <- (p_domain - p_complement) +
                        sqrt((upper_domain - p_domain)^2 + (p_complement - lower_complement)^2)
                
                
                ################
        
        
                # use derivation to scale newcombe p and conf_int for diff_btw_domain_and_complement
                # to be p and conf int for diff_btw_domain_and_overall
                diff_btw_domain_and_overall_p <- (1 - (n_domain / (n_domain + n_complement))) * diff_btw_domain_and_complement_p
                diff_btw_domain_and_overall_p
        
                conf_int_upper_for_diff_btw_domain_and_overall_p <- (1 - (n_domain / (n_domain + n_complement))) *
                        newcombe_conf_int_upper_for_diff_btw_domain_and_complement_p
        
                conf_int_lower_for_diff_btw_domain_and_overall_p <- (1 - (n_domain / (n_domain + n_complement))) *
                        newcombe_conf_int_lower_for_diff_btw_domain_and_complement_p
        
                diff_btw_domain_and_overall_significant <- ifelse((conf_int_lower_for_diff_btw_domain_and_overall_p > 0 &
                                                                           conf_int_upper_for_diff_btw_domain_and_overall_p > 0) |
                                                                          (conf_int_lower_for_diff_btw_domain_and_overall_p < 0 &
                                                                conf_int_upper_for_diff_btw_domain_and_overall_p < 0), 1, 0)
                
        
                ################
        
        
                # get domain_tbl
                domain_tbl <- tibble(domain_var = domain_var, domain_var_value = current_domain_var_value, stat_var = stat_var, stat = stat,
                        n_weighted = n_domain, n_unweighted = n_domain_unweighted,
                        x_weighted = x_domain_weighted, x_unweighted = x_domain_unweighted,
                        p_weighted = p_domain, p_unweighted = p_domain_unweighted,
                              conf_int_lower = lower_domain, conf_int_upper = upper_domain,
                        standard_error = standard_error_domain, coeff_of_variation = coeff_of_variation_domain,
                        strict_suppression = strict_suppression, confidentiality_concerns = confidentiality_concerns,
                        sample_or_population = sample_or_population,
                        unreliable_flag = unreliable_flag, suppressed_flag = suppressed_flag,
                        diff_btw_domain_and_overall_p = diff_btw_domain_and_overall_p,
                       conf_int_lower_weighted_for_diff_btw_domain_and_overall_p = conf_int_lower_for_diff_btw_domain_and_overall_p,
                       conf_int_upper_weighted_for_diff_btw_domain_and_overall_p = conf_int_upper_for_diff_btw_domain_and_overall_p,
                       diff_btw_domain_and_overall_significant = diff_btw_domain_and_overall_significant)
        }
        
        
        ##############################
        
        
        # return domain_tbl
        return(domain_tbl)
}



################


# create test_diff_in_proportions_btw_domain_and_overall_wrapper function
test_diff_btw_domain_and_overall <- function(data, domain_var, stat, stat_var, weight_var = NULL, z = 1.96, wilson_newcombe = FALSE,
                                              strict_suppression = TRUE, confidentiality_concerns = TRUE, sample_or_population = "sample") {
        
        # create domain_var_sym
        domain_var_sym <- sym(domain_var)
        
        # create variable_sym
        stat_var_sym <- sym(stat_var)
        
        
        ################
        
        
        # handle weights
        
        # add weight_var to data, and get weight_var_sym
        if(!is.null(weight_var)) {
                weight_var_sym <- sym(weight_var)
                data <- data %>% mutate(weight_var := !!weight_var_sym)
                weight_var_sym <- sym(weight_var)
        }
        
        # add weight_var to data if weight_var argument is NULL if needed
        if(is.null(weight_var)) {
                data <- data %>% mutate(weight_var = 1)
                weight_var_sym <- sym("weight_var")
        }
        
        
        ###########################
        
        
        # call test_diff_btw_domain_and_overall_mapper to get stats for domains
        domain_tbl <- map_dfr(.x = data %>% filter(!is.na(!!domain_var_sym)) %>% distinct(!!domain_var_sym) %>% pull(!!domain_var_sym), 
                .f = ~ test_diff_btw_domain_and_overall_mapper(data = data, domain_var = domain_var, current_domain_var_value = .x,
                                                                       stat = stat, stat_var = stat_var, weight_var = weight_var, z = z, 
                                                                       wilson_newcombe = wilson_newcombe,
                                                               strict_suppression = strict_suppression, confidentiality_concerns = confidentiality_concerns, 
                                                               sample_or_population = sample_or_population))

        
        ##########################
        
        
        # get overall stats if stat = proportion
        if(stat == "proportion") {
                
                # get overall stats, not actually needed for domain/overall test, but just helpful as descriptive stats to return w resutls
                n_overall_weighted <- data %>% summarize(weighted_sum = sum(weight_var, na.rm = TRUE)) %>% pull(weighted_sum)
                n_overall_unweighted <- data %>% nrow()
                x_overall_weighted <- data %>% mutate(stat_var_weighted = !!stat_var_sym * weight_var) %>%
                        summarize(sum_x = sum(stat_var_weighted, na.rm = TRUE)) %>% pull(sum_x)
                x_overall_unweighted <- data %>% filter(!!stat_var_sym == 1) %>% nrow()
                p_overall_weighted <- data %>% filter(!!stat_var_sym == 1) %>%
                        mutate(stat_var_weighted = !!stat_var_sym * weight_var) %>%
                        summarize(weighted_proportion = sum(stat_var_weighted, na.rm = TRUE) / n_overall_weighted) %>% pull(weighted_proportion)
                p_overall_unweighted <- x_overall_unweighted / n_overall_unweighted
                conf_int_lower_overall <- get_wilson_conf_int(n = n_overall_weighted, p = p_overall_weighted, z = z) %>%
                        pull(conf_int_lower)
                conf_int_upper_overall <- get_wilson_conf_int(n = n_overall_weighted, p = p_overall_weighted, z = z) %>%
                        pull(conf_int_upper)
                
                
                #################
                
                
                # handle suppression/confidentiality
                
                # get domain standard error, for use in coefficient of variation
                # note that se has to be calculated manually instead of using se() to account for asymmetric wilson conf_int
                standard_error_overall <- (conf_int_upper_overall - conf_int_lower_overall) / (1.96 * 2)
                
                # get domain coefficient of variation
                is_p_overall_weighted_the_min <- ifelse(p_overall_weighted <= (1 - p_overall_weighted), 1, 0)
                if(is_p_overall_weighted_the_min == 1) {
                        coeff_of_variation_overall <- standard_error_overall / (p_overall_weighted + .001)
                } 
                if(is_p_overall_weighted_the_min == 0){
                        coeff_of_variation_overall <- standard_error_overall / ((1 - p_overall_weighted) + .001)
                }
                
                # handle strict/minimum suppression
                if(strict_suppression == TRUE) {
                        
                        if(confidentiality_concerns == TRUE) {
                                
                                if(sample_or_population == "sample") {
                                        suppressed_flag <- ifelse(x_overall_unweighted < 20 | n_overall_unweighted < 100 | coeff_of_variation_overall > .3, 1, 0)
                                        unreliable_flag <- 0
                                }
                                
                                if(sample_or_population == "population") {
                                        suppressed_flag <- ifelse(x_overall_unweighted < 20 | n_overall_unweighted < 100 | coeff_of_variation_overall > .3, 1, 0)
                                        unreliable_flag <- 0
                                }
                        }
                        
                        if(confidentiality_concerns == FALSE) {
                                
                                if(sample_or_population == "sample") {
                                        suppressed_flag <- ifelse(x_overall_unweighted < 10 | coeff_of_variation_overall > .3, 1, 0)
                                        unreliable_flag <- 0
                                }
                                
                                if(sample_or_population == "population") {
                                        suppressed_flag <- ifelse(x_overall_unweighted < 20 | n_overall_unweighted < 100 | coeff_of_variation_overall > .3, 1, 0)
                                        unreliable_flag <- 0
                                }
                        }
                }
                
                if(strict_suppression == FALSE) {
                        
                        if(confidentiality_concerns == TRUE) {
                                
                                # note the minimum criteria are the same for both sample and population
                                suppressed_flag <- ifelse(x_overall_unweighted < 20 | n_overall_unweighted < 100 | coeff_of_variation_overall > .5, 1, 0)
                                unreliable_flag <- ifelse(x_overall_unweighted < 20 | n_overall_unweighted < 100 | 
                                                                  (coeff_of_variation_overall <= .5 & coeff_of_variation_overall > .3), 1, 0)
                                
                        }
                        
                        if(confidentiality_concerns == FALSE) {
                                
                                # note the minimum criteria are the same for both sample and population
                                suppressed_flag <- ifelse(x_overall_unweighted < 10 | coeff_of_variation_overall > .5, 1, 0)
                                unreliable_flag <- ifelse((coeff_of_variation_overall <= .5 & coeff_of_variation_overall > .3) |
                                                                  (x_overall_unweighted >= 10 & x_overall_unweighted < 20), 1, 0)
                        }
                }
                
                
                ###########################
                
                
                # add overall stats to domain_tbl to get output_tbl
                output_tbl <- tibble(domain_var = domain_var, domain_var_value = "overall", stat_var = stat_var, stat = stat,
                                     n_weighted = n_overall_weighted, n_unweighted = n_overall_unweighted,
                                     x_weighted = x_overall_weighted, x_unweighted = x_overall_unweighted,
                                     p_weighted = p_overall_weighted, p_unweighted = p_overall_unweighted,
                                     conf_int_lower = conf_int_lower_overall, conf_int_upper = conf_int_upper_overall,
                                     standard_error = standard_error_overall, coeff_of_variation = coeff_of_variation_overall,
                                     strict_suppression = strict_suppression, confidentiality_concerns = confidentiality_concerns,
                                     sample_or_population = sample_or_population,
                                     unreliable_flag = unreliable_flag, suppressed_flag = suppressed_flag,
                                     diff_btw_domain_and_overall_p = NA,
                                     conf_int_lower_weighted_for_diff_btw_domain_and_overall_p = NA,
                                     conf_int_upper_weighted_for_diff_btw_domain_and_overall_p = NA,
                                     diff_btw_domain_and_overall_significant = NA) %>%
                        bind_rows(., domain_tbl)        
        }
        
        
        ################################
        
        
        # return output_tbl
        return(output_tbl)
}



###########################################################################################
###########################################################################################


# # test
# data <- starwars %>% mutate(eye_color_blue_dummy = ifelse(eye_color == "blue", 1, 0))
# domain_var <- "gender"
# current_domain_var_value <- "male"
# stat <- "proportion"
# stat_var <- "eye_color_blue_dummy"
# weight_var <- NULL
# wilson_newcombe <- TRUE
# z <- 1.96
# sample_or_population <- "sample"
# confidentiality_concerns <- FALSE
# strict_suppression <- FALSE
# 
# test_diff_btw_domain_and_overall_mapper(data = data, domain_var = domain_var, current_domain_var_value = current_domain_var_value,
#                                         stat = stat, stat_var = stat_var, weight_var = weight_var, z = z, wilson_newcombe = wilson_newcombe,
#                                        sample_or_population = sample_or_population, confidentiality_concerns = confidentiality_concerns,
#                                        strict_suppression = strict_suppression) %>%
#                                       glimpse()
# 
# test_diff_btw_domain_and_overall(data = data, domain_var = domain_var, stat = stat, stat_var = stat_var,
#                                  weight_var = weight_var, z = z, wilson_newcombe = wilson_newcombe
#                                        sample_or_population = sample_or_population, confidentiality_concerns = confidentiality_concerns,
#                                        strict_suppression = strict_suppression) %>%) %>% glimpse()
# 


