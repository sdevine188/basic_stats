library(tidyverse)
library(rlang)


# create summarize_outliers()
summarize_outliers <- function(data, vars) {
        
        # get var_names from vars arg
        
        # handle single bare variables passed as vars
        if(deparse(substitute(vars)) %in% names(data)) {
                
                var_names <- deparse(substitute(vars))
                
        } else if("quosure" %in% class(vars) | "quosures" %in% class(vars)) {
                
                # handle var if it's passed using quo(), quos(), or vars(), including tidyselect helpers
                var_names <- data %>% ungroup() %>% select(!!!vars) %>% names()
                
        } else if(class(var) == "character") {
                
                # handle vars as a string
                var_names <- vars
        }

        
        ##################################################################################
        
        
        # get grouping_vars from grouped tbl
        grouping_vars <- data %>% groups() %>% map(.x = ., .f = ~ as_label(.x)) %>% unlist()
        
        
        ##################################################################################
        
        
        # create get_outlier_summary function
        get_outlier_summary <- function(data, current_var) {
                
                # get outlier_summary if data is not grouped
                if(is.null(grouping_vars)) {

                        # get outlier_summary
                        outlier_summary <- data %>% 
                                summarize(x = list(enframe(quantile(x = !!sym(current_var), na.rm = TRUE)))) %>% unnest(x) %>%
                                # for vectors with only a single unique non-NA value, converted to NA so that the single value won't be otherwise flagged as an outlier (see test notes below)
                                mutate(value_n_distinct = n_distinct(value),
                                       value = case_when(value_n_distinct == 1 ~ NA_real_, TRUE ~ value)) %>%
                                pivot_wider(names_from = name, values_from = value) %>%
                                rename(q0 = `0%`, q25 = `25%`, q50 = `50%`, q75 = `75%`, q100 = `100%`) %>%
                                mutate(iqr = q75 - q25, 
                                       lower_outlier_threshold = q25 - (iqr * 1.5),
                                       upper_outlier_threshold = q75 + (iqr * 1.5)) %>%
                                ungroup() %>% mutate(join_flag = 1)

                        
                        #################
                        
                        
                        # add upper/lower_outlier count/pct
                        outlier_summary <- data %>% mutate(join_flag = 1) %>%
                                left_join( ., outlier_summary, by = "join_flag") %>%
                                mutate(!!sym(str_c(current_var, "_lower_outlier_flag")) := case_when(!!sym(current_var) <= lower_outlier_threshold ~ 1, TRUE ~ 0),
                                       !!sym(str_c(current_var, "_upper_outlier_flag")) := case_when(!!sym(current_var) >= upper_outlier_threshold ~ 1, TRUE ~ 0)) %>%
                                summarize(q0 = unique(q0), q25 = unique(q25), q50 = unique(q50), q75 = unique(q50), q100 = unique(q100),
                                          lower_outlier_threshold = unique(lower_outlier_threshold),
                                          lower_outlier_count = sum(!!sym(str_c(current_var, "_lower_outlier_flag")), na.rm = TRUE),
                                          lower_outlier_pct = mean(!!sym(str_c(current_var, "_lower_outlier_flag")), na.rm = TRUE),
                                          upper_outlier_threshold = unique(upper_outlier_threshold),
                                          upper_outlier_count = sum(!!sym(str_c(current_var, "_upper_outlier_flag")), na.rm = TRUE),
                                          upper_outlier_pct = mean(!!sym(str_c(current_var, "_upper_outlier_flag")), na.rm = TRUE)) %>%
                                mutate(var = current_var) %>% select(var, everything())


                        ######################
                        
                        
                        # return outlier_summary
                        return(outlier_summary)
                }
                
                
                ##################################################################################
                
                
                # get outlier_summary if data is grouped
                if(!is.null(grouping_vars)) {

                        # get outlier_summary
                        outlier_summary <- data %>% group_by(!!!syms(grouping_vars)) %>%
                                summarize(x = list(enframe(quantile(x = !!sym(current_var), na.rm = TRUE)))) %>% unnest(x) %>%
                                group_by(!!!syms(grouping_vars)) %>%
                                # for vectors with only a single unique non-NA value, converted to NA so that the single value won't be otherwise flagged as an outlier (see test notes below)
                                mutate(value_n_distinct = n_distinct(value),
                                       value = case_when(value_n_distinct == 1 ~ NA_real_, TRUE ~ value)) %>%
                                ungroup() %>%
                                pivot_wider(names_from = name, values_from = value) %>%
                                rename(q0 = `0%`, q25 = `25%`, q50 = `50%`, q75 = `75%`, q100 = `100%`) %>%
                                mutate(iqr = q75 - q25,
                                       lower_outlier_threshold = q25 - (iqr * 1.5),
                                       upper_outlier_threshold = q75 + (iqr * 1.5)) %>%
                                ungroup()
          
                        
                        #####################
                        
                        
                        # add upper/lower_outlier count/pct
                        outlier_summary <- data %>% 
                                left_join(., outlier_summary, by = grouping_vars) %>%
                                mutate(!!sym(str_c(current_var, "_lower_outlier_flag")) := case_when(!!sym(current_var) <= lower_outlier_threshold ~ 1, TRUE ~ 0),
                                       !!sym(str_c(current_var, "_upper_outlier_flag")) := case_when(!!sym(current_var) >= upper_outlier_threshold ~ 1, TRUE ~ 0)) %>%
                                group_by(!!!syms(grouping_vars)) %>%
                                summarize(q0 = unique(q0), q25 = unique(q25), q50 = unique(q50), q75 = unique(q50), q100 = unique(q100),
                                          lower_outlier_threshold = unique(lower_outlier_threshold),
                                          lower_outlier_count = sum(!!sym(str_c(current_var, "_lower_outlier_flag")), na.rm = TRUE),
                                          lower_outlier_pct = mean(!!sym(str_c(current_var, "_lower_outlier_flag")), na.rm = TRUE),
                                          upper_outlier_threshold = unique(upper_outlier_threshold),
                                          upper_outlier_count = sum(!!sym(str_c(current_var, "_upper_outlier_flag")), na.rm = TRUE),
                                          upper_outlier_pct = mean(!!sym(str_c(current_var, "_upper_outlier_flag")), na.rm = TRUE)) %>%
                                ungroup() %>%
                                mutate(var = current_var) %>% select(var, everything())
                }
                
                
                ###################
                
                
                # return outlier_summary
                return(outlier_summary)
        }
        
        
        ##################################################################################
        
        
        # map over var_names calling get_outlier_summary()
        outlier_summary <- map_dfr(.x = var_names, .f = ~ data %>% get_outlier_summary(current_var = .x)) 

        
        ##################################################################################
        
        
        # return outlier_summary
        return(outlier_summary)
}


###################


# note that when quantile() is passed vectors with no non-NA variation, it returns the single value for all quantiles
# and this would otherwise result in the IQR = 0, and so IQR * 1.5 = 0, and so the single unique value would also become the upper/lower outlier threshold
# and so when flagging outliers as >= or <= to upper/lower_outlier threshold, the single unique value would be incorrectly flagged as an outlier
# to avoid this outcome, vectors with only a single unique non-NA value have the quantiles set to NA, 
# which cascades to NA outlier threshold values, and no outliers being flagged/dropped
var <- "height"
starwars %>% group_by(gender) %>% 
        summarize(x = list(enframe(quantile(x = !!sym(var), na.rm = TRUE)))) %>% unnest(x) %>% print(n = nrow(.))
starwars %>% filter(gender %in% c("none", "hermaphrodite"))


##################


# test
starwars %>% summarize_outliers(vars = height)
starwars %>% summarize_outliers(vars = height) %>% glimpse()
starwars %>% group_by(gender) %>% summarize_outliers(vars = height)
starwars %>% mutate(movie = case_when(mass > 170 ~ "old", mass <= 170 ~ "new", is.na(mass) ~ "new")) %>% 
        group_by(gender, movie) %>% summarize_outliers(vars = height)
starwars %>% summarize_outliers(vars = vars(height, mass))
starwars %>% summarize_outliers(vars = vars(height, mass)) %>% glimpse()
starwars %>% group_by(gender) %>% summarize_outliers(vars = vars(height, mass))

starwars %>% mutate(test_var_w_only_one_value = 42) %>% summarize_outliers(test_var_w_only_one_value)
starwars %>% mutate(test_var_w_only_one_value = 42) %>% summarize_outliers(vars(height, test_var_w_only_one_value))
starwars %>% mutate(test_var_w_only_one_value = 42) %>% group_by(gender) %>% summarize_outliers(test_var_w_only_one_value)


###############################################################################################################################
###############################################################################################################################
###############################################################################################################################


# create flag_outliers


###############################################################################################################################
###############################################################################################################################
###############################################################################################################################


# create plot_outliers


###############################################################################################################################
###############################################################################################################################
###############################################################################################################################


# create drop_outliers()
drop_outliers <- function(data, vars, which) {
        
        # get var_names from vars arg
        
        # handle single bare variables passed as vars
        if(deparse(substitute(vars)) %in% names(data)) {
                
                var_names <- deparse(substitute(vars))
                
        } else if("quosure" %in% class(vars) | "quosures" %in% class(vars)) {
                
                # handle var if it's passed using quo(), quos(), or vars(), including tidyselect helpers
                var_names <- data %>% ungroup() %>% select(!!!vars) %>% names()
                
        } else if(class(var) == "character") {
                
                # handle vars as a string
                var_names <- vars
        }
        
        
        ##################################################################################
        
        
        # get grouping_vars from grouped tbl
        grouping_vars <- data %>% groups() %>% map(.x = ., .f = ~ as_label(.x)) %>% unlist()
        
        
        ##################################################################################
        
        
        # create get_current_var_w_outliers_dropped()
        get_current_var_w_outliers_dropped <- function(data, current_var, which) {
        
                # get outlier_summary if data is not grouped
                if(is.null(grouping_vars)) {
                        
                        # get outlier_summary
                        outlier_summary <- data %>% 
                                summarize(x = list(enframe(quantile(x = !!sym(current_var), na.rm = TRUE)))) %>% unnest(x) %>%
                                # for vectors with only a single unique non-NA value, converted to NA so that the single value won't be otherwise flagged as an outlier (see test notes below)
                                mutate(value_n_distinct = n_distinct(value),
                                       value = case_when(value_n_distinct == 1 ~ NA_real_, TRUE ~ value)) %>%
                                pivot_wider(names_from = name, values_from = value) %>%
                                rename(q0 = `0%`, q25 = `25%`, q50 = `50%`, q75 = `75%`, q100 = `100%`) %>%
                                mutate(iqr = q75 - q25, 
                                       lower_outlier_threshold = q25 - (iqr * 1.5),
                                       upper_outlier_threshold = q75 + (iqr * 1.5)) %>%
                                ungroup() %>% mutate(join_flag = 1)
                
                        
                        #################
                        
                        
                        # when which = "both"
                        if(which == "both") {
                                
                                # get current_var_w_outliers_dropped
                                current_var_w_outliers_dropped <- data %>% mutate(join_flag = 1) %>%
                                        left_join( ., outlier_summary, by = "join_flag") %>%
                                        mutate(!!sym(current_var) := case_when(!!sym(current_var) <= lower_outlier_threshold ~ NA_real_,
                                                                               !!sym(current_var) >= upper_outlier_threshold ~ NA_real_,
                                                                               TRUE ~ as.numeric(!!sym(current_var))))
                                
                                # return current_var_w_outliers_dropped 
                                return(current_var_w_outliers_dropped %>% select(!!sym(current_var)))
                        }
                        
                        
                        ###################
                        
                        
                        # when which = "upper"
                        if(which == "upper") {
                                
                                # get current_var_w_outliers_dropped
                                current_var_w_outliers_dropped <- data %>% mutate(join_flag = 1) %>%
                                        left_join( ., outlier_summary, by = "join_flag") %>%
                                        mutate(!!sym(current_var) := case_when(!!sym(current_var) >= upper_outlier_threshold ~ NA_real_,
                                                                               TRUE ~ as.numeric(!!sym(current_var))))
                                
                                # return current_var_w_outliers_dropped 
                                return(current_var_w_outliers_dropped %>% select(!!sym(current_var)))
                        }
                        
                        
                        ###################
                        
                        
                        # when which = "upper"
                        if(which == "lower") {
                                
                                # get current_var_w_outliers_dropped
                                current_var_w_outliers_dropped <- data %>% mutate(join_flag = 1) %>%
                                        left_join( ., outlier_summary, by = "join_flag") %>%
                                        mutate(!!sym(current_var) := case_when(!!sym(current_var) <= lower_outlier_threshold ~ NA_real_,
                                                                               TRUE ~ as.numeric(!!sym(current_var))))
                                
                                # return current_var_w_outliers_dropped 
                                return(current_var_w_outliers_dropped %>% select(!!sym(current_var)))
                        }
                }
                
                
                ####################################################################################################
                
                
                # get outlier_summary if data is grouped
                if(!is.null(grouping_vars)) {
                        
                        # get outlier_summary
                        outlier_summary <- data %>% group_by(!!!syms(grouping_vars)) %>%
                                summarize(x = list(enframe(quantile(x = !!sym(current_var), na.rm = TRUE)))) %>% unnest(x) %>%
                                group_by(!!!syms(grouping_vars)) %>%
                                # for vectors with only a single unique non-NA value, converted to NA so that the single value won't be otherwise flagged as an outlier (see test notes below)
                                mutate(value_n_distinct = n_distinct(value),
                                       value = case_when(value_n_distinct == 1 ~ NA_real_, TRUE ~ value)) %>%
                                ungroup() %>%
                                pivot_wider(names_from = name, values_from = value) %>%
                                rename(q0 = `0%`, q25 = `25%`, q50 = `50%`, q75 = `75%`, q100 = `100%`) %>%
                                mutate(iqr = q75 - q25,
                                       lower_outlier_threshold = q25 - (iqr * 1.5),
                                       upper_outlier_threshold = q75 + (iqr * 1.5)) %>%
                                ungroup()
                        
                        
                        #################
                        
                        
                        # when which = "both"
                        if(which == "both") {
                                
                                # get current_var_w_outliers_dropped
                                current_var_w_outliers_dropped <- data %>% 
                                        left_join(., outlier_summary, by = grouping_vars) %>%
                                        mutate(!!sym(current_var) := case_when(!!sym(current_var) <= lower_outlier_threshold ~ NA_real_,
                                                                               !!sym(current_var) >= upper_outlier_threshold ~ NA_real_,
                                                                               TRUE ~ as.numeric(!!sym(current_var))))
                                
                                # return current_var_w_outliers_dropped 
                                return(current_var_w_outliers_dropped %>% select(!!sym(current_var)))
                        }
                        
                        
                        ###################
                        
                        
                        # when which = "upper"
                        if(which == "upper") {
                                
                                # get current_var_w_outliers_dropped
                                current_var_w_outliers_dropped <- data %>% 
                                        left_join(., outlier_summary, by = grouping_vars) %>%
                                        mutate(!!sym(current_var) := case_when(!!sym(current_var) >= upper_outlier_threshold ~ NA_real_,
                                                                               TRUE ~ as.numeric(!!sym(current_var))))
                                
                                # return current_var_w_outliers_dropped 
                                return(current_var_w_outliers_dropped %>% select(!!sym(current_var)))
                        }
                        
                        
                        ###################
                        
                        
                        # when which = "upper"
                        if(which == "lower") {
                                
                                # get current_var_w_outliers_dropped
                                current_var_w_outliers_dropped <- data %>% 
                                        left_join(., outlier_summary, by = grouping_vars) %>%
                                        mutate(!!sym(current_var) := case_when(!!sym(current_var) <= lower_outlier_threshold ~ NA_real_,
                                                                               TRUE ~ as.numeric(!!sym(current_var))))
                                
                                # return current_var_w_outliers_dropped 
                                return(current_var_w_outliers_dropped %>% select(!!sym(current_var)))
                        }
                }
        }
        
        ##################################################################################

        
        # map over var_names calling get_current_var_w_outliers_dropped()
        data_w_outliers_dropped <- map_dfc(.x = var_names, .f = ~ data %>% get_current_var_w_outliers_dropped(current_var = .x, which = which)) 
        
        
        ####################
        
        
        # overwrite data with data_w_outliers_dropped to get updated_data
        updated_data <- data %>% select(-c(var_names)) %>% bind_cols(data_w_outliers_dropped) %>%
                select(names(data))
        
        
        ####################
        
        
        # return updated_data
        return(updated_data)
}


###################


# test
starwars %>% summarize_outliers(vars = height)

starwars %>% select(height) %>% print(n = 20)
starwars %>% drop_outliers(height, which = "both") %>% select(height) %>% print(n = 20)
starwars %>% ggplot(data = ., aes(x = height)) + geom_histogram()
starwars %>% drop_outliers(height, which = "both") %>% ggplot(data = ., aes(x = height)) + geom_histogram()

starwars %>% select(height) %>% print(n = 20)
starwars %>% drop_outliers(height, which = "upper") %>% select(height) %>% print(n = 20)
starwars %>% ggplot(data = ., aes(x = height)) + geom_histogram()
starwars %>% drop_outliers(height, which = "upper") %>% ggplot(data = ., aes(x = height)) + geom_histogram()

starwars %>% select(height) %>% print(n = 20)
starwars %>% drop_outliers(height, which = "lower") %>% select(height) %>% print(n = 20)
starwars %>% ggplot(data = ., aes(x = height)) + geom_histogram()
starwars %>% drop_outliers(height, which = "lower") %>% ggplot(data = ., aes(x = height)) + geom_histogram()


#################


starwars %>% summarize_outliers(vars = vars(mass, height))

starwars %>% select(mass, height) %>% print(n = 20)
starwars %>% drop_outliers(vars(mass, height), which = "both") %>% select(mass, height) %>% print(n = 20)

starwars %>% select(mass, height) %>% print(n = 20)
starwars %>% drop_outliers(vars(mass, height), which = "upper") %>% select(mass, height) %>% print(n = 20)

starwars %>% select(mass, height) %>% print(n = 20)
starwars %>% drop_outliers(vars(mass, height), which = "lower") %>% select(mass, height) %>% print(n = 20)


##################


starwars %>% group_by(gender) %>% summarize_outliers(vars = vars(mass, height))
starwars %>% count(gender)
starwars %>% filter(gender == "none") %>% count(mass)

starwars %>% group_by(gender) %>% select(gender, mass, height) %>% print(n = 30)
starwars %>% group_by(gender) %>% drop_outliers(vars(mass, height), which = "both") %>% select(gender, mass, height) %>% print(n = 30)


