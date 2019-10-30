# # load get_ibfa_output_table()
# current_wd <- getwd()
# setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/RED/IBFA/analysis")
# source("get_ibfa_output_table.R")
# setwd(current_wd)

library(tidyverse)
library(janitor)

# load test_diff_btw_domain_and_overall()
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/basic_stats")
source("test_diff_btw_domain_and_overall.R")
setwd(current_wd)

# load as_percent()
current_wd <- getwd()
setwd("C:/users/sjdevine/Work Folders/Desktop/personal_drive/R/helper_scripts")
source("as_percent.R")
setwd(current_wd)


options(scipen=999)

# create get_ibfa_output_table()
get_ibfa_output_table <- function(data, domain_var, return_expanded_table = FALSE) {
        
        # get full table w/ treatment & control 
        treatment_fraud_domain_tbl <- data %>% filter(treatment_flag == 1) %>% 
                test_diff_btw_domain_and_overall(data = ., domain_var = domain_var, stat = "proportion", stat_var = "SOF_Finding_SPFF", 
                                                 weight_var = "weight", z = 1.96, wilson_newcombe = TRUE,
                                                 sample_or_population = "sample", confidentiality_concerns = FALSE, strict_suppression = FALSE) %>% 
                rename_at(.vars = vars(everything()), .funs = ~ str_c("treatment_fraud_", .))
        
        
        ##############
        
        
        treatment_denied_domain_tbl <- data %>% filter(treatment_flag == 1) %>% 
                test_diff_btw_domain_and_overall(data = ., domain_var = domain_var, stat = "proportion", stat_var = "Case_OutcomeDenied", 
                                                 weight_var = "weight", z = 1.96, wilson_newcombe = TRUE,
                                                 sample_or_population = "sample", confidentiality_concerns = FALSE, strict_suppression = FALSE) %>% 
                rename_at(.vars = vars(everything()), .funs = ~ str_c("treatment_denied_", .))
        
        
        ################
        
        
        control_denied_domain_tbl <- data %>% filter(frame_control_flag == 1) %>% 
                test_diff_btw_domain_and_overall(data = ., domain_var = domain_var, stat = "proportion", stat_var = "outcome_denied", 
                                                 weight_var = "weight", z = 1.96, wilson_newcombe = TRUE,
                                                 sample_or_population = "sample", confidentiality_concerns = FALSE, strict_suppression = FALSE) %>% 
                rename_at(.vars = vars(everything()), .funs = ~ str_c("control_denied_", .))
        
        
        ##############################################################################################################
        
        
        # combine into a single domain tbl
        domain_tbl <- treatment_fraud_domain_tbl %>% left_join(., treatment_denied_domain_tbl, 
                                                                                     by = c("treatment_fraud_domain_var_value" = "treatment_denied_domain_var_value")) %>%
                left_join(., control_denied_domain_tbl, by = c("treatment_fraud_domain_var_value" = "control_denied_domain_var_value"))
        
        
        ##################################################################################################################
        
        
        # get diff btw treatment/control denial rates, and use newcombe formula to get confidence interval for difference
        domain_tbl <- domain_tbl %>% 
                mutate(diff_btw_treatment_and_control_denied_p_weighted = treatment_denied_p_weighted - control_denied_p_weighted,
                       diff_btw_treatment_and_control_denied_conf_int_lower = diff_btw_treatment_and_control_denied_p_weighted - 
                               sqrt((treatment_denied_p_weighted - treatment_denied_conf_int_lower)^2 + 
                                            (control_denied_conf_int_upper - control_denied_p_weighted)^2),
                       diff_btw_treatment_and_control_denied_conf_int_upper = diff_btw_treatment_and_control_denied_p_weighted +
                               sqrt((treatment_denied_conf_int_upper - treatment_denied_p_weighted)^2 + 
                                            (control_denied_p_weighted - control_denied_conf_int_lower)^2))

        
        #################################
        
        
        # add unreliable/suppression flags
        domain_tbl <- domain_tbl %>%                                                                
                mutate(diff_btw_treatment_and_control_denied_p_weighted_suppressed_flag = 
                               case_when((treatment_denied_suppressed_flag == 1) | (control_denied_suppressed_flag == 1) ~ 1, TRUE ~ 0),
                       diff_btw_treatment_and_control_denied_conf_int_suppressed_flag = 
                               case_when(treatment_denied_suppressed_flag == 1 | control_denied_suppressed_flag == 1 ~ 1, TRUE ~ 0),
                       diff_btw_treatment_and_control_denied_conf_int_lower_negative_flag = case_when(
                               diff_btw_treatment_and_control_denied_conf_int_lower < 0 ~ 1, TRUE ~ 0),
                       diff_btw_treatment_and_control_denied_conf_int_upper_negative_flag = case_when(
                               diff_btw_treatment_and_control_denied_conf_int_upper < 0 ~ 1, TRUE ~ 0),
                       diff_btw_treatment_and_control_denied_p_weighted_unreliable_flag = 
                               case_when((diff_btw_treatment_and_control_denied_p_weighted_suppressed_flag == 0 & treatment_denied_unreliable_flag == 1) | 
                                                 (diff_btw_treatment_and_control_denied_p_weighted_suppressed_flag == 0 & control_denied_unreliable_flag == 1) |
                                                 (diff_btw_treatment_and_control_denied_p_weighted_suppressed_flag == 0 &
                                                          diff_btw_treatment_and_control_denied_conf_int_lower_negative_flag == 1) ~ 1, TRUE ~ 0))
        
        
        ##############################################################################################################
        
        
        # get domain_output_table
        domain_output_table <- domain_tbl %>%
                mutate(treatment_fraud_p_weighted_output = case_when(treatment_fraud_diff_btw_domain_and_overall_significant == 1 ~ 
                                                                             str_c(as_percent(treatment_fraud_p_weighted, digits = 1), "*"), 
                                                                     TRUE ~ as_percent(treatment_fraud_p_weighted, digits = 1)),
                       treatment_fraud_p_weighted_output = case_when(treatment_fraud_suppressed_flag == 1 ~ "-",
                                                                     treatment_fraud_unreliable_flag == 1 ~ str_c(treatment_fraud_p_weighted_output, "X"),
                                                                     TRUE ~ treatment_fraud_p_weighted_output),
                       treatment_fraud_conf_int_output = case_when(treatment_fraud_suppressed_flag == 1 ~ "-", 
                                                                   TRUE ~ str_c(as_percent(treatment_fraud_conf_int_lower, digits = 1), " to ", 
                                                                                as_percent(treatment_fraud_conf_int_upper, digits = 1))),
                       treatment_denied_p_weighted_output = case_when(treatment_denied_diff_btw_domain_and_overall_significant == 1 ~ 
                                                                              str_c(as_percent(treatment_denied_p_weighted, digits = 1), "*"), 
                                                                      TRUE ~ as_percent(treatment_denied_p_weighted, digits = 1)),
                       treatment_denied_p_weighted_output = case_when(treatment_denied_suppressed_flag == 1 ~ "-",
                                                                      treatment_denied_unreliable_flag == 1 ~ str_c(treatment_denied_p_weighted_output, "X"),
                                                                      TRUE ~ treatment_denied_p_weighted_output),
                       treatment_denied_conf_int_output = case_when(treatment_denied_suppressed_flag == 1 ~ "-", 
                                                                    TRUE ~ str_c(as_percent(treatment_denied_conf_int_lower, digits = 1), " to ", 
                                                                                 as_percent(treatment_denied_conf_int_upper, digits = 1))),
                       control_denied_p_weighted_output = case_when(control_denied_diff_btw_domain_and_overall_significant == 1 ~ 
                                                                            str_c(as_percent(control_denied_p_weighted, digits = 1), "*"), 
                                                                    TRUE ~ as_percent(control_denied_p_weighted, digits = 1)),
                       control_denied_p_weighted_output = case_when(control_denied_suppressed_flag == 1 ~ "-",
                                                                    control_denied_unreliable_flag == 1 ~ str_c(control_denied_p_weighted_output, "X"),
                                                                    TRUE ~ control_denied_p_weighted_output),
                       control_denied_conf_int_output = case_when(control_denied_suppressed_flag == 1 ~ "-", 
                                                                  TRUE ~ str_c(as_percent(control_denied_conf_int_lower, digits = 1), " to ", 
                                                                               as_percent(control_denied_conf_int_upper, digits = 1))),
                       diff_btw_treatment_and_control_p_weighted_output = case_when(diff_btw_treatment_and_control_denied_p_weighted_suppressed_flag == 1 ~ "-",
                                                                                    diff_btw_treatment_and_control_denied_p_weighted_unreliable_flag == 1 ~ 
                                                                                            str_c(as_percent(diff_btw_treatment_and_control_denied_p_weighted, digits = 1), "X"),
                                                                                    TRUE ~ as_percent(diff_btw_treatment_and_control_denied_p_weighted, digits = 1)),
                       diff_btw_treatment_and_control_denied_conf_int_lower_output = case_when(diff_btw_treatment_and_control_denied_conf_int_lower_negative_flag == 1 ~ 
                                                                                                       "--", TRUE ~ as_percent(diff_btw_treatment_and_control_denied_conf_int_lower, digits = 1)),
                       diff_btw_treatment_and_control_denied_conf_int_upper_output = case_when(diff_btw_treatment_and_control_denied_conf_int_upper_negative_flag == 1 ~ 
                                                                                                       "--", TRUE ~ as_percent(diff_btw_treatment_and_control_denied_conf_int_upper, digits = 1)),
                       diff_btw_treatment_and_control_denied_conf_int_output = str_c(diff_btw_treatment_and_control_denied_conf_int_lower_output, " to ",
                                                                                     diff_btw_treatment_and_control_denied_conf_int_upper_output),
                       diff_btw_treatment_and_control_denied_conf_int_output = case_when(diff_btw_treatment_and_control_denied_conf_int_suppressed_flag == 1 ~
                                                                                                 "-", TRUE ~ diff_btw_treatment_and_control_denied_conf_int_output))
        
        
        ##########################################################################################################################
        
        
        # handle if return_expanded_table = TRUE
        if(return_expanded_table == TRUE) {
                
                return(domain_output_table)
        }

        
        ########################################################################################################################
        
        
        # handle if return_expanded_table = FALSE
        if(return_expanded_table == FALSE) {
                
                domain_output_table <- domain_output_table %>% select(treatment_fraud_domain_var_value, treatment_fraud_p_weighted_output, treatment_fraud_conf_int_output,
                                                              treatment_denied_p_weighted_output, treatment_denied_conf_int_output,
                                                              control_denied_p_weighted_output, control_denied_conf_int_output,
                                                              diff_btw_treatment_and_control_p_weighted_output,
                                                              diff_btw_treatment_and_control_denied_conf_int_output)
                
                return(domain_output_table)
        }
}


####################################################################################################


# test
# data <- sample_frame
# domain_var <- "ben_region_of_birth"

# note that get_ibfa_output_table should take as data input the sample_frame tbl loaded and cleaned as shown in create_africa_and_coa_tables_scratchpad.R,
# which is also saved in "C:/users/sjdevine/Work Folders/Desktop/personal_drive/RED/IBFA/analysis"

# sample_frame %>% get_ibfa_output_table(domain_var = "ben_region_of_birth", return_expanded_table = FALSE)
# sample_frame %>% get_ibfa_output_table(domain_var = "ben_region_of_birth", return_expanded_table = TRUE) %>% glimpse()
# 
# # inspect suppression
# sample_frame %>% get_ibfa_output_table(domain_var = "ben_region_of_birth", return_expanded_table = TRUE) %>%
#         select(treatment_fraud_domain_var_value, treatment_fraud_n_unweighted, treatment_fraud_x_unweighted, 
#                 treatment_fraud_p_weighted, treatment_fraud_standard_error,
#                treatment_fraud_coeff_of_variation, treatment_fraud_unreliable_flag, treatment_fraud_suppressed_flag)
# 
# sample_frame %>% get_ibfa_output_table(domain_var = "ben_region_of_birth", return_expanded_table = TRUE) %>%
#         select(treatment_fraud_domain_var_value, treatment_denied_n_unweighted, treatment_denied_x_unweighted, 
#                treatment_denied_p_weighted, treatment_denied_standard_error,
#                treatment_denied_coeff_of_variation, treatment_denied_unreliable_flag, treatment_denied_suppressed_flag)
# 
# sample_frame %>% get_ibfa_output_table(domain_var = "ben_region_of_birth", return_expanded_table = TRUE) %>%
#         select(treatment_fraud_domain_var_value, control_denied_n_unweighted, control_denied_x_unweighted, 
#                control_denied_p_weighted, control_denied_standard_error,
#                control_denied_coeff_of_variation, control_denied_unreliable_flag, control_denied_suppressed_flag)
