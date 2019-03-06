# note the average proportion across individuals is different from the overal population proportion
# https://www.ncbi.nlm.nih.gov/pubmed/2723291



tbl <- tibble(anumber = c(1, 2, 3, 4, 5), number_of_drug_arrests_per_anum = c(1, 1, 1, 1, 1), total_number_of_arrests_per_anum = c(1, 1, 1, 1, 1000))
tbl

tbl %>% mutate(drug_arrests_as_pct_of_all_arrests_per_anum = number_of_drug_arrests_per_anum / total_number_of_arrests_per_anum)

tbl %>% mutate(drug_arrests_as_pct_of_all_arrests_per_anum = number_of_drug_arrests_per_anum / total_number_of_arrests_per_anum) %>% 
        summarize(avg_number_of_drug_arrests_per_anum = mean(number_of_drug_arrests_per_anum), 
                  avg_total_number_of_arrests_per_anum = mean(total_number_of_arrests_per_anum),
                avg_drug_arrests_as_pct_of_all_arrests_per_anum = mean(drug_arrests_as_pct_of_all_arrests_per_anum))

tbl %>% mutate(drug_arrests_as_pct_of_all_arrests_per_anum = number_of_drug_arrests_per_anum / total_number_of_arrests_per_anum) %>% 
        summarize(avg_number_of_drug_arrests_per_anum = mean(number_of_drug_arrests_per_anum), 
                  avg_total_number_of_arrests_per_anum = mean(total_number_of_arrests_per_anum),
                  avg_drug_arrests_as_pct_of_all_arrests_per_anum = mean(drug_arrests_as_pct_of_all_arrests_per_anum)) %>%
        mutate(avg_drug_arrests_as_pct_of_all_arrests_per_anum_based_on_population_averages = avg_number_of_drug_arrests_per_anum / avg_total_number_of_arrests_per_anum) %>% 
        data.frame()


###################################################################


test <- charges_I918 %>%
        mutate(charge_type_group = case_when(anum %in% unable_to_extract_charges_only_anumbers$anum ~ "Unable to extract charges", 
                                             TRUE ~ charge_type_group)) %>%
        filter(!charge_type_group %in% c("keyline_does_not_list_specific_charge", 
                                         "Unable to extract charge", "Undefined")) %>% 
        left_join(., array %>% filter(principal == 1), by = c("anum" = "BEN_A_NUMBER")) %>%
        count(anum, keydate, charge_type_group, approved, denied, pending) %>% select(-n) %>% mutate(arrest_w_at_least_one_charge = 1) %>%
        group_by(anum, charge_type_group) %>% mutate(total_arrests_for_charge_type_group_per_anumber = sum(arrest_w_at_least_one_charge)) %>% ungroup() %>%
        select(-arrest_w_at_least_one_charge) %>% count(anum, charge_type_group, approved, denied, pending, total_arrests_for_charge_type_group_per_anumber) %>%
        select(-n) %>% select(-c(approved, denied, pending)) %>% slice(1:10)
test

test %>% group_by(anum) %>% 
        mutate(total_arrests_per_anumber = sum(total_arrests_for_charge_type_group_per_anumber), 
               proportion_of_all_arrests_for_charge_type_group_per_anumber = total_arrests_for_charge_type_group_per_anumber / total_arrests_per_anumber,
               total_different_charge_type_groups_per_anumber = n()) %>% 
        ungroup() %>% data.frame() %>%
        group_by(charge_type_group) %>%
        summarize(number_of_anumbers_with_offense = n(), 
                  avg_number_of_arrests_for_charge_type_group = mean(total_arrests_for_charge_type_group_per_anumber),
                  avg_total_number_of_arrests = mean(total_arrests_per_anumber),
                  avg_proportion_of_all_arrests_for_charge_type_group = mean(proportion_of_all_arrests_for_charge_type_group_per_anumber),
                  avg_number_of_different_charge_type_groups_for_charge_type_group = mean(total_different_charge_type_groups_per_anumber)) %>%
        data.frame() 



# avg arrests for charge
mean(c(1, 6, 5))

# avg total arrests
mean(c(2, 8, 14))

# proportion based on population averages
4/8

# proportion based on population sums (same as proportion based on population averages)
sum(1, 6, 5) / sum(2, 8, 14)

# individual proportions
c(1/2, 6/8, 5/14)

# avg of individual proportions 
mean(c(1/2, 6/8, 5/14))


####################################################################


