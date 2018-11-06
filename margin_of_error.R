# http://www.r-tutor.com/elementary-statistics/interval-estimation/interval-estimate-population-proportion

library(MASS) 

setwd("H:/R/basic_stats")


gender.response <- na.omit(survey$Sex) 
head(gender.response)

n <- length(gender.response)    # valid responses count 
n

number_successes <- sum(gender.response == "Female") 
number_successes

sample_proportion <- number_successes / n
sample_proportion

standard_error = sqrt(sample_proportion * (1 - sample_proportion) / n)
standard_error

qnorm(.975)
margin_of_error <- qnorm(.975) * standard_error
margin_of_error

confidence_interval <- sample_proportion + c(-margin_of_error, margin_of_error) 
confidence_interval

prop.test(number_successes, n)


#####


# http://stattrek.com/estimation/confidence-interval-proportion.aspx?Tutorial=AP

# example of margin of error for proportion
z_score <- qnorm(.995)
se <- sqrt(.4 * (1 - .4) / 1600)
margin_of_error <- se * z_score 
.4 + c(-margin_of_error, margin_of_error)


####################################################################################


# http://www.stat.cmu.edu/~fienberg/Stat36-303-03/Handouts/StratificationNotes-03.pdf

# example with stratification

set.seed(123)

urn1 <- c(rep(1, 1), rep(0, 9))
mean(urn1)
urn2 <- c(rep(1, 2), rep(0, 8))
urn3 <- c(rep(1, 3), rep(0, 7))

urns <- data.frame(fraud = c(urn1, urn2, urn3))
urns$strat <- c(rep(1, 10), rep(2, 10), rep(3, 10))
glimpse(urns)

strata_stats <- urns %>% group_by(strat) %>% summarize(n_stratum = n(), n_sample = nrow(urns), fraud_sum = sum(fraud), 
                       fraud_rate_stratum = fraud_sum / n_stratum, 
                       rate_times_n_stratum = fraud_rate_stratum * n_stratum) %>%
                        mutate(pop_strata_count = c(100, 100, 100), 
                              var_stratum = ((fraud_rate_stratum * (1 - fraud_rate_stratum)) / n_stratum) * 
                                      (pop_strata_count - n_stratum) / (pop_strata_count - 1),
                       stratum_share_sample_sq = (n_stratum / n_sample)^2, 
                       var_times_stratum_share = var_stratum * stratum_share_sample_sq) %>% data.frame(.)
strata_stats

var <- strata_stats %>% summarize(overall_var = sum(var_times_stratum_share))
var
sqrt(var)
sqrt(var) * 1.96








