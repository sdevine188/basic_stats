# https://stats.idre.ucla.edu/r/faq/how-do-i-analyze-survey-data-with-a-stratified-random-sampling-with-allocation-to-strata-design/

library(survey)
library(foreign)

setwd("H:/R/basic_stats")

# stratified sample example
# mydata <- read.dta("https://stats.idre.ucla.edu/stat/examples/sop/jacktwn2.dta", convert.factors = FALSE)
mydata <- read.dta("jacktwn2.dta", convert.factors = FALSE)
head(mydata)

mydesign <- svydesign(id = ~1 , 
                      strata = ~stratum ,
                      data = mydata ,		
                      weight = ~sampwt ,
                      fpc = ~npop)
                
mydesign

sum(mydata$sampwt)
length(unique(mydata$stratum))

unique(mydata$twin)
mydata %>% filter(twin == 1) %>% summarize(twin_pct = n() / nrow(mydata))
svymean(~twin, mydesign)       

# margin of error for ibfa study
# https://onlinecourses.science.psu.edu/stat200/node/255
n <- 370
se <- sqrt(.3 * (1 - .3) / n)
margin <- se * 1.96
margin


.3*(1-.3) / (.05/1.96)^2
.5*(1-.5) / (.05/1.96)^2
.4*(1-.4) / (.05/1.96)^2



