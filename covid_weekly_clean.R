rm(list=ls())

library(tidyverse)
library(haven)
library(sjmisc)
library(RColorBrewer)

filter <- dplyr::filter
colors <- brewer.pal(3,'Set1')

#####
# Functions
#####

strip_n_flip <- function(x){
  x[x == 999] <- NA
  x <- abs(x - 5)
  return(x)
}

reorder <- function(x){
  newvar <- NA
  newvar[x == 1] <- 2
  newvar[x == 3] <- 1
  newvar[x == 2] <- 0
  return(newvar)
}

barplot_pid3 <- function(df, x,outcome, group=T){
  x <- enquo(x)
  if(group==T){
    out <- df %>%
      group_by(wave, pid3) %>%
      summarise(y = weighted.mean(!!x, na.rm=T, w=weight)) %>%
      mutate(Outcome=outcome)
  } else {
    out <- df %>%
      group_by(pid3) %>%
      summarise(y = weighted.mean(!!x, na.rm=T, w=weight)) %>%
      mutate(Outcome=outcome)
  }
  return(out)
}
barplot_pid3_worry <- function(df, x,cut=2,outcome, group=T){
  x <- enquo(x)
  if(group==T){
    out <- df %>%
      group_by(wave, pid3) %>%
      summarise(y = weighted.mean(!!x%in% c(4), na.rm=T, w=weight)) %>%
      mutate(Outcome=outcome)
  } else {
    out <- df %>%
      group_by(pid3) %>%
      summarise(y = weighted.mean(!!x %in% c(4), na.rm=T, w=weight)) %>%
      mutate(Outcome=outcome)
  }
  return(out)
}
barplot_pid3_incomechange <- function(df, x,cut=2,outcome, group=T){
  x <- enquo(x)
  if(group==T){
    out <- df %>%
      group_by(wave, pid3) %>%
      summarise(y = weighted.mean(!!x%in% c(3,4), na.rm=T, w=weight)) %>%
      mutate(Outcome=outcome)
  } else {
    out <- df %>%
      group_by(pid3) %>%
      summarise(y = weighted.mean(!!x %in% c(3,4), na.rm=T, w=weight)) %>%
      mutate(Outcome=outcome)
  }
  return(out)
}

barplot_pid3_trumpapprove <- function(df, x,cut=2,outcome, group=T){
  x <- enquo(x)
  if(group==T){
    out <- df %>%
      group_by(wave, pid3) %>%
      summarise(y = weighted.mean(!!x%in% c(3,4), na.rm=T, w=weight)) %>%
      mutate(Outcome=outcome)
  } else {
    out <- df %>%
      group_by(pid3) %>%
      summarise(y = weighted.mean(!!x %in% c(3,4), na.rm=T, w=weight)) %>%
      mutate(Outcome=outcome)
  }
  return(out)
}



barplot_pid3_sick <- function(df, x,outcome, group=T){
  x <- enquo(x)
  if(group==T){
    out <- df %>%
      group_by(wave, pid3) %>%
      summarise(y = weighted.mean(!!x == 1, na.rm=T, w=weight)) %>%
      mutate(Outcome=outcome)
  } else {
    out <- df %>%
      group_by(pid3) %>%
      summarise(y = weighted.mean(!!x == 1, na.rm=T, w=weight)) %>%
      mutate(Outcome=outcome)
  }
  return(out)
}

barplot_pid3_maybe_sick <- function(df, x,outcome, group=T){
  x <- enquo(x)
  if(group==T){
    out <- df %>%
      group_by(wave, pid3) %>%
      summarise(y = weighted.mean(!!x %in% c(1,3), na.rm=T, w=weight)) %>%
      mutate(Outcome=outcome)
  } else {
    out <- df %>%
      group_by(pid3) %>%
      summarise(y = weighted.mean(!!x %in% c(1,3), na.rm=T, w=weight)) %>%
      mutate(Outcome=outcome)
  }
  return(out)
}

####
# Read in data and cleaning -----
####


# read in stacked file
df <- read_dta('/Users/tylerreny/Dropbox/_nationscape/Democracy Tracker - UCLA team/outputs/stacked.dta') %>% 
  as.data.frame()

# read in most recent wave
new <- read_dta('/Users/tylerreny/Dropbox/_nationscape/Democracy Tracker - UCLA team/outputs/ns20200409/ns20200409.dta')
df <- add_rows(df, new) #from sjmisc package, preserves labelling
df$wave[is.na(df$wave)] <-  'ns20200416'
df$wave <- as.factor(df$wave)
levels(df$wave) <- 1:length(levels(df$wave))
df$wave <- as.numeric(df$wave)

# pull number of cases from NYTimes Github 
countydata <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')
countydata = countydata %>%
  group_by(fips) %>%
  summarise(cases_county=sum(cases), 
            deaths_county=sum(deaths))

# pull county demographics and join to cases by FIPS
countypop <- read_csv('https://raw.githubusercontent.com/tylerreny/county_demos/master/county_pop_1418.csv')
countypop$fips <- str_pad(countypop$fips, 5, 'left', '0')
countydata <- left_join(countydata,countypop)
countydata$cases_county <- countydata$cases_county/(countydata$total_pop) # per capita cases
countydata$deaths_county <- countydata$deaths_county/(countydata$total_pop) # per capita deaths
countydata <- countydata %>% 
  dplyr::select(fips, cases_county, deaths_county)

# download crosswalk to merge zip to county FIPS
crosswalk <- read_csv('https://raw.githubusercontent.com/tylerreny/county_demos/master/county_zip_crosswalk_122019.csv')

# when you have zips that have more than one entry because they cross state counties, select just
# the county with a larger percent of land area of the zip in it
to_join <- crosswalk %>%
  group_by(ZIP) %>%
  summarise(TOT_RATIO = max(TOT_RATIO))

# unique identifier for joining across both datasets
crosswalk$tempjoin <- paste0(crosswalk$ZIP, crosswalk$TOT_RATIO)
to_join$tempjoin <- paste0(to_join$ZIP, to_join$TOT_RATIO)

# subset to just those cases that match on the unique identifier
crosswalk <- crosswalk %>%
  filter(tempjoin %in% to_join$tempjoin)
crosswalk <- crosswalk %>% 
  dplyr::select(COUNTY, ZIP) %>% 
  rename(county=COUNTY, zip=ZIP)
crosswalk$zip <- str_pad(crosswalk$zip, 5, 'left',0)

# join back to survey
df <- left_join(df, crosswalk)

# fix fips
df$county <- str_pad(df$county, 5, 'left','0')

# join in county deaths data
df <- left_join(df, countydata, by=c('county'='fips'))

####
# Recoding vars ----
####

# recode survey vars
table(df$extra_prescriptions)
df <- df %>%
  mutate(extra_prescriptions2 = case_when(
    extra_prescriptions == 999 ~ 0, # if peopel don't know if they take prescription drugs we're going to say they dont
    TRUE ~ extra_prescriptions),
    extra_covid_cancel_meet = strip_n_flip(extra_covid_cancel_meet),
    extra_covid_close_business = strip_n_flip(extra_covid_close_business),
    extra_covid_close_schools  = strip_n_flip(extra_covid_close_schools),  
    extra_covid_work_home = strip_n_flip( extra_covid_work_home),          
    extra_covid_restrict_travel = strip_n_flip(extra_covid_restrict_travel),     
    extra_covid_restrict_home  = strip_n_flip(extra_covid_restrict_home), 
    extra_covid_social_distance = strip_n_flip(extra_covid_social_distance),     
    extra_covid_delay_elections  = strip_n_flip(extra_covid_delay_elections),    
    extra_covid_testing = strip_n_flip(extra_covid_testing),            
    extra_covid_gov_travel   = strip_n_flip(extra_covid_gov_travel),      
    extra_covid_increase_gov_unemp  = strip_n_flip(extra_covid_increase_gov_unemp), 
    extra_covid_gov_health  = strip_n_flip(extra_covid_gov_health),         
    extra_covid_gov_sick_leave  = strip_n_flip(extra_covid_gov_sick_leave),   
    extra_covid_gov_interest  = strip_n_flip(extra_covid_gov_interest),      
    extra_covid_gov_cash    = strip_n_flip(extra_covid_gov_cash),         
    extra_corona_concern = abs(extra_corona_concern-5),   
    extra_covid_wash = ifelse(extra_covid_wash==1,1,0),                
    extra_covid_cancel_travel = ifelse(extra_covid_cancel_travel==1,1,0),              
    extra_covid_stock_goods = ifelse(extra_covid_stock_goods==1,1,0),                       
    extra_covid_visit_family = ifelse(extra_covid_visit_family==1,1,0),                      
    extra_covid_quarantine = ifelse(extra_covid_quarantine==1,1,0),                        
    extra_covid_hospital = ifelse(extra_covid_hospital==1,1,0),
    extra_trump_corona <- abs(extra_trump_corona-5),
    pid3 = case_when(
      pid7 < 3 ~ 'Democrat',
      pid7 %in% c(3,4,5) ~ 'Independent',
      pid7 > 5 ~ 'Republican'
      )
    )


