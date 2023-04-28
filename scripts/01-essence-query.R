# Introduction #### 
# query essence api for
# total ed visits (the denominator)
# and the following numerators/denominators (for rate)
# suicide - all 
# suicide - by sex 
# suicide - by age 
# suicide - by hospital 
# suicide - by race 
# suicide - by ideation or attempt 
# where:
# Hospitals are: AZ-Banner Page; AZ-Flagstaff MC; AZ-Tuba City RHCC
# Patient is Coconino resident 
# CC & DD category: CDC Suicidal Ideation v1; CDC Suicide Attempt v1
# Has Been Emergency: Yes
# Admission Type Category: Emergency
# Discharge Dx available: Yes

# Setup #### 
# package libraries 
library(here)
library(tidyverse)
library(Rnssp)
library(janitor)
library(lubridate)

# set Rnnsp credentials
myProfile <- Credentials$new(
  username = askme("Enter your username: "),
  password = askme()
)

# Query Essence API for total ED visits ####
# total 
## count of total ED visits for each month ####
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=31Dec2022&geography=15919&geography=33622&geography=33177&percentParam=noPercent&datasource=va_hosp&admissionTypeCategory=e&startDate=1Dec2021&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=TimeSeries&ddAvailable=1&geographySystem=hospital&detector=nodetectordetector&removeZeroSeries=true&timeResolution=monthly&patientLoc=az_coconino&hasBeenE=1"

# query ESSENCE 
api_data_ed_total <- get_api_data(url)

# check variable names 
names(api_data_ed_total)

# create a tidy data table 
ed_total <- api_data_ed_total$timeSeriesData %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(
    date = ymd(date)
  )

# check work 
ggplot(data = ed_total) +
  geom_line(
    mapping = aes(
      x = date,
      y = count
    )
  )

# save to disk 
write_rds(
  x = ed_total,
  file = "data-tidy/essence-data-ed-visits-total.rds"
)

# Query Essence API for total suicide related ED visits #### 
## count of suicide related ED visits for each month ####
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=31Dec2022&geography=15919&geography=33622&geography=33177&percentParam=noPercent&datasource=va_hosp&admissionTypeCategory=e&startDate=1Dec2021&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=TimeSeries&ddAvailable=1&ccddCategory=cdc%20suicidal%20ideation%20v1&ccddCategory=cdc%20suicide%20attempt%20v1&geographySystem=hospital&detector=nodetectordetector&removeZeroSeries=true&timeResolution=monthly&patientLoc=az_coconino&hasBeenE=1"

# query ESSENCE 
api_data_ed_suicide <- get_api_data(url)

# check variable names 
names(api_data_ed_suicide)

# create a tidy data table 
ed_total_suicide <- api_data_ed_suicide$timeSeriesData %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(
    date = ymd(date)
  )

# check work 
ggplot(data = ed_total_suicide) +
  geom_line(
    mapping = aes(
      x = date,
      y = count
    )
  )

# save to disk 
write_rds(
  x = ed_total_suicide,
  file = "data-tidy/essence-data-ed-visits-total-suicide.rds"
)

## count of total ED visits for each month grouped by sex ####
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=31Dec2022&startMonth=January&graphOnly=true&geography=15919&geography=33622&geography=33177&percentParam=noPercent&datasource=va_hosp&admissionTypeCategory=e&startDate=1Dec2021&graphOptions=single&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=TimeSeries&ddAvailable=1&geographySystem=hospital&detector=nodetectordetector&removeZeroSeries=true&stratVal=sex&timeResolution=monthly&patientLoc=az_coconino&hasBeenE=1"

# query ESSENCE 
api_data_ed_suicide_total_sex <- get_api_data(url)

# check variable names 
names(api_data_ed_suicide_total_sex)

# create a tidy data table 
ed_suicide_total_by_sex <- api_data_ed_suicide_total_sex$timeSeriesData %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(
    date = ymd(date)
  )

# check work 
ggplot(data = ed_suicide_total_by_sex) +
  geom_line(
    mapping = aes(
      x = date,
      y = count,
      color = line_label,
      group = line_label
    )
  )

# save to disk 
write_rds(
  x = ed_suicide_total_by_sex,
  file = "data-tidy/essence-data-ed-visits-total-by-sex.rds"
)

## count of total ED visits for each month grouped by age ####
# note that age groups will need to be transformed to match target 
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=31Dec2022&startMonth=january&graphOnly=true&geography=15919&geography=33622&geography=33177&percentParam=noPercent&datasource=va_hosp&admissionTypeCategory=e&startDate=1Dec2021&graphOptions=single&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=TimeSeries&ddAvailable=1&geographySystem=hospital&detector=nodetectordetector&removeZeroSeries=true&stratVal=ageTenYear&timeResolution=monthly&patientLoc=az_coconino&hasBeenE=1"

# query ESSENCE 
api_data_ed_suicide_total_age <- get_api_data(url)

# check variable names 
names(api_data_ed_suicide_total_age)

# create a tidy data table 
ed_suicide_total_by_age <- api_data_ed_suicide_total_age$timeSeriesData %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(
    date = ymd(date)
  )

# check work 
ggplot(data = ed_suicide_total_by_age) +
  geom_line(
    mapping = aes(
      x = date,
      y = count,
      color = line_label,
      group = line_label
    )
  )

# save to disk 
write_rds(
  x = ed_suicide_total_by_age,
  file = "data-tidy/essence-data-ed-visits-total-by-age.rds"
)

## count of total ED visits for each month grouped by hospital ####
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=31Dec2022&startMonth=january&graphOnly=true&geography=15919&geography=33622&geography=33177&percentParam=noPercent&datasource=va_hosp&admissionTypeCategory=e&startDate=1Dec2021&graphOptions=single&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=TimeSeries&ddAvailable=1&geographySystem=hospital&detector=nodetectordetector&removeZeroSeries=true&stratVal=hospitalGrouping&timeResolution=monthly&patientLoc=az_coconino&hasBeenE=1"

# query ESSENCE 
api_data_ed_suicide_total_hosp <- get_api_data(url)

# check variable names 
names(api_data_ed_suicide_total_hosp)

# create a tidy data table 
ed_suicide_total_by_hosp <- api_data_ed_suicide_total_hosp$timeSeriesData %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(
    date = ymd(date)
  )

# check work 
ggplot(data = ed_suicide_total_by_hosp) +
  geom_line(
    mapping = aes(
      x = date,
      y = count,
      color = line_label,
      group = line_label
    )
  )

# save to disk 
write_rds(
  x = ed_suicide_total_by_hosp,
  file = "data-tidy/essence-data-ed-visits-total-by-hosp.rds"
)

## count of total ED visits for each month grouped by race and ethnicity ####
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=31Dec2022&startMonth=january&graphOnly=true&geography=15919&geography=33622&geography=33177&percentParam=noPercent&datasource=va_hosp&admissionTypeCategory=e&startDate=1Dec2021&graphOptions=single&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=TimeSeries&ddAvailable=1&geographySystem=hospital&detector=nodetectordetector&removeZeroSeries=true&stratVal=cRaceEthBroad&timeResolution=monthly&patientLoc=az_coconino&hasBeenE=1"

# query ESSENCE 
api_data_ed_suicide_total_race <- get_api_data(url)

# check variable names 
names(api_data_ed_suicide_total_race)

# create a tidy data table 
ed_suicide_total_by_race <- api_data_ed_suicide_total_race$timeSeriesData %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(
    date = ymd(date)
  )

# check work 
ggplot(data = ed_suicide_total_by_race) +
  geom_line(
    mapping = aes(
      x = date,
      y = count,
      color = line_label,
      group = line_label
    )
  )

# save to disk 
write_rds(
  x = ed_suicide_total_by_race,
  file = "data-tidy/essence-data-ed-visits-total-by-race.rds"
)

## count of suicide related ED visits for each month grouped by CCDD category ####
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=31Dec2022&startMonth=january&graphOnly=true&geography=15919&geography=33622&geography=33177&percentParam=noPercent&datasource=va_hosp&admissionTypeCategory=e&startDate=1Dec2021&graphOptions=single&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=TimeSeries&ddAvailable=1&ccddCategory=cdc%20suicidal%20ideation%20v1&ccddCategory=cdc%20suicide%20attempt%20v1&geographySystem=hospital&detector=nodetectordetector&removeZeroSeries=true&stratVal=ccddCategory&timeResolution=monthly&patientLoc=az_coconino&hasBeenE=1"

# query ESSENCE 
api_data_ed_suicide_total_ccdd <- get_api_data(url)

# check variable names 
names(api_data_ed_suicide_total_ccdd)

# create a tidy data table 
ed_suicide_total_by_ccdd <- api_data_ed_suicide_total_ccdd$timeSeriesData %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(
    date = ymd(date)
  )

# check work 
ggplot(data = ed_suicide_total_by_ccdd) +
  geom_line(
    mapping = aes(
      x = date,
      y = count,
      color = line_label,
      group = line_label
    )
  )

# save to disk 
write_rds(
  x = ed_suicide_total_by_ccdd,
  file = "data-tidy/essence-data-ed-visits-total-by-ccdd.rds"
)

# # total count of hospitalizations for each month of time period 
# # data quality time period: hasbeen = yes
# # stratified (group) by hospital 
# url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=31Dec2022&startMonth=january&dqCOVHasBeenEOneYearOperator=eq&dqCOVHasBeenEOneYear=Yes&graphOnly=true&geography=AZ_Coconino&percentParam=noPercent&datasource=va_er&startDate=1Jan2021&graphOptions=single&erFacility=15919&erFacility=33622&erFacility=33177&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=TimeSeries&geographySystem=region&detector=nodetectordetector&removeZeroSeries=true&stratVal=erFacility&timeResolution=monthly"
# 
# # query ESSENCE 
# api_data_total_by_hosp <- get_api_data(url)
# 
# names(api_data_total_by_hosp)
# 
# api_data_total_by_hosp <- api_data_total_by_hosp$timeSeriesData %>%
#   clean_names() %>%
#   as_tibble()
# 
# # # total count of hospitalizations for each month of time period 
# # data quality time period: hasbeen = yes
# # stratified (group) by sex
# url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=31Dec2022&startMonth=january&dqCOVHasBeenEOneYearOperator=eq&dqCOVHasBeenEOneYear=Yes&graphOnly=true&geography=AZ_Coconino&percentParam=noPercent&datasource=va_er&startDate=1Jan2021&graphOptions=single&erFacility=15919&erFacility=33622&erFacility=33177&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=TimeSeries&geographySystem=region&detector=nodetectordetector&removeZeroSeries=true&stratVal=sex&timeResolution=monthly"
# 
# # query ESSENCE 
# api_data_total_by_sex <- get_api_data(url)
# 
# names(api_data_total_by_sex)
# 
# api_data_total_by_sex <- api_data_total_by_sex$timeSeriesData %>%
#   clean_names() %>%
#   as_tibble()
# 
# # # total count of hospitalizations for each month of time period 
# # data quality time period: hasbeen = yes
# # stratified (group) by age group
# url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=31Dec2022&startMonth=january&dqCOVHasBeenEOneYearOperator=eq&dqCOVHasBeenEOneYear=Yes&graphOnly=true&geography=AZ_Coconino&percentParam=noPercent&datasource=va_er&startDate=1Jan2021&graphOptions=single&erFacility=15919&erFacility=33622&erFacility=33177&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=TimeSeries&geographySystem=region&detector=nodetectordetector&removeZeroSeries=true&stratVal=age&timeResolution=monthly"
# 
# # query ESSENCE 
# api_data_total_by_age <- get_api_data(url)
# 
# names(api_data_total_by_age)
# 
# api_data_total_by_age <- api_data_total_by_age$timeSeriesData %>%
#   clean_names() %>%
#   as_tibble()

# Weekly count of suicide related ED visits ####
## JSON URL from ESSENCE API
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?endDate=31Dec2022&startMonth=january&geography=15919&geography=33622&geography=33177&percentParam=noPercent&datasource=va_hosp&admissionTypeCategory=e&startDate=26Dec2021&graphOptions=single&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=TimeSeries&ddAvailable=1&ccddCategory=cdc%20suicidal%20ideation%20v1&ccddCategory=cdc%20suicide%20attempt%20v1&geographySystem=hospital&detector=nodetectordetector&removeZeroSeries=true&timeResolution=weekly&patientLoc=az_coconino&hasBeenE=1"

## Get Data from ESSENCE
api_data_suicide_total_week <- get_api_data(url) # or api_data <- myProfile$get_api_data(url)

names(api_data_suicide_total_week)

# tidy
ed_suicide_total_week_by_ccdd <- api_data_suicide_total_week$timeSeriesData  %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(
    date = ymd(date)
  )

# check work with plot
ed_suicide_total_week_by_ccdd %>%
  mutate(
    date = as.Date(date)
  ) %>%
  filter(date <= as.Date(Sys.Date()-3)) %>%
  ggplot(mapping = aes(
    x = date,
    y = count
  )) +
  geom_line() +
  geom_point() +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b-%y"
  ) +
  scale_color_manual(values = c("#C658E8", "#5886E8")) +
  ylim(0,NA) +
  labs(
    title = str_c("Suicide Related Hospitalization, Weekly Count of"),
    subtitle = "Among Coconino County Residents",
    x = "Date",
    y = "Count",
    color = "CC DD Category",
    caption = str_wrap(
      string = "Each point represents one week. Data source: CDC NSSP BioSense Platform reporting data from AZ-Banner Page Hosp, AZ-Flagstaff MC, and AZ-Tuba City RHC",
      width = 120
    )
  ) +
  theme_classic()

# save to disk 
write_rds(
  x = ed_suicide_total_week_by_ccdd,
  file = "data-tidy/essence-data-ed-visits-total-suicide-week-by-ccdd.rds"
)

# data details ####
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?endDate=31Dec22&geography=15919&geography=33622&geography=33177&percentParam=noPercent&datasource=va_hosp&admissionTypeCategory=e&startDate=26Dec21&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=TimeSeries&ddAvailable=1&ccddCategory=cdc%20suicidal%20ideation%20v1&ccddCategory=cdc%20suicide%20attempt%20v1&geographySystem=hospital&detector=nodetectordetector&removeZeroSeries=true&timeResolution=weekly&patientLoc=az_coconino&hasBeenE=1"

api_data_details <- get_api_data(url)

## Inspect data object structure
names(api_data_details)

## Get a glimpse of the pulled dataset
glimpse(api_data_details$dataDetails)

# tidy
ed_suicide_data_details <- api_data_details$dataDetails %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(
    date = mdy(date)
  )

glimpse(ed_suicide_data_details)

essence_df <- ed_suicide_data_details %>%
  distinct(
    date,
    hospital_name,
    sex,
    age,
    hospital,
    medical_record_number,
    .keep_all = TRUE
  ) %>%
  select(
    date,
    week_year,
    month_year,
    year,
    hospital_name,
    region,
    chief_complaint_orig,
    chief_complaint_parsed,
    category_flat,
    sub_category_flat,
    discharge_diagnosis,
    sex,
    age,
    age_group,
    c_ethnicity,
    c_race,
    c_race_c_eth_combined_narrow,
    c_race_c_eth_combined_broad,
    admit_date_time,
    admit_reason_code,
    admit_reason_combo,
    ccdd_category
  ) 

write_rds(
  x = essence_df,
  file = "data-raw/essence-data-details.rds"
)

