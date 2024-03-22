library(tidyverse)
library(data.table)
options(scipen=999)

# read in data
chicago_data <- read_csv("Chicago Health Atlas Data Download - Community areas.csv")

# removing unnecessary rows
chicago_data <- chicago_data[-c(1:4), -c(1)]

# renaming columns
colnames(chicago_data) <- c("neighborhood", "geoid", "population", "latitude", "longitude",
                            "perceived_violence", "svi", "walkability", 
                            "eviction_filing_rate", "eviction_rate", "perceived_sidewalk_qual", 
                            "internet_access", "easy_to_transit", "rent_burdened", 
                            "tree_canopy", "litter", "low_food_access", "adult_loneliness", 
                            "mental_health_need", "disability", "overall_health", 
                            "perceived_safety", "trust_law_enforce", "trust_local_gov", 
                            "high_school_grad", "college_grad", "preschool_enroll", 
                            "unemployment", "community_belonging", "single_parent", "poverty", 
                            "percent_snap", "percent_eligible_no_snap", "public_assistance", 
                            "foreign_born", "limited_english_proficiency", "nonhispanic_white", 
                            "firearm_homicide", "grade9_ed", "roads_rail_airport_proximity_index", 
                            "food_insecurity", "medicaid", "noncitizen", "veteran")

# converting columns to numeric
chicago_data <- chicago_data %>% 
  mutate_at(c(3:44), as.numeric)

# creating a variable to group by area of Chicago
loop <- list("Loop")
north <- list("Near North Side", "Lincoln Park", "Lincoln Square", "North Center", 
              "Lake View", "Uptown", "Edgewater", "Rogers Park", "West Ridge", 
              "Edison Park", "Norwood Park", "Jefferson Park", "Forest Glen", "North Park", 
              "O'Hare")
west <- list("West Town", "Humboldt Park", "Lower West Side", "Albany Park", "Portage Park", 
             "Irving Park", "Avondale", "Logan Square", "Dunning", "Montclare", 
             "Belmont Cragin", "Hermosa", "Austin", "West Garfield Park", "East Garfield Park", 
             "Near West Side", "North Lawndale", "South Lawndale", "Garfield Ridge", 
             "Archer Heights", "Brighton Park", "McKinley Park", "New City", "West Elsdon", 
             "Gage Park", "Clearing", "West Lawn", "Chicago Lawn")
south <- list("Kenwood", "Hyde Park", "Bridgeport", "Armour Square", "South Shore", 
              "Beverly", "Near South Side", "Douglas", "Oakland", "Fuller Park", 
              "Grand Boulevard", "Washington Park", "Woodlawn", "Chatham", "Avalon Park",
              "South Chicago", "Burnside", "Calumet Heights", "Roseland", "Pullman", 
              "South Deering", "East Side", "West Pullman", "Riverdale", "Hegewisch", 
              "West Englewood", "Englewood", "Greater Grand Crossing", "Ashburn", 
              "Auburn Gresham", "Washington Heights", "Morgan Park", "Mount Greenwood")

chicago_data <- chicago_data %>% 
  mutate(
    chicago_area = case_when(
      neighborhood %in% loop ~ "loop",
      neighborhood %in% north ~ "north",
      neighborhood %in% west ~ "west",
      neighborhood %in% south ~ "south"))

chicago_data <- chicago_data %>% 
  mutate(
    'facet_area' = case_when(
      chicago_area == 'north' ~ 'The North Side',
      chicago_area == 'west' ~ 'The West Side',
      chicago_area == 'south' ~ 'The South Side',
      .default = NA)
  )

# read in national data
acs_demographics <- read_csv("national data/ACSDP1Y2022.DP02-2024-03-02T185128.csv")
acs_benefits <- read_csv("national data/ACSDP1Y2022.DP03-2024-03-02T185055.csv")
acs_race <- read_csv("national data/ACSDP1Y2022.DP05-2024-03-02T185027.csv")
acs_poverty <- read_csv("national data/ACSST1Y2022.S1701-2024-03-02T185232.csv")
medicaid <- read_csv("national data/data.csv")
food_security <- readxl::read_xlsx("national data/household_pie2022.xlsx")

acs_demographics <- acs_demographics %>% 
  slice(8, 12, 75, 76, 79, 82, 108, 110, 131) %>% 
  rename(c(statistic = `Label (Grouping)`, us_percent = `United States!!Percent`, 
           us_percent_me = `United States!!Percent Margin of Error`, 
           us_count = `United States!!Estimate`, us_count_me = `United States!!Margin of Error`)) %>% 
  mutate(across(c("us_count_me", "us_percent", "us_percent_me"), ~na_if( ., "(X)")),
         across(c("us_count_me", "us_percent", "us_percent_me"), ~str_replace( ., "%", "")),
         across(c("us_count_me", "us_percent", "us_percent_me"), ~str_replace( ., "±", "")),
         across(c("us_count_me", "us_percent", "us_percent_me"), ~str_replace( ., ",", "")),
         across(c("us_count_me", "us_percent", "us_percent_me"), ~str_replace( ., ",", "")))

acs_benefits <- acs_benefits %>% 
  slice(10, 78, 80) %>% 
  rename(c(statistic = `Label (Grouping)`, us_percent = `United States!!Percent`, 
           us_percent_me = `United States!!Percent Margin of Error`, 
           us_count = `United States!!Estimate`, us_count_me = `United States!!Margin of Error`)) %>% 
  mutate(across(.cols = everything(), ~na_if( ., "(X)")),
         across(.cols = everything(), ~str_replace( ., "%", "")),
         across(.cols = everything(), ~str_replace( ., "±", "")),
         across(.cols = everything(), ~str_replace( ., ",", "")),
         across(.cols = everything(), ~str_replace( ., ",", "")))

acs_race <- acs_race %>%  
  slice(2, 83) %>% 
  rename(statistic = `Label (Grouping)`, us_percent = `United States!!Percent`, 
         us_percent_me = `United States!!Percent Margin of Error`, us_count = `United States!!Estimate`, 
         us_count_me = `United States!!Margin of Error`) %>% 
  mutate(across(c("us_count_me", "us_percent", "us_percent_me"), ~na_if( ., "(X)")),
         across(c("us_count_me", "us_percent", "us_percent_me"), ~na_if( ., "*****")),
         across(c("us_count_me", "us_percent", "us_percent_me"), ~str_replace( ., "%", "")),
         across(c("us_count_me", "us_percent", "us_percent_me"), ~str_replace( ., "±", "")),
         across(c("us_count_me", "us_percent", "us_percent_me"), ~str_replace( ., ",", "")),
         across(c("us_count_me", "us_percent", "us_percent_me"), ~str_replace( ., ",", "")))

acs_poverty <- acs_poverty %>% 
  select(`Label (Grouping)`, `United States!!Below poverty level!!Estimate`, 
         `United States!!Below poverty level!!Margin of Error`, 
         `United States!!Percent below poverty level!!Estimate`, 
         `United States!!Percent below poverty level!!Margin of Error`) %>% 
  slice(1) %>% 
  rename(statistic = `Label (Grouping)`, us_percent = `United States!!Percent below poverty level!!Estimate`, 
         us_percent_me = `United States!!Percent below poverty level!!Margin of Error`, 
         us_count = `United States!!Below poverty level!!Estimate`, 
         us_count_me = `United States!!Below poverty level!!Margin of Error`) %>% 
  mutate(across(.cols = everything(), ~na_if( ., "(X)")),
         across(.cols = everything(), ~str_replace( ., "%", "")),
         across(.cols = everything(), ~str_replace( ., "±", "")),
         across(.cols = everything(), ~str_replace( ., ",", "")),
         across(.cols = everything(), ~str_replace( ., ",", "")))

numeric_columns <- c("us_percent", "us_percent_me", "us_count", "us_count_me")

acs_benefits[numeric_columns] <- lapply(acs_benefits[numeric_columns], as.numeric)
acs_demographics[numeric_columns] <- lapply(acs_demographics[numeric_columns], as.numeric)
acs_poverty[numeric_columns] <- lapply(acs_poverty[numeric_columns], as.numeric)
acs_race[numeric_columns] <- lapply(acs_race[numeric_columns], as.numeric)

merged_acs <- bind_rows(acs_benefits, acs_demographics, acs_poverty, acs_race)

# Transpose the data
transposed_acs <- as.data.frame(t(merged_acs[, -1]))

# Add a column for the original column names
transposed_acs <- cbind(measure = colnames(merged_acs)[-1], transposed_acs)

# Set the column names
colnames(transposed_acs)[-16] <- merged_acs$statistic

new_colnames <- c("measure", "unemployment", "public_assistance", "percent_snap", "single_parent1", 
             "single_parent2", "high_school_grad", "college_grad", "veteran", "disability", "foreignborn", 
             "non_citizen", "limited_english_proficiency", "poverty", "population", "nonhispanic_white")

colnames(transposed_acs) <- new_colnames

row.names(transposed_acs) <- NULL

total_population <- transposed_acs %>% 
  filter(measure == "us_count") %>% 
  pull(population)

single_parent1_count <- transposed_acs %>% 
  filter(measure == "us_count") %>% 
  pull(single_parent1)

single_parent2_count <- transposed_acs %>% 
  filter(measure == "us_count") %>% 
  pull(single_parent2)

noncitizen_count <- transposed_acs %>% 
  filter(measure == "us_count") %>% 
  pull(non_citizen)

noncitizen_count_me <- transposed_acs %>% 
  filter(measure == "us_count_me") %>% 
  pull(non_citizen)

foreignborn_count <- transposed_acs %>% 
  filter(measure == "us_count") %>% 
  pull(foreignborn)

single_parent_total <- single_parent1_count + single_parent2_count

single_parent_percent <- ((single_parent1_count + single_parent2_count) / total_population) * 100

noncitizen_percent <- (noncitizen_count / total_population) * 100

foreignborn_percent <- (foreignborn_count / total_population) * 100

medicaid_data <- medicaid %>% 
  filter(final_report == "Y") %>% 
  select(state_abbreviation, state_name, total_medicaid_enrollment, report_date) %>% 
  filter(report_date == "12/01/2022") 

total_enrollment <- medicaid_data %>% 
  summarise(total = sum(total_medicaid_enrollment)) %>% 
  pull(total)

medicaid_percent <- (total_enrollment / total_population) * 100

food_insecurity_rate <- food_security %>% 
  slice(2, 3, 4) %>% 
  rename("group" = "U.S. households by food security status, 2022", "percent" = "...2") %>% 
  mutate(percent = as.numeric(percent)) %>% 
  filter(group != "Food-secure households") %>% 
  summarise(total_insecure = sum(percent)) %>% 
  pull(total_insecure)

transposed_acs <- transposed_acs %>% 
  mutate(
    single_parent = case_when(
      measure == "us_count" ~ single_parent_total,
      measure == "us_percent" ~ single_parent_percent,
      .default = NA),
    noncitizen = case_when(
      measure == "us_count" ~ non_citizen,
      measure == "us_count_me" ~ non_citizen,
      measure == "us_percent" ~ noncitizen_percent,
      .default = NA),
    foreign_born = case_when(
      measure == "us_count" ~ foreignborn,
      measure == "us_count_me" ~ foreignborn,
      measure == "us_percent" ~ foreignborn_percent,
      .default = NA),
    medicaid = case_when(
      measure == "us_count" ~ total_enrollment,
      measure == "us_percent" ~ medicaid_percent,
      .default = NA),
    food_insecurity = case_when(
      measure == "us_percent" ~ food_insecurity_rate,
      .default = NA)
    )

numeric_columns2 <- c("single_parent", "noncitizen")

transposed_acs[numeric_columns2] <- lapply(transposed_acs[numeric_columns2], as.numeric)

final_acs <- transposed_acs %>% 
  subset(measure == "us_percent") %>% 
  select(-c(single_parent1, single_parent2, non_citizen, measure, foreignborn)) %>% 
  mutate(neighborhood = "United States")

chicago_data_acs <- bind_rows(chicago_data, final_acs)

#export the cleaned dataset
write.csv(chicago_data_acs, "snap_enrollment/community_data.csv", row.names=FALSE)
