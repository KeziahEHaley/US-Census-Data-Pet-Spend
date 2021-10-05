#PROJECT SUMMARY: Marketing suggested these higher spending zip codes are due to high population densities and wanted to increase 
#ad spend in the zip codes with the highest population densities. The US Census data is used to check this assumption.


#CONCLUSION: Research on the demographics of the higher spending zip codes reveals the variance is mostly like due to higher income
#levels, higher education levels, and are areas with higher home values. Population density shows no statistical evidence of impacting 
#higher pet spend based on the data model


# Load Packages & Data ----------------------------------------------------

#Load the needed libraries
library(GGally) #corr matrix 
library(readxl) #read excel sheets
library(stringr) #work w strings
library(tidyverse) #suite of data science packages

#Import US Demographic data - 47 columns so col_types & col_select is tedious
us_demo <- read_csv("~/Data Sources/US Census Data Demographics 18.csv",
                    col_names = T)


# Tidy Data ---------------------------------------------------------------

#The csv file converted the zip to double as data type but it needs to be a string with the correct amount of preceeding zeros
us_demo <- us_demo %>%
  mutate(zip = as.character(zip),
         zip = case_when(
           str_length(zip) == 3 ~ paste0("00", zip),
           str_length(zip) == 4 ~ paste0("0", zip),
           TRUE ~ zip 
         )) %>%
  filter(!state_name %in% c("American Samoa", "Federated States of Micronesia", "Guam", "Marshall Islands", "Northern Mariana Islands",
                            "Palau", "Puerto Rico", "Virgin Islands"))


# Data Mining -------------------------------------------------------------


#Find the summary stats of the us_demo data by demographics of possible interest
us_demo_means <- us_demo %>%
  summarize(pop = mean(population, na.rm = T),
            density = mean(density, na.rm = T),
            age_median = mean(age_median, na.rm = T),
            female = mean(female, na.rm = T),
            family_size = mean(family_size, na.rm = T),
            income_household_median = mean(income_household_median, na.rm = T),
            income_individual_median = mean(income_individual_median, na.rm = T),
            home_ownership = mean(home_ownership, na.rm = T),
            home_value = mean(home_value, na.rm = T),
            rent_median = mean(rent_median, na.rm = T),
            rent_burden = mean(rent_burden, na.rm = T),
            education_college_or_above = mean(education_college_or_above, na.rm = T),
            unemployment_rate = mean(unemployment_rate, na.rm = T)) %>%
  mutate(pop_den = pop/density) %>% 
  select(age_median:pop_den)

#Compare the summary stats of the us_demo data for top spending zip codes only
us_demo_top_means <- top_pet_spend_zips %>% 
  left_join(us_demo, by = c("zip")) %>%
  summarize(pop = mean(population, na.rm = T),
            density = mean(density, na.rm = T),
            age_median = mean(age_median, na.rm = T),
            female = mean(female, na.rm = T),
            family_size = mean(family_size, na.rm = T),
            income_household_median = mean(income_household_median, na.rm = T),
            income_individual_median = mean(income_individual_median, na.rm = T),
            home_ownership = mean(home_ownership, na.rm = T),
            home_value = mean(home_value, na.rm = T),
            rent_median = mean(rent_median, na.rm = T),
            rent_burden = mean(rent_burden, na.rm = T),
            education_college_or_above = mean(education_college_or_above, na.rm = T),
            unemployment_rate = mean(unemployment_rate, na.rm = T)) %>%
  mutate(pop_den = pop/density) %>% 
  select(age_median:pop_den)
  
#The means suggest that population density decreased for the top spending zip codes


# Data Modeling -----------------------------------------------------------

us_demo_top <- top_pet_spend_zips %>% 
  left_join(us_demo, by = c("zip")) %>% 
  select(spend, density, female, age_median, family_dual_income, income_household_median, income_individual_median, 
         home_value, rent_median, education_college_or_above, unemployment_rate) %>% 
  rename(`Pet_Spend` = spend,
         `Pop_Density` = density,
         `Prop_Female` = female,
         `Median_Age` = age_median,
         `Dual_Family_Income` = family_dual_income,
         `Med_Household_Income` = income_household_median,
         `Med_Individual_Income` = income_individual_median,
         `Home_Value` = home_value,
         `Median_Rent` = rent_median,
         `College_Edu.` = education_college_or_above,
         `Unemployment` = unemployment_rate)

#Use the correlation matrix to determine correlation coefficients 
ggcorr(us_demo_top, palette = "RdBu", label = TRUE, nudge_x = -0.2 , name = "Corr. Co") 

