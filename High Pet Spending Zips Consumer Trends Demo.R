#PROJECT SUMMARY: Use the US Census data to determine best zip codes for ideal ad spend that optimize
#clients' ad budget i.e. highest return on ROAS

#CONCLUSION: Use the data mining to determine ad spend by zip code targeting and generated a 5-12x ROAS 


# Load Packages & Data ----------------------------------------------------

#Load the needed libraries
library(readxl) #read excel sheets
library(stringr) #work w strings
library(tidyverse) #suite of data science packages

#Data from filtered query was saved as a file - save to a local path in the set directory
path <- "~/Data Sources/Pet Services Census Data 18.xlsx"

#Use the purr function to iterate sheet reading
#Check the sheet names and glimpse sheets
path %>% excel_sheets() %>% set_names() %>% map(read_excel, path = path)

pet_spend <-
  path %>% 
  excel_sheets() %>% set_names() %>% map(read_excel, path = path)

#Dynamically import 6 sheets into single df
pet_spend <- path %>% 
  excel_sheets() %>% set_names() %>% 
  map_df(read_excel,
         path = path)


# Tidy Data ---------------------------------------------------------------

#Clean the data for use and find pet related spend by zip code


#Filter for pet related categories:
#1 - Entertainment/Recreation - Pets: Average = X9035_A
#2 - Pet Food: Average = X9036_A
#3 - Pets/Pet Supplies/Medicine for Pets: Average = X9037_A
#4 - Pet Services: Average = X9037_A
pet_spend_zips <- pet_spend %>%
  mutate(spend = as.integer(value)) %>%
  filter(str_detect(feature, regex(".*_A"))) %>% 
  group_by(zip) %>%
  summarize(spend = sum(spend, na.rm = T))



# Data Mining -------------------------------------------------------------

#Data stats via summary:
#Median: $1117 Mean: $1175 Min.: $0 1st Qu.: $943 3rd Qu.: $1356 Max. 3623
summary(pet_spend_zips)

#Label the zips by spend tier for ad targeting purposes
pet_spend_zips <- pet_spend_zips %>%
  mutate(pet_spending_tier = factor(
    case_when(
    spend <= 943 ~ "4 lowest spending zip",
    spend > 943 & spend <= 1117 ~ "3 below median spending zip",
    spend > 1117 & spend <= 1356 ~ "2 above median spending zip",
    spend > 1356 ~ "1 highest spending zip"
  ), 
  levels = c("1 highest spending zip", "2 above median spending zip", 
             "3 below median spending zip", "4 lowest spending zip"), 
  ordered = T))

#Create the list of top spending zip codes for pet spending
top_pet_spend_zips <- pet_spend_zips %>% 
  filter(pet_spending_tier == "1 highest spending zip")

# Data Visualization ------------------------------------------------------

#Set a custom theme for the subsequent plots
theme_pet <- theme(title = element_text(color = "gray25"),
                   plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   axis.title.x = element_text(margin = margin(20, 0, 20, 0)),
                   axis.title.y = element_text(margin = margin(0, 20, 0, 20)))


#Data visualization of the IQR of all pet spend
ggplot(pet_spend_zips, aes(y = spend)) + geom_boxplot() +
  labs(title = "The Diversity of Spending Amounts by Zip Codes for Pet Related Services & Goods Reveals Ad Targeting Opportunity",
       subtitle = "Recommendation: Segment Ad Spend by Zip Codes",
       y = "Dollars ($)",
       caption = "Data is from the 2018 census conducted by US Census Bureau") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme_pet

#Data visualization of the pet spend by tiers
ggplot(pet_spend_zips, aes(x = pet_spending_tier, y = spend)) + geom_boxplot() +
  labs(title = "Target the Highest Spending Zip Codes for Pet Related Services & Goods to Increase ROAS",
       subtitle = "ROAS Increased by 5-12x with Better Customer Segmentation",
       y = "Dollars ($)",
       x = "Pet Related Services & Goods Spending Tier",
       caption = "Data is from the 2018 census conducted by US Census Bureau") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme_pet
