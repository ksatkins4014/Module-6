########################################################################################
# Summary: Tidying and analyzing cotton production trends in NC
# Date: September 26, 2019

########################################################################################

# Motivating questions ----
### How have yield and area harvested of cotton changed across all of the NC ag districts over time?
### What were the top 3 cotton producing counties in NC in terms of total lbs of cotton for 2018?

# Clear workspace & load packages ----
rm(list=ls(all=TRUE))
library(tidyverse)

# 2. Read & inspect the dataset ----
cotton_data <- read_csv("data/cotton-usda-nass.csv")

### inspection including: str(), head(), tail(), dim(), and external data source summary using Summary()
str(cotton_data)
head(cotton_data)
tail(cotton_data)
dim(cotton_data)
summary(cotton_data)

# 3.1. Create a NC data subset ----

## isolating NC data

nc_cotton <- cotton_data %>%
  filter(state == "NORTH CAROLINA")

## selecting applicable data for motivating questions

nc_cotton %>%
  select(year, state, ag_district, county, data_item, value) -> nc_cotton

## dataset inspection
str(nc_cotton)
head(nc_cotton)
tail(nc_cotton)
dim(nc_cotton)
summary(nc_cotton)


# 3.2. Divide the data_item column ----

nc_cotton %>%
  separate(data_item, 
      into = c("cotton_type", "measurement"),
      sep =  " - ") -> nc_cotton


 

# 3.3. Convert the value column to numeric type
nc_cotton %>%
  filter(value >= 0) %>%
  drop_na(value) -> nc_cotton

as.numeric(nc_cotton$value)
nc_cotton$value <- as.numeric(nc_cotton$value)

summary(nc_cotton)


# 4. Visualizing trends ----

nc_cotton %>%
  ggplot(mapping = aes(x = year, y = value)) +
  geom_point(size = 0.5) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(vars(measurement), 
             vars(ag_district),
             scales = "free")
  

# 5. Summarize data from 2018 ----
### Filter out unnecessary data and non-unique county names
nc_cotton %>%
  select(year, county, measurement , value) %>%
  filter(county != "OTHER (COMBINED) COUNTIES") %>%
  filter(year == 2018) %>% 
  ###spread the measurement column to separate yield and acres harvested
  spread(measurement, value) -> nc_cotton_2018


##create new column of total lbs harvested and select top 3 in the column  
nc_cotton_2018 %>%
  mutate(total_lbs = `ACRES HARVESTED`*`YIELD, MEASURED IN LB / ACRE`) %>%
  select(county, total_lbs) %>%
  top_n(3) %>% view()


  





