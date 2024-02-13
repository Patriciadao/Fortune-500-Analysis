#Set up
library(dplyr)
library(ggplot2)
fortune_data <- read.csv("/Users/daodao/Desktop/MSBA/WRANGLING/Final Project/fortune1000_2023.csv")

#1.I want to see what proportion of fortune CEO are female

first_question <- (sum(fortune_data$FemaleCEO == "yes"))/1000
print(first_question)
ggplot(fortune_data, aes(x = FemaleCEO)) +
  geom_bar(fill = "black") +
  labs(
    title = "Female CEO Distribution",
    x = "Female CEO",
    y = "Count"
  ) +
  theme_minimal()
#And OMG, the proportion is less than 10%....

#2.I want to see who are the most outstanding new comers to the fortune 500

second_question <- fortune_data %>%
  filter(Newcomer_to_the_Fortune500 == "yes", Profitable == "yes") %>% 
  select(Rank, Company, Sector, Industry, Profits_M, Change_in_Rank, Growth_in_Jobs)
print(second_question)

#Interesting, Lululemon and AirBnb are both rising up, and most of those new comers are making profit

#3.I want to see the fortune 1000 location distribution to see where their headquarters mostly clustered

library(tidyverse)
library(leaflet)
library(mapproj)
location_data <- read.csv("/Users/daodao/Desktop/uscities.csv")
fortune_data$city_state <- paste(fortune_data$HeadquartersCity, fortune_data$HeadquartersState, sep = " ")
location_data$city_state <- paste(location_data$city, location_data$state_id, sep = " ")
data_with_geo <- left_join(fortune_data, location_data, by = c("city_state" = "city_state"))
city_state_data1 <- data.frame(
  Company = data_with_geo$Company,
  City = data_with_geo$city_state,
  Latitude = data_with_geo$lat,
  Longitude = data_with_geo$lng
)
mymap1 <- leaflet() %>%
  setView(lng = -98.583333, lat = 39.833333, zoom = 4) %>%
  addTiles()
mymap1 <- mymap1 %>%
  addCircleMarkers(
    data = city_state_data1,
    lng = ~Longitude,
    lat = ~Latitude,
    radius = ~3,
    stroke = FALSE,
    fillOpacity = 0.1,
    popup = ~Company
  )
mymap1

#According to my map, most fortune 1000 companies are located in the Eastern part of U.S.

#4.I want to see the fortune 1000 location distribution to see where the companies with most revenue are
city_state_data2 <- data.frame(
  Company = data_with_geo$Company,
  Revenue = data_with_geo$Revenues_M,
  City = data_with_geo$city_state,
  Latitude = data_with_geo$lat,
  Longitude = data_with_geo$lng
)
mymap2 <- leaflet() %>%
  setView(lng = -98.583333, lat = 39.833333, zoom = 4) %>%
  addTiles()
mymap2 <- mymap2 %>%
  addCircleMarkers(
    data = city_state_data2,
    lng = ~Longitude,
    lat = ~Latitude,
    radius = ~Revenue/10000,
    stroke = FALSE,
    fillOpacity = 0.1,
    popup = ~Company
  )
mymap2

#Companies like Walmart,Amazon and Apple really stand out with big circles

#5. I want to see the percentage distribution of fortune 1000 companies in different sectors
library(dplyr)
sector_distribution <- fortune_data %>%
  group_by(Sector) %>%
  summarize(
    Count = n(),
    Percentage = Count / nrow(fortune_data) * 100
  ) %>%
  arrange(desc(Count))

sector_distribution

#Top three are financials, technology and energy. Alright, not really exciting for me.

#6. I want to see what are the profitable companies in the household product and apparel sectors for professional consideration
target_companies <- fortune_data %>%
filter((Sector == "Household Products" | Sector == "Apparel" | Sector == "Retailing" ) & Profitable == "yes") %>% 
select(Rank, Sector, Company, HeadquartersCity, HeadquartersState)

#Thanks R studio, now I know what are the companies I'm going to apply next year

