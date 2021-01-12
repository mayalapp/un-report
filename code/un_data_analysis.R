# load in tidyverse functions
library(tidyverse)

# load in the data
gapminder_data <- read_csv("data/gapminder_data.csv")
View(gapminder_data)

# use the summarize function to find summary statistics
summarize(gapminder_data, averageLifeExp = mean(lifeExp))

# piping function: %>%  
gapminder_data %>% summarize(averageLifeExp = mean(lifeExp))

# Exercise: find the mean population of the gapminder dataset
gapminder_data %>% summarize(averagePopulation = mean(pop), 
                             recent_year = max(year))

# filters rows where year is 2007 
gapminder_data %>% filter(year == 2007) %>% summarize(averageLifeExp = mean(lifeExp))

# Exercise: Find the average GDP per capita for the first year in the dataset
gapminder_data %>% summarize(first_year = min(year)) #1952 
gapminder_data %>% 
  filter(year == 1952) %>% 
  summarize(averageGDP_percap = mean(gdpPercap), first_year = min(year))

# we can use: >, <, >=, <= 

#group_by 

gapminder_data %>% 
  group_by(year) %>% 
  summarize(averageLifeExp = mean(lifeExp))


# Exercise: find the mean life expectancy for each continent 
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(averageLifeExp = mean(lifeExp))


# mutate - add more columns to your dataset 
gapminder_data %>% 
  mutate(gdp = gdpPercap * pop)

# Exercise: make a new column using mutate() that is population in millions 
gapminder_data %>% 
  mutate(popMil = pop / 1000000)

# select() - specify which column we want to keep 

gapminder_data %>% 
  select(year, pop)

# drop the continent column (and save it into new_dataframe)
new_dataframe <- gapminder_data %>% 
  select(-continent)

# Exercise: create a dataframe with the country, continent, year, and lifeExp columns 
gapminder_data %>% 
  select(-pop, -gdpPercap)

gapminder_data %>% 
  select(country, continent, year, lifeExp)

# arrange(year) - arrange rows 

# long vs. wide 
# pivot_longer, and pivot_wider 
gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

# rename() - rename columns

# Exercise: Create a new dataset with only data from the Americas and 2007
# drop the continent and year columns 

gapminder_Americas_2007 <- gapminder_data %>% 
  filter(year == 2007, continent == "Americas") %>% 
  select(-continent, -year) 

gapminder_data <- read_csv("data/gapminder_data.csv") %>% 
  filter(year == 2007, continent == "Americas") %>% 
  select(-continent, -year) 

View(gapminder_data)

# Goals: data from a year that is close to 2007 
# A column for country and we want columns for different types of Co2 emissions (total, per capita)

# Exercise: select only the country, year, series, value 
co2_emissions <- read_csv("data/co2-un-data.csv", skip = 2, 
         col_names = c("region", "country", "year", "series", "value", 
                       "footnotes", "source")) %>% 
  select(-region, -footnotes, -source) %>% 
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series, values_from = value) %>% 
  filter(year == 2005) %>%
  select(-year) %>% 
  mutate(country = recode(country, 
                          "Bolivia (Plurin. State of)" = "Bolivia", 
                          "United States of America" = "United States", 
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))

View(co2_emissions) 

# joining the datasets
inner_join(gapminder_data, co2_emissions, by = "country")
gapminder_data %>% inner_join(co2_emissions, by = "country")

# checking what is in gapminder and not c02
anti_join(gapminder_data, co2_emissions)

# change PR to be a part of the US 
gapminder_data <- read_csv("data/gapminder_data.csv") %>% 
  filter(year == 2007, continent == "Americas") %>% 
  select(-continent, -year) %>% 
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>% 
  group_by(country) %>% 
  summarize(lifeExp = sum(lifeExp * pop)/sum(pop), 
            gdpPercap = sum(gdpPercap * pop)/ sum(pop), 
            pop = sum(pop))

View(gapminder_data)

anti_join(gapminder_data, co2_emissions) # everything matches! 

gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by = "country")

# mutate and the if_else 
# if_else(condition, true, false)
gap_co2_region <- gapminder_co2 %>% 
  mutate(region = if_else(country == "Canada" | 
                            country == "United States" | 
                            country == "Mexico", "north", "south"))

# country %in% c("Canada", "United States", "Mexico")
# | - or 
# && - and 
# ! - not 

# is there a relationship between gdp and co2 
# exercise: create a scatter plot of gdp vs co2 emissions, color it by region

gap_co2_region %>% ggplot() +
  aes(x = gdpPercap,y = per_capita_emissions, color = region) +
  labs(x = "GDP Per Capita", y = "Emissions") +
  geom_point()


# write your dataframe to a csv file! 
write_csv(gap_co2_region, "data/gapminder_co2.csv")




  