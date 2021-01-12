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


  