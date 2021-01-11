# load tidyverse packages
library(tidyverse)

# read in data
gapminder_1997 <- read_csv("data/gapminder_1997.csv")

# learn more about a function
?read_csv

read_csv(file = "data/gapminder_1997.csv")

# make a plot
ggplot(data = gapminder_1997) +
  aes(x = `gdpPercap`) +
  labs(x = "GDP Per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy") +
  labs(title = "Do people
       - in wealthy countries live longer?") +
  geom_point() +
  aes(color = continent) +
  scale_color_brewer(palette = "Set1") +
  aes(size = pop/1000000) +
  labs(size = "Population (in millions)") +
  aes(shape = continent)

# different color palettes
RColorBrewer::display.brewer.all()

# collapse code to make more concise
ggplot(data = gapminder_1997, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000)) +
  labs(x = "GDP Per Capita", y = "Life Expectancy", size = "Population (in millions)",
       title = "Do people in wealthy countries live longer?") +
  geom_point() +
  scale_color_brewer(palette = "Set1")

# read in full dataset
gapminder_data <- read_csv("data/gapminder_data.csv")
dim(gapminder_data)

ggplot(data = gapminder_data) +
  aes(x = year, y = lifeExp, color = continent, group = country) +
  geom_line()

# plot categorical variables
# use gapminder_1997 data with geom_boxplot() to make boxplot where contient is the x axis, and life expectancy is the y axis

ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  labs(x = 'Continent', y = 'Life Expectancy (years)', title = 'Life expectancy by continent (1997)') +
  geom_jitter(aes(size=pop)) +
  geom_violin(alpha=0.5, aes(fill = continent))

# univariate plots

ggplot(gapminder_1997) +
  aes(x = lifeExp) +
  geom_histogram(bins = 20) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# saving plots

ggsave("figures/awesome_plot.jpg", width = 6, height = 4)

violin_plot <- ggplot(data = gapminder_1997, aes(x = continent, y = lifeExp)) +
  labs(x = 'Continent', y = 'Life Expectancy (years)', title = 'Life expectancy by continent (1997)') +
  geom_jitter(aes(size=pop)) +
  geom_violin(alpha=0.5, aes(fill = continent))

violin_plot
violin_plot + theme_bw()
violin_plot

violin_plot <- violin_plot + theme_bw()
violin_plot

ggsave("figures/awesome_violin_plot.jpg", plot = violin_plot, width = 6, height = 4)

# Faceting plots
facet_plot <- ggplot(gapminder_1997) +
  aes(x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))

# practice saving a plot as "my_awesome_plot.jpg"
ggsave("figures/my_awesome_plot.jpg", plot = facet_plot)
