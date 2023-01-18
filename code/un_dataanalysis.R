library(tidyverse)
gapminder_data <- read_csv("data/gapminder_data.csv")
view(gapminder_data)
summarize(gapminder_data, averageLifeExp=mean(lifeExp), medianLifeExp=median(lifeExp))

#Learning to pipe
gapminder_summary <- gapminder_data%>%
  summarize(averageLifeExp=mean(lifeExp))

# Filtering gapminder data, # == called as logical operator; we can use < > (!= means every year other than 2007)
gapminder_summary_2007 <- gapminder_data%>%
  filter(year == 2007) %>% 
  summarize(averageLifeExp=mean(lifeExp))

read.csv(gapminder_data)
gapminder_data%>%
  summarize (min(year))
  
gapminder_data%>%
filter (year == 1952)%>%
  summarize(GDPperCap=mean(gdpPercap))

#Using group_by()

gapminder_data%>%
  group_by(year, continent)%>%
  summarize(average = mean(lifeExp),
            error = sd(lifeExp))

#mutate function

gapminder_data%>%
  mutate(gdp = pop * gdpPercap)%>%
mutate(gdpinmillions = gdp/1000000)

# select function
gapminder_data%>%
  select(pop, year)

gapminder_data%>%
  select(-continent, - pop)

# pivot wider

gapminder_data%>%
  select (country, continent, year, lifeExp)%>%
  pivot_wider(names_from = year, values_from = lifeExp)
