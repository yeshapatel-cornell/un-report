library(tidyverse)
library(readr)
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

# working with messy data

co2_emmission_dirty <- read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region","country","year","series","value","footnotes","source"))

co2_emissions <- co2_emmission_dirty %>%
  select(country, year, series, value) %>%
mutate (series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                        "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter (year == 2005) %>%
  select (-year)
 
#Bringing in 2007 population data

gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter (year == 2007) %>%
  select (country, pop, lifeExp, gdpPercap)

# joining two dataframes together

# innerjoin- discards countries not common between the tables

joint_co2_pop <- inner_join(co2_emissions, gapminder_data_2007, by = "country")

anti_join(co2_emissions, gapminder_data_2007, by = "country")

anti_join(gapminder_data_2007, co2_emissions, by ="country")  

full_join(co2_emissions, gapminder_data_2007)%>%
  view


co2_emissions%>%
  left_join(gapminder_data_2007)

co2_emissions%>%
  right_join(gapminder_data_2007)

# writing a csv

write_csv (joint_co2_pop, file="data/joint_co2_pop.csv")

joint_co2_and_pop <- read_csv ("data/joint_co2_pop.csv")

# create a seperate histogram for both lifeExp and gdpPercap to explore those variables

ggplot (joint_co2_and_pop) +
  aes (x = lifeExp) + 
  geom_histogram () +
  labs (x= "Life Expectancy")
 

ggplot (joint_co2_and_pop) +
  aes (x = gdpPercap) + 
  geom_histogram () +
  labs (x= "GDP per Capita")

joint_co2_and_pop %>%
  ggplot(aes(x= gdpPercap))+
  labs (x= "GDP per Capita")+
  geom_histogram()

gdp_co2_plot <- joint_co2_and_pop %>%
  ggplot(aes (x=gdpPercap,y=per_capita_emissions))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE) +
  labs (x= "GDP per Capita", y ="CO2 emissions per Capita (metric tons)", title= "Comparing Per Capita CO2 emissions and GDP")+
  theme_classic()+
  ggpubr::stat_regline_equation(aes(label = after_stat(rr.label)))


ggsave(gdp_co2_plot, filename = "figures/gdg_co2_plot.png", height = 4, width = 6, units = "in", dpi = 300)

install.packages("ggpubr")

