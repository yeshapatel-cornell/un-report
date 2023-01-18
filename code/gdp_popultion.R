#Run life expectancy and CO2 emissions versus population
#Jan 27th, 2023

#load in packages necessary for analysis

library("tidyverse")
library(readr)

#read data for analysis
gapminder_1997 <- read_csv("un-report/gapminder_1997.csv")

#Plotting data for visualization

ggplot(data = gapminder_1997) +
  aes(x = gdpPercap) +
  labs(x= "GDP per Capita")+
  aes(y = lifeExp) +
  labs(y = "Life Expectancy (yrs)") +
  geom_point() +
  labs(title = "Do people in wealthy countries live longer ?") +
  aes(color = continent) +
  aes(shape = continent) +
  scale_color_brewer(palette = "Set1") + 
  aes(size = pop/1000000) +
  labs(size = "Population (in millions)")
 
# scale_shape_manual, size, color.....try different options

# legend order can also be changed

#short-handed ggplot
ggplot(data = gapminder_1997,
       aes (x = gdpPercap, y = lifeExp, color = continent, 
            shape = continent, size = pop/1000000)) + 
  labs(x= "GDP per Capita", y ="Life Expentancy (yrs)", 
       size = "Population (in millions)") +
  geom_point()

#Read in all of the data from gapminder (more years than 1997)
library(readr)
gapminder_data <- read_csv("un-report/gapminder_data.csv")
view(gapminder_data)
dim(gapminder_data)  #first number is of rows then columns
head(gapminder_data)
tail(gapminder_data)

ggplot(data = gapminder_data) +
  aes(x=year, y=lifeExp, color=continent, group=country) +
  geom_line()

# learn about data
str(gapminder_data) # $ sign indicates column name

ggplot(data = gapminder_1997) +
  aes(x=continent, y=lifeExp) +
  geom_violin(color = "cornflower blue", fill = "pink") +
  geom_jitter(aes (size = pop))


#histogram
ggplot(gapminder_1997) +
  aes( x= lifeExp)+
  geom_histogram(bins = 20) +
  theme_prism() # explore package called ggthemes

#installing themes used in prism graphpad
install.packages("ggprism")
library("ggprism")

ggplot(gapminder_1997) +
  aes (x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))

ggplot(gapminder_1997) +
  aes (x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_grid(rows = vars(continent))

ggsave("awesome_plot.tiff", device = tiff, width = 6, height = 4)

lifeExp <- ggplot(gapminder_1997) +  # saved the graph as lifeExp object and then saved that with ggsave
  aes (x = gdpPercap, y = lifeExp) +
  geom_point() +
  facet_wrap(vars(continent))

ggsave (plot = lifeExp, file = "lifeExp.jpg", width = 6, height = 4)



