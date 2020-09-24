## ----setup=FALSE-------------------------------------------

library(gapminder)
library(tidyverse)
library(socviz)


## 4.2 Grouped Data and the "Group" Aesthetic

# Reminder of what gapminder looks like
view(gapminder)

# Brief reminder of what we looked at in chapter 3

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))
p

# We plotted a scatterplot of y against x
p + geom_point() 

# Then we added a smoothed line of y against x

p + geom_smooth()

# Then we combined both the scatterplot and the smoother line

p + geom_point() + geom_smooth() 

# In section 4.2, let's try the same thing, but with different variables


p <- ggplot(data = gapminder,
            mapping = aes(x = year,
                          y = gdpPercap))
p + geom_line()       

# To see what's going on, take a look at structure of data

str(gapminder)

# We are asking it to plot a Factor variable on a continuous axis

# Let's plot continuous variable against continuous variable, but grouped by Factors

p <- ggplot(data = gapminder,
            mapping = aes(x = year,
                          y = gdpPercap))
p + geom_line(mapping = 
                    aes(group = country))       

## 4.3 Facet to Make Small Multiples
p <- ggplot(data = gapminder,
            mapping = aes(x = year,
                          y = gdpPercap))

p + geom_line(mapping = aes(group = country)) + 
    facet_wrap(~ continent)      

# Note how the layout of the picture allows facets to share axes titles

# Also note that "~ continent" is a powerful statement. For now, DON'T used it
# on continuous variables, and be careful about using it Factors with a lot of levels.

p + geom_line(mapping = aes(group = country)) + 
  facet_wrap(~ country)      

# FIne-tuning facet_wrap() to specify 5 columns

p + geom_line(mapping = aes(group = country)) + 
  facet_wrap(~ continent, ncol = 5)      

# Let's add a smoother line, set color for each country to grey, 
# use log10 y-axis, and add labels

p + geom_line(color="gray70", mapping=aes(group = country)) +
    geom_smooth(size = 1.1, method = "loess", se = FALSE) +
    scale_y_log10(labels=scales::dollar) +
    facet_wrap(~ continent, ncol = 5) +
    labs(x = "Year",
         y = "log GDP per capita",
         title = "GDP per capita on Five Continents")      

# Why did we specify "se = FALSE"?

## ----facet_grid-----------------------------------------------------

p <- ggplot(data = gss_sm,
            mapping = aes(x = age, y = childs))
p + geom_point(alpha = 0.2) + geom_smooth() +
    facet_grid(sex ~ race)



## ----histogram------------------------------------------------------

p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion))
p + geom_bar()



## ----histogram_2----------------------------------------------------

p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion))
p + geom_bar(mapping = aes(y = ..prop..))



## ----histogram_3----------------------------------------------------

p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion))
p + geom_bar(mapping = aes(y = ..prop.., group = 1)) 




## ----gss_tab--------------------------------------------------------

table(gss_sm$religion)



## ----gss_color_fill-------------------------------------------------


p <- ggplot(data = gss_sm,
            mapping = aes(x = religion, color = religion))
p + geom_bar()

p <- ggplot(data = gss_sm,
            mapping = aes(x = religion, fill = religion))
p + geom_bar() + guides(fill = FALSE)



## ----two_way_1------------------------------------------------------

p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion, 
                          fill = religion))
p + geom_bar()



## ----two_way_2------------------------------------------------------
p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion, 
                          fill = religion))
p + geom_bar(position = "fill")      


## ----two_way_3------------------------------------------------------
p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion, 
                          fill = religion))
p + geom_bar(position = "dodge",
             mapping = aes(y = ..prop..))      


## ----two_way_4------------------------------------------------------
p <- ggplot(data = gss_sm,
            mapping = aes(x = bigregion, 
                          fill = religion))
p + geom_bar(position = "dodge",
             mapping = aes(y = ..prop.., 
                           group = religion))       


## ----hist_1---------------------------------------------------------

p <- ggplot(data = midwest,
            mapping = aes(x = area))
p + geom_histogram()

p <- ggplot(data = midwest,
            mapping = aes(x = area))
p + geom_histogram(bins = 10)



## ----hist_2---------------------------------------------------------

oh_wi <- c("OH", "WI")

p <- ggplot(data = subset(midwest, subset = state %in% oh_wi),
            mapping = aes(x = percollege, fill = state))
p + geom_histogram(alpha = 0.4, bins = 20)



## ----density_1------------------------------------------------------

p <- ggplot(data = midwest,
            mapping = aes(x = area))
p + geom_density()




## ----density_2------------------------------------------------------


p <- ggplot(data = midwest,
            mapping = aes(x = area, fill = state, color = state))
p + geom_density(alpha = 0.3)



## ----titanic_1------------------------------------------------------

titanic



## ----titanic_2------------------------------------------------------

p <- ggplot(data = titanic,
            mapping = aes(x = fate, y = percent, fill = sex))
p + geom_bar(position = "dodge", stat = "identity") + theme(legend.position = "top")



## ----lifegap_1------------------------------------------------------

oecd_sum



## ----lifegap_2, fig.height = 4, fig.width = 9, layout = 'l-page'----

p <- ggplot(data = oecd_sum,
            mapping = aes(x = year, y = diff, fill = hi_lo))
p + geom_col() + guides(fill = FALSE) +
  labs(x = NULL, y = "Difference in Years",
       title = "The US Life Expectancy Gap",
       subtitle = "Difference between US and OECD
                   average life expectancies, 1960-2015",
       caption = "Data: OECD. After a chart by Christopher Ingraham,
                  Washington Post, December 27th 2017.")


