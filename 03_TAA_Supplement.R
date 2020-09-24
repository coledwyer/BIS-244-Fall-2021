# Should already have tidyverse installed, so this just loads it into memory
library(tidyverse)

# Read in us-counties.csv

us_counties <- read_csv("us-counties.csv")

# Filter out non-PA counties

pa_counties <- us_counties %>% filter(state=="Pennsylvania")
View(pa_counties)

# First attempt at plotting
p <- ggplot(data = pa_counties,
            mapping = aes(x = date,
                          y = cases))
p + geom_point()+
  labs(x = "Date", y = "COVID-19 Cases",
       title = "COVID-19 Cases in Pennsylvania",
       subtitle = "Data points are cumulative cases",
       caption = "Source: NY Times web GitGub repo")

# Why are we getting multiple "lines"?

# Let's try coloring by county

p <- ggplot(data = pa_counties,
            mapping = aes(x = date,
                          y = cases,
                          color = county))
p + geom_point()+
  labs(x = "Date", y = "COVID-19 Cases",
       title = "COVID-19 Cases in Pennsylvania",
       subtitle = "Data points are cumulative cases",
       caption = "Source: NY Times web GitGub repo")

# Filter down to just Lehigh county

lehigh <- pa_counties %>% filter(county=="Lehigh")
View(lehigh)

p <- ggplot(data = lehigh,
            mapping = aes(x = date,
                          y = cases))
p + geom_point(color="brown")+
  labs(x = "Date", y = "COVID-19 Cases",
       title = "COVID-19 Cases in Lehigh County, Pennsylvania",
       subtitle = "Data points are cumulative cases",
       caption = "Source: NY Times web GitGub repo")
