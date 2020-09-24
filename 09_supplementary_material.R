## ----setup, include=FALSE-------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

library(gapminder)
library(here)
library(tidyverse)
library(ggrepel)
library(socviz)


## ----09-supplementary-material-1------------------------------------
my_numbers <- c(1, 2, 3, 1, 3, 5, 25)
your_numbers <- c(5, 31, 71, 1, 3, 21, 6)


## ----09-supplementary-material-2------------------------------------
my_numbers[4]
my_numbers[7]


## ----09-supplementary-material-3------------------------------------
my_numbers[2:4]


## ----09-supplementary-material-4------------------------------------
my_tb <- tibble(
    mine = c(1,4,5, 8:11),
    yours = c(3,20,16, 34:31))

class(my_tb)
my_tb


## ----09-supplementary-material-5------------------------------------
my_tb[3,1] # Row 3 Col 1
my_tb[1,2] # Row 1, Col 2 


## ----09-supplementary-material-6------------------------------------
my_tb[3,"mine"] # Row 3 Col 1
my_tb[1,"yours"] # Row 1, Col 2 


## ----09-supplementary-material-7------------------------------------
my_tb[3,"mine"] # Row 3 Col 1
my_tb[1,"yours"] # Row 1, Col 2 


## ----09-supplementary-material-8------------------------------------
my_tb[,"mine"] # All rows, Col 1


## ----09-supplementary-material-9------------------------------------
my_tb[4,] # Row 4, all cols


## ----09-supplementary-material-10-----------------------------------
my_tb$mine


## ----09-supplementary-material-11-----------------------------------
out <- lm(mine ~ yours, data = my_tb)

out$coefficients

out$call

out$qr$rank # nested 


## ----09-supplementary-material-12-----------------------------------
my_tb$ours <- my_tb$mine + my_tb$yours
my_tb


## ----09-supplementary-material-13-----------------------------------
knitr::kable(preg, caption="Some untidy data.", booktabs = TRUE)


## ----09-supplementary-material-14-----------------------------------
knitr::kable(preg2,
             caption="The same data, still untidy, but in a different way.", booktabs = TRUE)


## ----09-supplementary-material-15-----------------------------------
preg3 <- preg %>% 
  gather(treatment, n, treatmenta:treatmentb) %>%
  mutate(treatment = gsub("treatment", "", treatment)) %>%
    arrange(name, treatment)
knitr::kable(preg3, 
      caption="Tidied data. Every variable a column, every observation a row.", booktabs = TRUE)


## ----09-supplementary-material-15-pivot-----------------------------
preg4 <- preg %>% 
  pivot_longer(treatmenta:treatmentb, 
               names_to = "treatment", 
               names_prefix = "treatment",
               values_to = "n") %>%
    arrange(name, treatment)
knitr::kable(preg4, 
      caption="Tidied data using pivot_longer(). Every variable a column, every observation a row.", booktabs = TRUE)


## ----09-supplementary-material-16-----------------------------------
edu


## ----09-supplementary-material-18, echo=TRUE, message=FALSE, tidy = FALSE----
edu_t <- gather(data = edu,
                key = school,
                value = freq,
                elem4:coll4)

head(edu_t) 
tail(edu_t) 


## ----09-supplementary-material-18-pivot, echo=TRUE, message=FALSE, tidy = FALSE----
edu_t <- pivot_longer(data = edu, 
                      cols = elem4:coll4, 
                      names_to = "school",
                      values_to = "freq")
  
head(edu_t) 
tail(edu_t) 


## ----09-supplementary-material-19, fig.caption="A bad date.", fig.cap = "A bad date."----
p <- ggplot(data = bad_date, aes(x = date, y = N))
p + geom_line()


## ----09-supplementary-material-20, echo = TRUE, fig.width = 5, fig.height = 4, fig.cap = "Still bad."----

bad_date2 <- rbind(bad_date, bad_date)

p <- ggplot(data = bad_date2, aes(x = date, y = N))
p + geom_line()



## ----09-supplementary-material-21-----------------------------------
# install.packages("lubridate")
library(lubridate)

bad_date$date <- mdy(bad_date$date)
head(bad_date)



## ----09-supplementary-material-22, fig.cap="Much better.", out.width="100%", fig.height=4, fig.width=6, fig.margin=TRUE----
p <- ggplot(data = bad_date, aes(x = date, y = N))
p + geom_line()


## ----09-supplementary-material-23, eval = FALSE---------------------
## 
## url <- "https://cdn.rawgit.com/kjhealy/viz-organdata/master/organdonation.csv"
## 
## bad_year <- read_csv(url)
## bad_year %>% select(1:3) %>% sample_n(10)
## 


## ----09-supplementary-material-24-----------------------------------

bad_year <- read_csv(file = "data/organdonation.csv")
bad_year %>% select(1:3) %>% sample_n(10)



## ----09-supplementary-material-25, fig.cap="Integer year shown with a decimal point.",  fig.height = 4, fig.width = 5----

p <- ggplot(data = bad_year, aes(x = year, y = donors))
p + geom_point()



## ----09-supplementary-material-26-----------------------------------
bad_year$year <- int_to_year(bad_year$year)
bad_year %>% select(1:3)


## ----missing-1------------------------------------------------------
drat::addRepo("kjhealy")
install.packages("congress")
library(congress)

library(naniar)
library(visdat)



## ----missing-2------------------------------------------------------
vis_dat(congress)


## ----missing-3------------------------------------------------------
gg_miss_var(organdata)


## ----missing-4------------------------------------------------------
vis_dat(organdata)


## ----missing-5------------------------------------------------------
miss_var_summary(organdata)


## ----missing-6------------------------------------------------------
organdata %>%
  select(consent_law, year, pubhealth, roads) %>%
  group_by(consent_law) %>%
  miss_var_summary()



## ----missing-7------------------------------------------------------

vis_miss(organdata)


## ----missing-8------------------------------------------------------

gg_miss_upset(organdata)


## ----missing-9------------------------------------------------------
ggplot(data = organdata, mapping = aes(x = pubhealth, y = donors)) + 
geom_point()


## ----missing-10-----------------------------------------------------

ggplot(data = organdata, mapping = aes(x = pubhealth, y = donors)) + 
geom_miss_point()


## ----missing-11-----------------------------------------------------
gg_miss_fct(x = riskfactors, fct = marital)


## ----zero-1---------------------------------------------------------

## Hex colors for sex
sex_colors <- c("#E69F00", "#993300")

## Hex color codes for Dem Blue and Rep Red
party_colors <- c("#2E74C0", "#CB454A")

## Group labels
mf_labs <- tibble(M = "Men", F = "Women")

df <- read_csv("data/fc_sample.csv")

df



## ----zero-2---------------------------------------------------------
df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N))




## ----zero-3---------------------------------------------------------

df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N)) %>%
    ggplot(aes(x = start_year,
               y = freq,
               fill = sex)) +
    geom_col() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = sex_colors, 
                      labels = c("Women", "Men")) +
    labs(x = "Year", y = "Percent", fill = "Group") +
    facet_wrap(~ party)


## ----zero-4---------------------------------------------------------

df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N)) %>%
    ggplot(aes(x = start_year,
               y = freq,
               color = sex)) +
    geom_line(size = 1.1) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = sex_colors, 
                       labels = c("Women", "Men")) +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(x = "Year", y = "Percent", color = "Group") +
    facet_wrap(~ party)


## ----zero-5---------------------------------------------------------
df_f <- df %>% modify_if(is.character, as.factor)

df_f %>%
    group_by(start_year, party, sex) %>%
    tally()



## ----zero-5a--------------------------------------------------------
df_f %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N)) %>%
    ggplot(aes(x = start_year,
               y = freq,
               color = sex)) +
    geom_line(size = 1.1) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = sex_colors, 
                       labels = c("Women", "Men")) +
    guides(color = guide_legend(reverse = TRUE)) +
    labs(x = "Year", y = "Percent", color = "Group") +
    facet_wrap(~ party)


## ----zero-6---------------------------------------------------------
df %>%
    group_by(start_year, party, sex) %>%
    summarize(N = n()) %>%
    mutate(freq = N / sum(N)) %>%
    ungroup() %>%
    complete(start_year, party, sex,
             fill = list(N = 0, freq = 0))



## ----zero-7---------------------------------------------------------




## ----addnum1, echo = FALSE------------------------------------------
add_xy <- function(x = NULL, y = NULL) {
    x + y
}



## ----addnum2--------------------------------------------------------

add_xy(x = 1, y = 7)



## ----addnum4--------------------------------------------------------
add_xy(x = 5, y = 2)


## ----plot-section-function-1, echo=TRUE, tidy = FALSE---------------

plot_section <- function(section="Culture", x = "Year",
                         y = "Members", data = asasec,
                         smooth=FALSE){
    require(ggplot2)
    require(splines)
    # Note use of aes_string() rather than aes() 
    p <- ggplot(subset(data, Sname==section),
            mapping = aes_string(x=x, y=y))

    if(smooth == TRUE) {
        p0 <- p + geom_smooth(color = "#999999",
                              size = 1.2, method = "lm",
                              formula = y ~ ns(x, 3)) +
            scale_x_continuous(breaks = c(seq(2005, 2015, 4))) +
            labs(title = section)
    } else {
    p0 <- p + geom_line(color= "#E69F00", size=1.2) +
        scale_x_continuous(breaks = c(seq(2005, 2015, 4))) +
        labs(title = section)
    }

    print(p0)
}



## ----09-supplementary-material-27, fig.cap='Using a function to plot your results.', out.width="50%", fig.width=4, fig.height=3, fig.show = "hold"----

plot_section("Rationality")
plot_section("Sexualities", smooth = TRUE)



## ----plot-section-function-2, echo = TRUE, tidy = FALSE-------------

plot_section <- function(section="Culture", x = "Year",
                         y = "Members", data = asasec,
                         smooth=FALSE, ...){
    require(ggplot2)
    require(splines)
    # Note use of aes_string() rather than aes() 
    p <- ggplot(subset(data, Sname==section),
            mapping = aes_string(x=x, y=y))

    if(smooth == TRUE) {
        p0 <- p + geom_smooth(color = "#999999",
                              size = 1.2, ...) +
            scale_x_continuous(breaks = c(seq(2005, 2015, 4))) +
            labs(title = section)
    } else {
    p0 <- p + geom_line(color= "#E69F00", size=1.2) +
        scale_x_continuous(breaks = c(seq(2005, 2015, 4))) +
        labs(title = section)
    }

    print(p0)
}




## ----09-supplementary-material-28, fig.cap='Our custom function can now pass arguments along to fit different smoothers to Section membership data.', out.width="50%", fig.width=4, fig.height=3, fig.show = "hold"----

plot_section("Comm/Urban",
             smooth = TRUE,
             method = "loess")
plot_section("Children",
             smooth = TRUE,
             method = "lm",
             formula = y ~ ns(x, 2))



## ----09-supplementary-material-29, eval = FALSE, echo = TRUE--------
## 
## # You will need to use install.packages() to install
## # these map and GIS libraries if you do not already
## # have them.
## 
## library(maptools)
## library(mapproj)
## library(rgeos)
## library(rgdal)
## 
## us_counties <- readOGR(dsn="data/geojson/gz_2010_us_050_00_5m.json",
##                        layer="OGRGeoJSON")
## 
## us_counties_aea <- spTransform(us_counties,
##                     CRS("+proj=laea +lat_0=45 +lon_0=-100 \
##                          +x_0=0 +y_0=0 +a=6370997 +b=6370997 \
##                          +units=m +no_defs"))
## 
## us_counties_aea@data$id <- rownames(us_counties_aea@data)


## ----09-supplementary-material-30, eval = FALSE, echo = TRUE--------
## 
## alaska <- us_counties_aea[us_counties_aea$STATE == "02",]
## alaska <- elide(alaska, rotate=-50)
## alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
## alaska <- elide(alaska, shift=c(-2100000, -2500000))
## proj4string(alaska) <- proj4string(us_counties_aea)
## 
## hawaii <- us_counties_aea[us_counties_aea$STATE=="15",]
## hawaii <- elide(hawaii, rotate=-35)
## hawaii <- elide(hawaii, shift=c(5400000, -1400000))
## proj4string(hawaii) <- proj4string(us_counties_aea)
## 
## us_counties_aea <- us_counties_aea[!us_counties_aea$STATE %in% c("02", "15", "72"),]
## us_counties_aea <- rbind(us_counties_aea, alaska, hawaii)
## 


## ----09-supplementary-material-31, eval = FALSE, echo = TRUE, tidy = FALSE----
## county_map <- tidy(us_counties_aea, region = "GEO_ID")
## county_map$id <- stringr::str_replace(county_map$id,
##                                       pattern = "0500000US", replacement = "")


## ----09-supplementary-material-32, eval = FALSE, echo = TRUE, tidy = FALSE----
## county_map <- tidy(us_counties_aea, region = "GEO_ID")
## county_map$id <- stringr::str_replace(county_map$id,
##                                       pattern = "0500000US", replacement = "")


## ----09-supplementary-material-33, eval = FALSE---------------------
## devtools::install_github("hrbrmstr/hrbrthemes")


## ----09-supplementary-material-34, eval = FALSE, tidy = FALSE-------
## 
## theme_map <- function(base_size=9, base_family="") {
##     require(grid)
##     theme_bw(base_size=base_size, base_family=base_family) %+replace%
##         theme(axis.line=element_blank(),
##               axis.text=element_blank(),
##               axis.ticks=element_blank(),
##               axis.title=element_blank(),
##               panel.background=element_blank(),
##               panel.border=element_blank(),
##               panel.grid=element_blank(),
##               panel.spacing=unit(0, "lines"),
##               plot.background=element_blank(),
##               legend.justification = c(0,0),
##               legend.position = c(0,0)
##               )
## }
## 

