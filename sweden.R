rm(list=ls())

library(ggplot2)
library(cowplot)
library(readxl)
library(shadowtext)

# https://www.scb.se/en/finding-statistics/statistics-by-subject-area/population/population-composition/population-statistics/pong/tables-and-graphs/preliminary-statistics-on-deaths
raw <- read_excel("2020-09-21-preliminar_statistik_over_doda_inkl_eng.xlsx", sheet=2, skip=6)

years <- as.numeric(names(raw)[2:7])

names(raw) <- c("date", "y2015", "y2016", "y2017", "y2018", "y2019", "y2020", "avg", "-", "sort", "day", "month")
first33 <- raw[1:233,]

totals <- c(
  sum(first33$y2015),
  sum(first33$y2016),
  sum(first33$y2017),
  sum(first33$y2018),
  sum(first33$y2019),
  sum(first33$y2020)
)

# https://www.scb.se/contentassets/d017b78940424473a31527f8c685ed34/be0101_tab8samdrag2019mars_eng.xlsx
raw_populations <- read_excel("be0101_tab8samdrag2019mars_eng.xlsx", sheet=1, skip=5)
populations <- rev(as.numeric(raw_populations[1,2:6]))
populations[6] <- 10099265 # https://www.worldometers.info/world-population/sweden-population/

d <- data.frame(total=totals, year=years, population=populations, stringsAsFactors=F)
d$norm_year <- d$total/d$population*100000

pop_2015 <- d$population[which(d$year == 2015)]
d$norm_2015 <- d$total/pop_2015*100000

d$rounded_year <- round(d$norm_year)
d$rounded_2015 <- round(d$norm_2015)
d$color <- "gray"
d$color[which(d$year == 2020)] <- "orange"

ggplot(d, aes(x=year, y=rounded_2015, fill=color)) +
  geom_bar(stat = "identity") +
  geom_shadowtext(label=d$rounded_2015, nudge_y = -30) +
  scale_x_continuous(name="Year", breaks=c(2015:2020)) +
  scale_y_continuous(name="Deaths / 100k") +
  labs(title="Sweden", subtitle="Deaths per 100k (all causes, including COVID-19)\nJan 1-Aug 20\nNormalized to 2015 population") +
  guides(fill=F) +
  scale_fill_manual(values=c("gray", "orange"))

ggplot(d, aes(x=year, y=rounded_year, fill=color)) +
  geom_bar(stat = "identity") +
  geom_shadowtext(label=d$rounded_year, nudge_y = -30) +
  scale_x_continuous(name="Year", breaks=c(2015:2020)) +
  scale_y_continuous(name="Deaths / 100k") +
  labs(title="Sweden", subtitle="Deaths per 100k (all causes, including COVID-19)\nJan 1-Aug 20\nNormalized to yearly population") +
  guides(fill=F) +
  scale_fill_manual(values=c("gray", "orange"))
