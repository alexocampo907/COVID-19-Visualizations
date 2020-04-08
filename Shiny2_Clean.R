rm(list=ls())

library(tidyverse)


# JHU for Shiny -----------------------------------------------------------

library(tidycovid19)
world <- download_merged_data(cached = TRUE)
world <- world[,c("country","confirmed","deaths","date")]
names(world) <- c("Country","Confirmed","Deaths","Date")

world$Country[world$Country == "Korea, South"] <- "South Korea"
world$Country[world$Country == "US"] <- "USA"


# State Data NYtimes ------------------------------------------------------

library(readr)
usa <- read_csv("https://raw.github.com/nytimes/covid-19-data/master/us-states.csv")
usa <- usa[,-3]
names(usa) <- c("Date","State","Confirmed","Deaths")

usa.counties <- read_csv("https://raw.github.com/nytimes/covid-19-data/master/us-counties.csv")



# Todays ------------------------------------------------------------------
print(c(last(usa$Date),last(world$Date)))

usa.today <- usa %>% filter(usa$Date == max(usa$Date))
world.today <- world %>% filter(world$Date == max(world$Date))

# Save Data ----------------------------------------------------------

# original shiny app
save(world,file="~/Dropbox/My_Documents_Dropbox/COVID-19/COVID-19-Visualizations/world.RData")
save(usa,file="~/Dropbox/My_Documents_Dropbox/COVID-19/COVID-19-Visualizations/usa.RData")


# Launch Shiny App --------------------------------------------------------

setwd("~/Dropbox/My_Documents_Dropbox/COVID-19/COVID-19-Visualizations")
if(F) rsconnect::deployApp('~/Dropbox/My_Documents_Dropbox/COVID-19/COVID-19-Visualizations')


