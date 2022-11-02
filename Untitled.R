library(tidyverse)
library(googlesheets4)
library(lubridate)
install.packages("openintro")
library(openintro)
install.packages("palmerpenguins")
library(palmerpenguins)
library(maps)
library(ggmap)
install.packages("ggthemes")
library(ggthemes)
gs4_deauth()
theme_set(theme_minimal())
getwd()
setwd("/Users/praveenkumar/desktop")
getwd()
read_csv("train.csv")
india_map = get_stamenmap(bbox = c(left = 69, bottom=6, right = 103, top=40),
                          maptype="toner-hybrid",
                          zoom=6)

ggmap(india_map)

ggmap(india_map)+
  geom_point(train,mapping=aes(x=LATITUDE, y=LONGITUDE), size=0.005,color="blue")



###bhumikas code

india_map = get_stamenmap(bbox = c(left =60.285, bottom=7.842, right = 112.404, top=34.958),
                          maptype="toner", zoom=6)

ggmap(india_map)


