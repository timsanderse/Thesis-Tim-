# Thesis-Tim-
-
# This is from Rstudio 

#Thesis Tim 
View(Einddataset)

## Library
library(ggplot2)
library(dplyr)
library(tidyverse)
library(rstudioapi)
library(ggmap)

install.packages("maps")
install.packages("rstudioapi")
install.packages("ggmap")

#Descriptive Vaccinatiekaart
register_google(key = "AIzaSyB2ksiIg13GxMOJZIikwUaqnyKVUpgz4_4")

mapcrime <- ggmap(get_googlemap(center = c(lon =-2.9437 , lat = 53.45),
                              zoom = 10,
                              maptype ='terrain',
                              color = 'color'))
print(mapcrime)

mean(Einddataset$Gevaccineerd)

## Mediaanleeftijd op Gevaccineerd 
ggplot(data = Einddataset, aes(x = Mediaanleeftijd, y = Gevaccineerd)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

leeftijdlm <- lm(Gevaccineerd ~ Mediaanleeftijd, data = Einddataset)

summary(leeftijdlm)

## Herkomst op Gevaccineerd 
Einddataset$totaal <- Einddataset$Belgische.herkomst + Einddataset$niet.Belgische.herkomst
Einddataset$PercentageNietBelg <- round(Einddataset$niet.Belgische.herkomst / Einddataset$totaal * 100) 

ggplot(data = Einddataset, aes(x = PercentageNietBelg, y = Gevaccineerd)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

herkomstlm <- lm(Gevaccineerd ~ PercentageNietBelg, data = Einddataset)

summary(herkomstlm)

## Model 
model <- lm(Gevaccineerd ~ PercentageNietBelg +  Mediaanleeftijd, data=Einddataset)
summary(model)
