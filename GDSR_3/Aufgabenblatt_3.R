## Vorbereitendes

rm(list=ls()) # Entfernt alle Objekte (Datensätze, Variablen usw.)

# install.packages('nycflights13')

library(tidyverse)
library(nycflights13)

View(flights)

Fluege_df <- na.omit(flights)

## Analyse

# a)

Fluege_df |> 
  filter(dep_delay >120) |> 
  group_by(origin) |> 
  summarize(n=n())

# b)

Fluege_df |> 
  filter(dep_delay > 0) |>
  group_by(origin) |> 
  summarize(
    MW=mean(dep_delay),
    Min = min(dep_delay),
    Max = max(dep_delay),
    Total = sum(dep_delay),
    n=n())

# c)

Fluege_df |> 
  ggplot(aes(x=dep_delay, y=arr_delay, colour = origin)) +
  geom_point() + 
  facet_wrap(~origin)

# d)
 
Fluege_df |> 
  filter(dep_delay>60) |> 
  ggplot(aes(x=dep_delay, y=arr_delay, colour = origin)) +
  geom_point() + 
  facet_wrap(~origin)

# e)

Fluege_df |> 
  lm(formula = arr_delay ~ dep_delay + air_time) |> 
  summary()

# f)

Fluege_df |> 
  filter(dep_delay>60) |>
  filter(origin == 'EWR') |> 
  lm(formula = arr_delay ~ dep_delay + air_time) |> 
  summary()

Fluege_df |> 
  filter(dep_delay>60) |>
  filter(origin == 'JFK') |> 
  lm(formula = arr_delay ~ dep_delay + air_time) |> 
  summary()

Fluege_df |> 
  filter(dep_delay>60) |>
  filter(origin == 'LGA') |> 
  lm(formula = arr_delay ~ dep_delay + air_time) |> 
  summary()


### als Loop:

Fluege_df |> 
  group_by(origin) |> 
  summarize(n=n())


airportliste <- c('EWR', 'JFK', 'LGA')


for (i in airportliste) {
  Fluege_df |> 
    filter(dep_delay>60) |>
    filter(origin == i) |> 
    lm(formula = arr_delay ~ dep_delay + air_time) |> 
    summary() |> 
    print()
}

### aus der Hausaufgabe

# Teilaufgabe g)
# Ermitteln der drei Airlines mit den meisten Flügen

Fluege_df |> 
  group_by(carrier) |> 
  summarize(n=n()) |> 
  arrange(desc(n))

## UA, B6 und EV

Fluege_df |> 
  filter(carrier=='UA') |> 
  lm(formula = air_time ~ distance + origin) |> 
  summary()

Fluege_df |> 
  filter(carrier=='B6') |> 
  lm(formula = air_time ~ distance + origin) |> 
  summary()

Fluege_df |> 
  filter(carrier=='EV') |> 
  lm(formula = air_time ~ distance + origin) |> 
  summary()


