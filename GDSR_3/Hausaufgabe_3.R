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
  filter(air_time >120) |> 
  group_by(origin) |> 
  summarize(n=n())

# b)

Fluege_df |> 
  group_by(origin) |> 
  summarize(
    MW=mean(air_time),
    Min = min(air_time),
    Max = max(air_time),
    Total = sum(air_time),
    n=n())

# c)

Fluege_df |> 
  group_by(origin) |> 
  filter(air_time==max(air_time)) |> 
  select(origin, air_time, dest)

# d)

Fluege_df |> 
  ggplot(aes(x=origin, y=air_time)) +
  geom_boxplot() +
  labs(
    title = "Flugzeit nach Abflugflughafen",
    subtitle = "Daten aus dem nycflights13 Paket",
    xlab = "Abflugflughafen",
    ylab = "Flugzeit in Minuten"
  ) +
  theme_classic()

# e)

Fluege_df |> 
  filter(dep_delay>60) |> 
  ggplot(aes(x=origin, y=air_time)) +
    geom_boxplot() +
    labs(
      title = "Flugzeit nach Abflugflughafen",
      subtitle = "Daten aus dem nycflights13 Paket",
      xlab = "Abflugflughafen",
      ylab = "Flugzeit in Minuten"
    ) +
    theme_classic()

# f) 

Fluege_df |> 
  lm(formula = air_time ~ distance + origin) |> 
  summary()

# g)
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
