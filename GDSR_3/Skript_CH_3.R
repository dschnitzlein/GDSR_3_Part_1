rm(list=ls()) # Entfernt alle Objekte (Datensätze, Variablen usw.) aus dem Speicher

## Vorbereitung

library(ggplot2)
library(dplyr)
library(palmerpenguins)
library(ggthemes)


## Aufgabe 1

Pinguine <- na.omit(penguins)

Pinguine_neu <- mutate(Pinguine, 
                       body_mass_kg=body_mass_g/1000, 
                       bill_length_cm=bill_length_mm/10
                       )

Pinguine_neu <- filter(Pinguine_neu, species=="Adelie")

body_mass_mean <- mean(Pinguine_neu$body_mass_kg)
bill_length_mean <- mean(Pinguine_neu$bill_length_cm)

ggplot(data=Pinguine_neu, aes(x=bill_length_cm, y=body_mass_kg)) +
  geom_point(aes(colour=species, shape=species)) +
  geom_smooth(method='lm') +
  geom_hline(yintercept=body_mass_mean, colour='blue', linetype=2) +
  geom_vline(xintercept=bill_length_mean, colour='blue', linetype=2) +
  labs(
    title= "Gewicht und Schnabellänge bei Pinguinen",
    subtitle = "Beobachtungen aus dem palmerspenguins Datensatz",
    x = "Schnabellänge in cm",
    y = "Gewicht in kg",
    colour= "Pinguinart",
    shape = "Pinguinart") +
  scale_color_colorblind() +
  theme_bw()


## Aufgabe 2

## Alles in einer Pipe

penguins |> 
  na.omit() |>
  filter(species=='Adelie') |> 
  mutate(
    body_mass_kg=body_mass_g/1000, 
    bill_length_cm=bill_length_mm/10
  ) |>
  mutate(
    body_mass_mean=mean(body_mass_kg), 
    bill_length_mean=mean(bill_length_cm)
  ) |> 
  ggplot(aes(x=bill_length_cm, y=body_mass_kg)) +
    geom_point(aes(colour=species, shape=species)) +
    geom_smooth(method='lm') +
    geom_line(aes(y=body_mass_mean, x=bill_length_cm), colour='blue', linetype=2) +
    geom_line(aes(y=body_mass_kg, x=bill_length_mean), colour='blue', linetype=2) +
    labs(
     title= "Gewicht und Schnabellänge bei Pinguinen",
      subtitle = "Beobachtungen aus dem palmerspenguins Datensatz",
      x = "Schnabellänge in cm",
      y = "Gewicht in kg",
      colour= "Pinguinart",
      shape = "Pinguinart") +
    scale_color_colorblind() +
    theme_bw()

## Besser

penguins_new <- penguins |> 
  na.omit() |> 
  filter(species=='Adelie') |> 
  mutate(
    body_mass_kg=body_mass_g/1000, 
    bill_length_cm=bill_length_mm/10
  )

body_mass_mean <- mean(penguins_new$body_mass_kg)
bill_length_mean <- mean(penguins_new$bill_length_cm)

penguins_new |> 
  ggplot(aes(x=bill_length_cm, y=body_mass_kg)) +
  geom_point(aes(colour=species, shape=species)) +
  geom_smooth(method='lm') +
  geom_hline(yintercept=body_mass_mean, colour='blue', linetype=2) +
  geom_vline(xintercept=bill_length_mean, colour='blue', linetype=2) +
  labs(
    title= "Gewicht und Schnabellänge bei Pinguinen",
    subtitle = "Beobachtungen aus dem palmerspenguins Datensatz",
    x = "Schnabellänge in cm",
    y = "Gewicht in kg",
    colour= "Pinguinart",
    shape = "Pinguinart") +
  scale_color_colorblind() +
  theme_bw()

## Aufgabe 3

penguins |> 
  na.omit() |> 
  mutate(
    body_mass_kg=body_mass_g/1000, 
    flipper_length_cm=flipper_length_mm/10,
    bill_depth_cm=bill_depth_mm/10
  ) |> 
  lm(formula=body_mass_kg ~ flipper_length_cm + bill_depth_cm + sex + bill_depth_cm:sex) |> 
  summary()

penguins |> 
  na.omit() |> 
  mutate(
    body_mass_kg=body_mass_g/1000, 
    flipper_length_cm=flipper_length_mm/10,
    bill_depth_cm=bill_depth_mm/10
  ) |> 
  filter(species=='Adelie') |> 
  lm(formula=body_mass_kg ~ flipper_length_cm + bill_depth_cm + sex + bill_depth_cm:sex) |> 
  summary()


penguins |> 
  na.omit() |> 
  mutate(
    body_mass_kg=body_mass_g/1000, 
    flipper_length_cm=flipper_length_mm/10,
    bill_depth_cm=bill_depth_mm/10
  ) |> 
  filter(species=='Gentoo') |> 
  lm(formula=body_mass_kg ~ flipper_length_cm + bill_depth_cm + sex + bill_depth_cm:sex) |> 
  summary()


penguins |> 
  na.omit() |> 
  mutate(
    body_mass_kg=body_mass_g/1000, 
    flipper_length_cm=flipper_length_mm/10,
    bill_depth_cm=bill_depth_mm/10
  ) |> 
  filter(species=='Chinstrap') |> 
  lm(formula=body_mass_kg ~ flipper_length_cm + bill_depth_cm + sex + bill_depth_cm:sex) |> 
  summary()

## Exkurs: Als Loop

spec_list <- c('Adelie', 'Gentoo', 'Chinstrap') 

for (i in spec_list) {
  penguins |> 
    na.omit() |> 
    mutate(
      body_mass_kg=body_mass_g/1000, 
      flipper_length_cm=flipper_length_mm/10,
      bill_depth_cm=bill_depth_mm/10
    ) |> 
    filter(species==i) |> 
    lm(formula=body_mass_kg ~ flipper_length_cm + bill_depth_cm + sex + bill_depth_cm:sex) |> 
    summary() |> 
    print()
}


