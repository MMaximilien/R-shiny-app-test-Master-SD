# Import des librairies
library(tidyverse)
library(readxl)
library(ggiraph)
library(DT)
library(gt)

library(shiny)

# Imports des données
df <- read_csv2("data/data_allocine.csv")
#view(df)
glimpse(df)
str(df)

# Nettoyage des données
df_fr <- filter(df, nationalite == "français")
df_fr <- arrange(df_fr, desc(duree))

df_correspondance <- read_excel("data/correspondances_allocine.xlsx")
#view(df_correspondance)
glimpse(df_correspondance)
str(df_correspondance)

# Jointure avec allocine
df_allocine <- df %>% 
  left_join(df_correspondance, by = c("nationalite" = "nationalité"))

# Traitements des données
df_allocine <- df_allocine %>% 
  select(-recompenses) %>% 
  mutate(annee_sortie = year(date_sortie))
