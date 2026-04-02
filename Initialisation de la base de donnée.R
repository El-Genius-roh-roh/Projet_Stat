library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(FactoMineR)
library(naniar)
library(factoextra)
library(tibble)

#On import la base de données
data<-readRDS('data_clean.rds')

#Inversion des variables négatives

data <- data %>%
  mutate(across(c(ST343Q02JA, ST343Q04JA, ST343Q05JA, ST343Q07JA, ST343Q10JA), 
                ~ 6 - .x))
data <- data %>%
  mutate(ST342Q01JA = 5 - ST342Q01JA)

data <- data %>%
  mutate(across(c(ST313Q02JA, ST313Q03JA, ST313Q04JA, ST313Q06JA, ST313Q08JA, ST313Q09JA, ST313Q10JA), ~ 6 - .x))

data = data %>% 
  mutate(across(c(ST345Q01JA, ST345Q03JA, ST345Q04JA, ST345Q07JA, ST345Q10JA), ~ 6 - .x))

data <- data %>%
  mutate(across(c(ST311Q01JA, ST311Q05JA, ST311Q07JA), ~ 6 - .x))

data <- data %>%
  mutate(across(c(ST303Q05JA, ST303Q07JA), ~ 6 - .x))

data <- data %>%
  mutate(across(c(ST307Q04JA, ST307Q06JA, ST307Q07JA, ST307Q10JA), ~ 6 - .x))


NA_individus<-data %>%
  transmute(
    id_ligne = row_number(), 
    nb_na = rowSums(is.na(.))
  ) %>% arrange(.,nb_na)



individus_inf_60<- NA_individus %>% filter(nb_na<60)
indices_lignes<-individus_inf_60$id_ligne
data_moins_na<-data[indices_lignes,]

