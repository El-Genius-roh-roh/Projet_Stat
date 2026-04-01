library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(FactoMineR)
library(naniar)
library(factoextra)

#On import la base de données
data<-readRDS('data_clean.rds')


NA_individus<-data %>%
  transmute(
    id_ligne = row_number(), 
    nb_na = rowSums(is.na(.))
  ) %>% arrange(.,nb_na)



individus_inf_60<- NA_individus %>% filter(nb_na<60)
indices_lignes<-individus_inf_60$id_ligne
data_moins_na<-data[indices_lignes,]