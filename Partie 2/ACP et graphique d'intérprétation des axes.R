#ACP sur toutes les variables
ACP = PCA(data_moins_na, graph = F)

# Extraction des coordonnées des individus sur les axes
axes_acp <- as.data.frame(ACP$ind$coord)


#------Interprétation--------

#Script qui permet d'afficher les graphiques pour interpréter les axes :

#Je commence par afficher la répartition des contributions de chaque variable
#pour chaque axe
fviz_contrib(ACP, choice = "var", axes = 1)
fviz_contrib(ACP, choice = "var", axes = 2)
fviz_contrib(ACP, choice = "var", axes = 3)
fviz_contrib(ACP, choice = "var", axes = 4)
fviz_contrib(ACP, choice = "var", axes = 5)


#Création d'un dataframe qui contient les contributions de chaque variable
#à chaque dimension
contribution = as.data.frame(ACP$var$contrib)

#Ajout d'une collone qui correspond à la dimension à laquelle la varible à le
#plus contribué
contribution %>% 
  mutate(dim_principale = apply(select(., starts_with("Dim")), 1, which.max))
