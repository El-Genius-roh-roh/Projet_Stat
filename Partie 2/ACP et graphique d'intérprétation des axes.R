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
contribution = as.data.frame(ACP$var$contrib) %>% 
  rownames_to_column("variable") 
# Cette ligne permet juste de nommer la collone qui donne
# le nom aux lignes (c'est à dire la collone des variables)

#Ajout d'une collone qui correspond à la dimension à laquelle la varible à le
#plus contribué + ajout d'une collone qui indique la valeur de la plus grande 
#contribution
contribution = contribution %>% 
  mutate(Dim_principale = apply(select(., Dim.1, Dim.2, Dim.3, Dim.4, Dim.5), 1, which.max)) %>% 
  mutate(Plus_grande_contrib = apply(select(., Dim.1, Dim.2, Dim.3, Dim.4, Dim.5), 1, max)) %>% 
  filter(Plus_grande_contrib > 0.6) #Seuil


#Affichage des dimensions des variables qui ont le contribution maximal sur l'axe 1
contribution %>% 
  filter(Dim_principale == 1)


# --------Etude des variables sur les graphs de l'ACP---------


#Création d'un vecteur qui contient le nom de toutes les variables qui ont pour
#contribution max l'axe 1

var_axe1 = contribution %>% 
  filter(Dim_principale == 1) %>% 
  pull(variable) #Cette ligne permet de renvoyer un vecteur qui contient
# seuelement ce qui se trouve sur la collonne "variable" (dans notre cas c'est 
# le nom de variable)


fviz_pca_var(ACP, axes = c(1, 2), select.var = list(name = var_axe1), repel = T)
#repel = T évite juste que les variables se chevauchent


#On fait la même pour les autres axes

var_axe2 = contribution %>% 
  filter(Dim_principale == 2) %>% 
  pull(variable)
fviz_pca_var(ACP, axes = c(1, 2), select.var = list(name = var_axe2), repel = T)


var_axe3 = contribution %>% 
  filter(Dim_principale == 3) %>% 
  pull(variable)
fviz_pca_var(ACP, axes = c(3, 4), select.var = list(name = var_axe3), repel = T)

var_axe4 = contribution %>% 
  filter(Dim_principale == 4) %>% 
  pull(variable)
fviz_pca_var(ACP, axes = c(3, 4), select.var = list(name = var_axe4), repel = T)
