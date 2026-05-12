#ACP sur toutes les variables
ACP = PCA(data_moins_na, graph = F)

#Affichage de la variance expliquée par chaque axe

fviz_eig(ACP, ncp = 10, addlabels = TRUE, y = "Pourcentage de variance expliquée",
         main = "Variance expliquée par chaque dimension") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
        plot.title = element_text(hjust = 0.5, face = "bold")) 
# angle = 90 fait pivoter le texte à la verticale


# Extraction des coordonnées des individus sur les axes
axes_acp <- as.data.frame(ACP$ind$coord)


#------Interprétation--------

# 1. On extrait les contributions (assure-toi que l'objet ACP existe)
contribs <- as.data.frame(ACP$var$contrib)

# 2. On trie par la première colonne (Axe 1) et on prend les 10 premières lignes
top10_dim1 <- contribs[order(-contribs[, 1]), ][1:5, ]

# 3. On trie par la deuxième colonne (Axe 2) et on prend les 10 premières lignes
top10_dim2 <- contribs[order(-contribs[, 2]), ][1:5, ]

# 4. On fusionne les deux listes
tableau_final <- unique(rbind(top10_dim1, top10_dim2))

# 5. On arrondit pour que ce soit joli
tableau_final <- round(tableau_final[, 1:2], 2)

# 6. On affiche
print(tableau_final)


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
  filter(Dim_principale == 4)


# --------Etude des variables sur les graphs de l'ACP---------


var_axe1 = contribution %>%
  filter(Dim_principale == 1) %>%
  slice_max(Dim.1, n = 10) %>%
  pull(variable)

var_axe2 = contribution %>%
  filter(Dim_principale == 2) %>%
  slice_max(Dim.2, n = 10) %>%
  pull(variable)

# Extraction des coordonnées des variables
var_coords <- as.data.frame(ACP$var$coord) %>%
  rownames_to_column("variable") %>%
  filter(variable %in% c(var_axe1, var_axe2)) %>%
  mutate(groupe = ifelse(variable %in% var_axe1, 
                         "Variables qui contribuent à l'axe 1",
                         "Variables qui contribuent à l'axe 2")) %>%
  mutate(variable = recode(variable,
                           "ST341Q05JA" = "Je vois de la beauté partout",
                           "ST342Q03JA" = "Je trouve qu'avoir des nouvelles idées est satisfaisant",
                           "ST340Q06JA" = "J'aime réflèchir à des nouvelles façon de résoudre des problèmes",
                           "ST340Q01JA" = "Faire quelque chose de créatif est satisfaisant",
                           "ST340Q09JA" = "Je peux proposer plusieurs solutions à un problème",
                           "ST338Q06JA" = "Je lis des journaux",
                           "ST338Q07JA" = "Participation club de science",
                           "ST338Q05JA" = "Participation club de théâtre",
                           "ST338Q04JA" = "Participation club de débat",
                           "ST338Q02JA" = "Participation atelier d'écriture"
  ))

# Cercle
cercle <- data.frame(
  x = cos(seq(0, 2*pi, length.out = 100)),
  y = sin(seq(0, 2*pi, length.out = 100))
)

ggplot(var_coords) +
  geom_path(data = cercle, aes(x, y), color = "grey70") +
  geom_segment(aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2, color = groupe),
               arrow = arrow(length = unit(0.2, "cm"))) +
  scale_color_manual(
    values = c("Variables qui contribuent à l'axe 1" = "#E06C6C", 
               "Variables qui contribuent à l'axe 2" = "#5B9BD5"),
    labels = c("Variables qui contribuent à l'axe 1" = "10 Variables qui contribuent le plus à l'axe 1",
               "Variables qui contribuent à l'axe 2" = "10 Variables qui contribuent le plus à l'axe 2")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  coord_fixed() +
  labs(title = "Les 10 variables les mieux représentées sur respectivement l'axe 1 et 2",
       x = paste0("Dim1 (", round(ACP$eig[1,2], 1), "%)"),
       y = paste0("Dim2 (", round(ACP$eig[2,2], 1), "%)"),
       color = "Type de contribution") + # Change le titre de la légende ici
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")



var_axe3 = contribution %>% 
  filter(Dim_principale == 3) %>% 
  pull(variable)
fviz_pca_var(ACP, axes = c(3, 4), select.var = list(name = var_axe3), repel = T)

var_axe4 = contribution %>% 
  filter(Dim_principale == 4) %>% 
  pull(variable)
fviz_pca_var(ACP, axes = c(3, 4), select.var = list(name = var_axe4), repel = T)

var_axe5 = contribution %>% 
  filter(Dim_principale == 5) %>% 
  pull(variable)
fviz_pca_var(ACP, axes = c(4, 5), select.var = list(name = var_axe5), repel = T)



# -------Nouvelle base de donnée avec comme variables les axes de l'ACP-------


dataACP = as.data.frame(ACP$ind$coord) %>% 
  rename(Créativité = Dim.1,
         Pratique_activités = Dim.2,
         Emotionnel_froid = Dim.3,
         Activité_créative_et_Emotionel_social = Dim.4,
         Encouragement_de_environement_à_la_créativité_Leardership = Dim.5)


# Méthode du coude pour voir combien de cluster on prend. Je dois faire un échantillon sinon c'est trop gros
dataACP_echantillon <- dataACP %>% slice_sample(n = 10000)
fviz_nbclust(dataACP, kmeans, method = "wss") +
  labs(title = "Méthode du coude")

#Méthode du coude
inertie <- sapply(1:10, function(k) {
  kmeans(dataACP, centers = k, nstart = 10)$tot.withinss
})

plot(1:10, inertie, type = "b", 
     xlab = "Nombre de clusters", 
     ylab = "Inertie intra-classe",
     main = "Méthode du coude")


#Algo k-means (j'ai estimer que 4 clusters c'était bien)
km <- kmeans(dataACP, centers = 4, nstart = 25, iter.max = 100)

#Ajout du numéro du cluster correspondant pour chaque variable
dataACP = dataACP %>% 
  mutate(cluster = as.factor(km$cluster))



