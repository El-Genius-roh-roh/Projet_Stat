# ============================================================
# CORRÉLATION DE SPEARMAN – Leadership (ST305) x Coopération (ST343)
# ============================================================

library(tidyverse)
library(corrplot)
library(Hmisc)

# ------------------------------------------------------------
# 1. Labels complets (pour autres usages)
# ------------------------------------------------------------
labels_questions_305 <- c(
  "ST305Q01JA" = "Je suis à l'aise pour prendre le rôle de leader dans un groupe",
  "ST305Q02JA" = "Je sais comment convaincre les autres de faire ce que je veux",
  "ST305Q03JA" = "J'aime diriger les autres",
  "ST305Q04JA" = "Je partage mon opinion",
  "ST305Q05JA" = "Je parle aux autres de ce qui m'importe",
  "ST305Q06JA" = "Je prends des initiatives lorsque je travaille avec mes camarades",
  "ST305Q07JA" = "J'attends que les autres prennent les devants",
  "ST305Q08JA" = "J'ai du mal à influencer les gens",
  "ST305Q09JA" = "Je veux être en charge",
  "ST305Q10JA" = "J'aime être un leader dans ma classe"
)

labels_questions_343 <- c(
  "ST343Q01JA" = "J'aime aider les autres",
  "ST343Q02JA" = "Je m'énerve quand je dois faire des compromis avec les autres",
  "ST343Q03JA" = "Je travaille bien avec les autres",
  "ST343Q04JA" = "Je commence des disputes avec les autres",
  "ST343Q05JA" = "J'évite de travailler avec d'autres élèves",
  "ST343Q06JA" = "Je suis prêt(e) à aider n'importe qui",
  "ST343Q07JA" = "J'ai tendance à être égoïste",
  "ST343Q08JA" = "Je travaille mieux quand je fais partie d'une équipe",
  "ST343Q09JA" = "J'aime coopérer avec mes camarades de classe",
  "ST343Q10JA" = "Je me dispute souvent"
)

# ------------------------------------------------------------
# 2. Labels abrégés pour le corrplot
# ------------------------------------------------------------
labels_court_305 <- c(
  "ST305Q01JA" = "Rôle de leader",
  "ST305Q02JA" = "Convaincre les autres",
  "ST305Q03JA" = "Diriger les autres",
  "ST305Q04JA" = "Paratager mon opinion",
  "ST305Q05JA" = "Parler de ce qui importe",
  "ST305Q06JA" = "Prendre des initiatives",
  "ST305Q07JA" = "Etre pionnier",
  "ST305Q08JA" = "Influencer les gens",
  "ST305Q09JA" = "Vouloir des responsabilités",
  "ST305Q10JA" = "Leader en classe"
)

labels_court_343 <- c(
  "ST343Q01JA" = "Aider les autres",
  "ST343Q02JA" = "Compromis avec autrui",
  "ST343Q03JA" = "Travailler avec autrui",
  "ST343Q04JA" = "Ne pas créer des disputes",
  "ST343Q05JA" = "Éviter le travail commun",
  "ST343Q06JA" = "Aider les gens",
  "ST343Q07JA" = "Tendance coopérative",
  "ST343Q08JA" = "Travail d'équipe",
  "ST343Q09JA" = "Coopérer en classe",
  "ST343Q10JA" = "Se disputer rarement"
)

# ------------------------------------------------------------
# 3. Extraction et renommage avec labels abrégés
# ------------------------------------------------------------
items_305 <- data_moins_na %>% select(all_of(names(labels_court_305)))
items_343 <- data_moins_na %>% select(all_of(names(labels_court_343)))

colnames(items_305) <- labels_court_305
colnames(items_343) <- labels_court_343

# ------------------------------------------------------------
# 4. Scores moyens et corrélation globale
# ------------------------------------------------------------
scores <- tibble(
  score_leadership  = rowMeans(items_305, na.rm = TRUE),
  score_cooperation = rowMeans(items_343, na.rm = TRUE)
)

cor_test <- cor.test(scores$score_leadership,
                     scores$score_cooperation,
                     method = "spearman",
                     exact  = FALSE)

cat("=== Corrélation de Spearman : Leadership x Coopération ===\n")
cat(sprintf("  rho   = %.3f\n", cor_test$estimate))
cat(sprintf("  p-val = %.4f\n", cor_test$p.value))

# ------------------------------------------------------------
# 5. Matrice de corrélation item x item
# ------------------------------------------------------------
mat_complete <- bind_cols(items_305, items_343)

cor_matrix <- rcorr(as.matrix(mat_complete), type = "spearman")

rho_cross  <- cor_matrix$r[1:10, 11:20]
pval_cross <- cor_matrix$P[1:10, 11:20]

# ------------------------------------------------------------
# 6. Visualisation – corrplot
# ------------------------------------------------------------
corrplot(
  rho_cross,
  method      = "color",
  col         = colorRampPalette(c("#FF6347", "white", "#006400"))(200),
  tl.col      = "black",
  tl.srt      = 45,
  tl.cex      = 0.75,
  cl.cex      = 0.8,
  addCoef.col = "black",
  number.cex  = 0.65,
  title       = "Corrélation Leadership × Coopération",
  mar         = c(0, 0, 2, 0),
  p.mat       = pval_cross,
  sig.level   = 0.05,
  insig       = "pch",
  pch.cex     = 1.2
)

