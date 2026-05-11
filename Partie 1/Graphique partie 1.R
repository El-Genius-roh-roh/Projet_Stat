# ------------------------------------------------------------
# 1. Correspondance variables -> labels français
# ------------------------------------------------------------
labels_questions <- c(
  "ST305Q01JA" = "Je suis à l'aise pour mener un groupe",
  "ST337Q02JA" = "Cours/activités d'écriture créative",
  "ST337Q03JA" = "Cours/activités musicales",
  "ST337Q04JA" = "Club de débat",
  "ST337Q05JA" = "Cours/activités de théâtre",
  "ST337Q06JA" = "Publications",
  "ST337Q07JA" = "Club scientifique",
  "ST337Q08JA" = "Cours/activités de programmation informatique"
)

# ------------------------------------------------------------
# 2. Préparation des données
# ------------------------------------------------------------
plot_data <- data_moins_na %>%
  select(starts_with("ST337")) %>%
  pivot_longer(cols = everything(), names_to = "Question", values_to = "Reponse") %>%
  mutate(
    # Remplacement des codes par les labels français
    Question = recode(Question, !!!labels_questions),
    # Modalités
    Reponse = factor(Reponse,
                     levels = c(5, 4, 3, 2, 1, 6),
                     labels = c("Tous les jours ou presque",
                                "Environ une à deux fois par semaine",
                                "Environ une à deux fois par mois",
                                "Environ une à deux fois par an",
                                "Jamais ou presque jamais",
                                "Non disponible dans l'établissement"))
  ) %>%
  filter(!is.na(Reponse)) %>%
  count(Question, Reponse) %>%
  group_by(Question) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# ------------------------------------------------------------
# 3. Graphique
# ------------------------------------------------------------
ggplot(plot_data, aes(x = Question, y = pct, fill = Reponse)) +
  geom_col(color = "black", size = 0.2, width = 0.8) +
  geom_text(aes(label = paste0(round(pct * 100, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(
    values = c("Tous les jours ou presque"                = "#006400",
               "Environ une à deux fois par semaine"      = "#4BB662",
               "Environ une à deux fois par mois"         = "#A8D08D",
               "Environ une à deux fois par an"           = "#FFB38A",
               "Jamais ou presque jamais"                 = "#FF6347",
               "Non disponible dans l'établissement"      = "#AAAAAA"),
    na.translate = FALSE
  ) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  labs(
    title = "Fréquence de participation aux activités extrascolaires",
    x     = NULL,
    y     = "Proportion des réponses",
    fill  = "Fréquence"
  ) +
  theme_minimal() +
  theme(
    panel.grid      = element_blank(),
    axis.text.y     = element_text(face = "bold", size = 9),
    legend.position = "bottom",
    legend.text     = element_text(size = 8),
    plot.title      = element_text(face = "bold", size = 12)
  ) +
  guides(fill = guide_legend(nrow = 2))