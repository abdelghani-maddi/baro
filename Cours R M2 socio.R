library(gtsummary)
library(tidyverse)

df_baro <- baro24 %>%
  select(id, starts_with("q29_")) %>%
  as.data.frame()


# Vecteur avec les nouveaux noms de colonnes correspondants
new_column_names <- c(
  "IA_Presse", "IA_Erreurs", "IA_DiagMed", "IA_Voiture", 
  "IA_Analyser", "IA_Infos", "IA_Connais", "IA_Art", "IA_Justice", 
  "IA_Texte", "IA_Avion", "IA_Traitement"
)

# Vecteur avec les noms d'origine des colonnes dans data
original_column_names <- c(
  "q29_i1. Personnellement, faites-vous confiance ou non à l'utilisation de l'IA pour : - Rédiger un article de presse",
  "q29_i2. Personnellement, faites-vous confiance ou non à l'utilisation de l'IA pour : - Détecter des erreurs dans des textes ou des programmes informatiques",
  "q29_i3. Personnellement, faites-vous confiance ou non à l'utilisation de l'IA pour : - Réaliser des diagnostics médicaux",
  "q29_i4. Personnellement, faites-vous confiance ou non à l'utilisation de l'IA pour : - Piloter des voitures",
  "q29_i5. Personnellement, faites-vous confiance ou non à l'utilisation de l'IA pour : - Analyser des quantités importantes de données ou de textes",
  "q29_i6. Personnellement, faites-vous confiance ou non à l'utilisation de l'IA pour : - Trouver des informations",
  "q29_i7. Personnellement, faites-vous confiance ou non à l'utilisation de l'IA pour : - Approfondir ses connaissances",
  "q29_i8. Personnellement, faites-vous confiance ou non à l'utilisation de l'IA pour : - Créer des œuvres artistiques originales",
  "q29_i9. Personnellement, faites-vous confiance ou non à l'utilisation de l'IA pour : - Rendre des décisions de justice",
  "q29_i10. Personnellement, faites-vous confiance ou non à l'utilisation de l'IA pour : - Ecrire un texte",
  "q29_i11. Personnellement, faites-vous confiance ou non à l'utilisation de l'IA pour : - Piloter un avion",
  "q29_i12. Personnellement, faites-vous confiance ou non à l'utilisation de l'IA pour : - Prescrire un traitement ou des médicaments"
)


# Appliquer les nouveaux noms en remplaçant les anciens noms
names(df_baro) <- new_column_names[match(names(df_baro), original_column_names)]


# Définir les niveaux souhaités pour les questions de type q29
levels_q29 <- c("Pas du tout confiance", "Plutôt pas confiance", "Plutôt confiance", "Tout à fait confiance")

# Boucle pour réordonner toutes les colonnes qui commencent par "q29"
for (col in names(df_baro)) {
  if (startsWith(col, "IA_")) {
    df_baro[[col]] <- factor(df_baro[[col]], levels = levels_q29)
  }
}


df_baro %>% 
  tbl_summary(include = starts_with("IA_"))


###########
library(ggstats)
library(labelled)

# Appliquer les labels aux colonnes
df_baro <- set_variable_labels(df_baro,
                            IA_Presse = "IA_Presse",
                            IA_Erreurs = "IA_Erreurs",
                            IA_DiagMed = "IA_DiagMed",
                            IA_Voiture = "IA_Voiture",
                            IA_Analyser = "IA_Analyser",
                            IA_Infos = "IA_Infos",
                            IA_Connais = "IA_Connais",
                            IA_Art = "IA_Art",
                            IA_Justice = "IA_Justice",
                            IA_Texte = "IA_Texte",
                            IA_Avion = "IA_Avion",
                            IA_Traitement = "IA_Traitement"
)

names(df_baro)[1] <- "id"


gglikert(df_baro, include = IA_Presse:IA_Traitement)



library(labelled)
niveaux <- c(
  "Pas du tout d'accord",
  "Plutôt pas d'accord",
  "Ni d'accord, ni pas d'accord",
  "Plutôt d'accord",
  "Tout à fait d'accord"
)
set.seed(42)
df <-
  tibble(
    groupe = sample(c("A", "B"), 150, replace = TRUE),
    q1 = sample(niveaux, 150, replace = TRUE),
    q2 = sample(niveaux, 150, replace = TRUE, prob = 5:1),
    q3 = sample(niveaux, 150, replace = TRUE, prob = 1:5),
    q4 = sample(niveaux, 150, replace = TRUE, prob = 1:5),
    q5 = sample(c(niveaux, NA), 150, replace = TRUE),
    q6 = sample(niveaux, 150, replace = TRUE, prob = c(1, 0, 1, 1, 0))
  ) |> 
  mutate(across(q1:q6, ~ factor(.x, levels = niveaux))) |> 
  set_variable_labels(
    q1 = "Première question",
    q2 = "Seconde question",
    q3 = "Troisième question",
    q4 = "Quatrième question",
    q5 = "Cinquième question",
    q6 = "Sixième question"
  )

library(gtsummary)
df |> 
  tbl_summary(include = q1:q6)


df |> 
  tbl_likert(
    include = q1:q6
  )


library(ggstats)
df |> 
  gglikert(
    include = q1:q6,
    totals_include_center = TRUE,
    sort = "ascending"
  ) +
  guides(
    fill = guide_legend(nrow = 2)
  )
