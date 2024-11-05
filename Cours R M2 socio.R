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







