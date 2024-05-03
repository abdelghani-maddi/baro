###############################
# Chargement des packages ----
###############################

library(tidyverse)
library(ade4)
library(factoextra)
library(explor)
library(questionr)
library(FactoMineR)
library(readxl)
library(ade4)
library(dendextend)
library(gtsummary)


###############################
# Chargement des données ----
###############################

baro24 <- read_excel("~/Documents/baro/baro24.xlsx")

###############################
# Préparation des données -----
###############################
# Travailler sur une copie
data <- baro24

# Renommer les colonnes sur les fréquences des activités scientifiques
# Liste des colonnes à renommer
cols_to_rename <- grep("^q9_i", names(data), value = TRUE)

# Nouveaux noms de colonnes
new_names <- paste0("q9_i_", seq_along(cols_to_rename))

# Renommer les colonnes
data <- data %>%
  rename(!!!setNames(cols_to_rename, new_names))

# Convertir les colonnes en facteurs (modalités)
baro24[, grep("^q9_i_", names(baro24))] <- lapply(baro24[, grep("^q9_i_", names(baro24))], factor)

# Fonction pour recoder les modalités en scores
recodage_score <- function(data) {
  # Liste des colonnes à recoder
  cols_to_recode <- grep("^q9_i_", names(data))
  
  # Boucle sur les colonnnes
  for (col in cols_to_recode) {
    data[[col]] <- recode(data[[col]], 
                          "Toutes les semaines" = 2,
                          "1 à 3 fois par mois" = 1,
                          "Ne sait pas" = 0,
                          "Moins souvent" = -1,
                          "Jamais" = -2)
  }
  
  return(data)
}

# Appliquer la fonction pour recoder les modalités en scores
data <- recodage_score(data)

describe(data$q9_i_1)

# Créer une nouvelle colonne contenant la moyenne des scores des colonnes q9_i_...
data$scor_q9_i <- rowMeans(data[, grep("^q9_i_", names(data))])

###############################
# Selection des variables d'intérêt 
# pour réaliser une analyse factorielle et CAH
###############################

df <- data %>%
  select(sexe, catage, ends_with("recod"), score1, scor_q9_i, interet_science, `q22_i3. Dans quelle mesure êtes-vous d'accord ou non avec ces propositions ? - Je préfère échanger avec des personnes qui partagent mes opinions`, `rs4. Quels sont aujourd'hui vos principaux centres d'intérêt ? - Au Total / Les sciences`, `q8. Quelles sont vos principales sources d'informations sur des sujets scientifiques ? / Vous ne vous informez pas sur des sujets scientifiques`) %>%
  select(-starts_with("att"), -starts_with("desc"))

names(df) <- c("sexe", "catage", "diplome_recod", "position_recod", "revfoyer_recod", "sourceinfo_recod", "typo2_recod",
               "score1", "scor_freq_activ_sci", "interet_science", "echang_meme_opinions", "centr_intert_scien", "pas_info_sujets_sci")


## Cutting df$scor_freq_activ_sci into df$scor_q9_i_rec
df$scor_freq_activ_sci <- cut(df$scor_freq_activ_sci,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(-2, -1.0625, 0.1875, 2)
)


## Recoding df$scor_q9_i into df$frec_activ_scient
df$scor_freq_activ_sci <- df$scor_freq_activ_sci %>%
  fct_recode(
    "Faible" = "[-2,-1.062)",
    "Moyenne" = "[-1.062,0.1875)",
    "Forte" = "[0.1875,2]"
  )


# Transformer les colonnes de 1 à 12 en facteurs
df <- df %>%
  mutate_at(vars(1:12), factor)

## Recoding df$diplome_recod
df$diplome_recod <- df$diplome_recod %>%
  fct_recode(
    "Secondaire" = "Bac",
    "Supérieur" = "Bac+2",
    "Supérieur" = "Bac+3",
    "Supérieur" = "Bac+4",
    "Supérieur" = "Bac+5 ou sup",
    "Secondaire" = "BEPC",
    "Secondaire" = "CAP/BEP",
    NULL = "nsp"
  )

## Recoding df$echang_meme_opinions
df$echang_meme_opinions <- df$echang_meme_opinions %>%
  fct_recode(
    NULL = "Ne sait pas",
    "Non" = "Pas du tout d'accord",
    "Oui" = "Plutôt d'accord",
    "Non" = "Plutôt pas d'accord",
    "Oui" = "Tout à fait d'accord"
  )

## Recoding df$interet_science
df$interet_science <- df$interet_science %>%
  fct_recode(
    NULL = "Ne sait pas",
    "Pas intéressé" = "Pas vraiment intéressé",
    "intéressé" = "Plutôt intéressé",
    "intéressé" = "Très intéressé"
  )


res <- dudi.acm(df, scannf = FALSE, nf = Inf)

# Décommenter la ligne ci-dessous pour avoir l'interface interactive
# explor::explor(res)


# calcul de la matrice de distance
md <- dist.dudi(res)

# calcul du dendrogramme
arbre <- hclust(md, method = "ward.D2")

# Représenter le dendrogramme
# Une façon plus visuelle de représenter le dendogramme
library(dendextend)
color_branches(arbre, k = 5) %>% ggplot(labels = FALSE)

# Ou bien avec :

library(factoextra)
fviz_dend(arbre, k = 5, show_labels = FALSE, rect = TRUE)

# saut d'inertie : pour déterminer le nombre de clusters de façon objective et statistique
inertie <- sort(arbre$height, decreasing = TRUE)
plot(inertie[1:20], type = "s")


# déterminer le nombre de classes avec des indicateurs poussés
library(WeightedCluster)
as.clustrange(arbre, md) %>% plot()


# Caractériser les classes ----
df$typo <- cutree(arbre, 4) # fonction cutree : apratenance de chaque observation à chaque classe (ne pas modifier l'ordre des observations dans les différents objets !!!)

# Soit faire un tbl_summary
df %>%
  tbl_summary(include = c(names(df), "typo"), by = "typo") %>%
  add_p() %>%
  separate_p_footnotes()






