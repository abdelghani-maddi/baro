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
  select(sexe, catage, ends_with("recod"), score1, scor_q9_i) %>%
  select(-starts_with("att"), -starts_with("desc"))

# Transformer les colonnes de 1 à 7 en facteurs
df <- df %>%
  mutate_at(vars(1:7), factor)


## Cutting df$scor_q9_i into df$scor_q9_i_rec
df$scor_q9_i <- cut(df$scor_q9_i,
  include.lowest = TRUE,
  right = FALSE,
  dig.lab = 4,
  breaks = c(-2, -1.0625, 0.1875, 2)
)


## Recoding df$scor_q9_i into df$frec_activ_scient
df$frec_activ_scient <- df$scor_q9_i %>%
  fct_recode(
    "Faible" = "[-2,-1.062)",
    "Moyenne" = "[-1.062,0.1875)",
    "Forte" = "[0.1875,2]"
  )


df <- df %>%
  select(-scor_q9_i,-score1)


# Transformer les colonnes de 1 à 7 en facteurs
df <- df %>%
  mutate_at(vars(1:7), factor)



res <- dudi.acm(df, scannf = FALSE, nf = Inf)

explor::explor(res)

