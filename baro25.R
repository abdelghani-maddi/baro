# Packages -----

library(tidyverse)
library(ade4)
library(factoextra)
library(explor)
library(questionr)
library(FactoMineR) # pas nécessaire comme j'ai utilisé ade4
library(readxl)
library(dendextend)
library(gtsummary)
library(readxl)
library(GGally)     # Pour des graphiques de corrélations et autres visualisations
library(effects)    # Pour calculer et visualiser les effets marginaux
library(ggeffects)  # Pour visualiser les effets marginaux des modèles


# Les data ----

baro25 <- read_excel("~/Documents/Documents/Recherche/Baro/baro25_export.xlsx")


# Préparer les données ----

##
baro_enq1 <- baro25 %>% # Enquête 1
  filter(echantillon_main == "Oui") %>%
  select(serial_number, respondent_id, poids1, scoreatt, scoreali, scoreesprit, regions_nouvelles_en_13, habitez_vous_2, genre, tranche_dage,
         csp_2, en_tenant_compte_de_toutes_les_ressources_de_votre_foyer_cest_a_dire_des_salaires_nets_allocations_familiales_allocations_chomage_retraites_pensions_et_autres_revenus_nets_dans_quelle_tranche_se_situent_les_revenus_mensuels_nets_de_votre_foyer,
         diplome, autopol)
names(baro_enq1) <- c("num", "id", "poids", "scoreatt", "scoreali", "scoreesprit", "region", "habitat", "genre", "age", "csp", "revenu", "diplome", "autopol")   

##
baro_enq2 <- baro25 %>% # Enquête 2
  filter(echantillon_15_24_ans == "Oui") %>%
  select(serial_number, respondent_id, poids2, starts_with("score"), regions_nouvelles_en_13, habitez_vous_2, genre, tranche_dage,
         csp_2, en_tenant_compte_de_toutes_les_ressources_de_votre_foyer_cest_a_dire_des_salaires_nets_allocations_familiales_allocations_chomage_retraites_pensions_et_autres_revenus_nets_dans_quelle_tranche_se_situent_les_revenus_mensuels_nets_de_votre_foyer,
         diplome, autopol)

names(baro_enq2) <- c("num", "id", "poids", "scoreatt", "scoreali", "scoreesprit", "region", "habitat", "genre", "age", "csp", "revenu", "diplome", "autopol")   


# Explorer les données -----

baro_enq1 %>%
  select(-num, -id, -poids) %>%
  tbl_summary()

# Analyse des données -----
## Régression -----


## Reordering baro_enq1$csp
baro_enq1$csp <- baro_enq1$csp %>%
  fct_relevel(
    "Ouvriers s5=23-26", "Employés s5=18-22", "Cadres et professions intellectuelles supérieures s5=5-10",
    "Professions intermédiaires s5=11-17", "Artisans, commerçants et chefs d'entreprise s5=2-4",
    "Retraités s5=27-32", "Autres personnes sans activité professionnelle s5=33-35",
    "Agriculteurs exploitants s5=1"
  )

## Recoding baro_enq1$autopol
baro_enq1$autopol <- baro_enq1$autopol %>%
  fct_recode(
    NULL = "NR",
    NULL = "NSP"
  )

## Recoding baro_enq1$diplome
baro_enq1$diplome <- baro_enq1$diplome %>%
  fct_recode(
    NULL = "NR"
  )

## Recoding baro_enq1$habitat
baro_enq1$habitat <- baro_enq1$habitat %>%
  fct_recode(
    NULL = "Ne sait pas"
  )


## Recoding baro_enq1$region
baro_enq1$region <- baro_enq1$region %>%
  fct_recode(
    NULL = "Corse"
  )


## Recoding baro_enq1$revenu
baro_enq1$revenu <- baro_enq1$revenu %>%
  fct_recode(
    "5 000 Euros et plus par mois" = "5 000 Euros à 7499 Euros par mois",
    "5 000 Euros et plus par mois" = "7500 Euros et plus par mois"
  )

## Reordering baro_enq1$revenu
baro_enq1$revenu <- baro_enq1$revenu %>%
  fct_relevel(
    "De 2 000 à 2 999 Euros par mois", "Je ne souhaite pas répondre",
    "Moins de 750 Euros par mois", "De 750 à 999 Euros par mois",
    "De 1 000 à 1 499 Euros par mois", "De 1 500 à 1 999 Euros par mois",
    "De 3 000 à 3 499 Euros par mois", "De 3 500 à 4 999 Euros par mois",
    "5 000 Euros et plus par mois"
  )


## Recoding baro_enq1$csp
baro_enq1$csp <- baro_enq1$csp %>%
  fct_recode(
    NULL = "Agriculteurs exploitants s5=1"
  )

## Reordering baro_enq1$diplome
baro_enq1$diplome <- baro_enq1$diplome %>%
  fct_relevel(
    "Sans diplôme", "CAP/BEP", "Bac", "Bac pro", "Bac +1 ou 2",
    "Bac + 3 ou 4", "Bac +5 ou plus"
  )

## Reordering baro_enq1$age
baro_enq1$age <- baro_enq1$age %>%
  fct_relevel(
    "65+", "18-20", "21-24", "25-34", "35-49", "50-64"
  )


## Reordering baro_enq1$region
baro_enq1$region <- baro_enq1$region %>%
  fct_relevel(
    "Ile de France", "Auvergne - Rhône - Alpes", "Bourgogne - Franche - Comté",
    "Bretagne", "Centre", "Grand-Est (Alsace - Champagne - Ardennes - Lorraine)",
    "Hauts de France (Nord - Pas de Calais - Picardie)", "Normandie (Basse - Normandie - Haute - Normandie)",
    "Nouvelle Aquitaine (Aquitaine - Limousin - Poitou - Charente)",
    "Occitanie (Languedoc - Roussillon - Midi - Pyrénées)", "Pays de la Loire",
    "Provence - Alpes - Côte d'Azur"
  )


baro_enq1 <- na.omit(baro_enq1)

###########################################

###########################################


reg_att1 <- glm(scoreatt ~ scoreali + scoreesprit + genre + age + autopol + revenu + habitat +  csp + diplome + region,
                family = gaussian,
                data = baro_enq1,
                weights = poids
                )

reg_att1 %>%
  tbl_regression() %>%
  add_global_p()

# Visualisation des coefficients avec ggcoef_model
ggcoef_model(reg_att1, exponentiate = TRUE)


# Validation pas à pas
reg_att1V2 <-  step(reg_att1)


# Comparaison entre le modèle complet et le modèle simplifié
ggcoef_compare(
  list("modèle complet" = reg_att1, "modèle simplifié" = reg_att1V2),
  exponentiate = TRUE,
  type = "f"
)




