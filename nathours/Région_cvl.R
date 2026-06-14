# ---- Chargement bibliothèques  ----
library(dplyr)
library(stringr)

# ---- Importation données ----
pop <- read.csv2("population_francaise_communes.csv", fileEncoding = "ISO-8859-1", header = TRUE)

# ---- Vérification colonnes ----
colnames(pop) <- c("Code.region", "Nom.region", "Code.departement", "Code.arrondissement",
                   "Code.canton", "Code.commune", "Commune",
                   "Population.municipale", "Population.comptee", "Population.totale")

# ---- Nettoyage données ----
pop$Population.totale <- as.numeric(str_remove_all(pop$Population.totale, " "))

# ---- Extraction  données région centre val de loire ----
donnees <- pop %>%
  filter(Code.region == 24) %>%
  select(Code.departement, Commune, Population.totale)

# Données de base
U <- donnees
N <- nrow(U)
T <- sum(U$Population.totale, na.rm = TRUE)
cat("Nombre total de communes:", N, "\n")
cat("Population totale exacte (T):", T, "\n")

#Fonction d'échantillonnage aléatoire simple
realiser_SRS <- function(seed = 1234) {
  set.seed(seed)
  n <- 100
  E <- U[sample(1:N, n), ]
  
  # Moyenne 
  xbar <- mean(E$Population.totale)
  
  #  IDC 95%
  ic <- t.test(E$Population.totale)$conf.int
  
  # Estimation
  T_est <- xbar * N
  idc_T <- ic * N
  marge <- (idc_T[2] - idc_T[1]) / 2
  
  return(data.frame(
    T_exact = T,
    T_estimee = T_est,
    Borne_inf = idc_T[1],
    Borne_sup = idc_T[2],
    Marge_erreur = marge,
    Contient_T = (T >= idc_T[1] & T <= idc_T[2])
  ))
}

#Répétition du SRS 10 fois
set.seed(123)
seeds <- sample(1:1000, 10)
resultats_SRS <- do.call(rbind, lapply(seeds, realiser_SRS))

# Résumé 
print(resultats_SRS)
cat("Proportion d'IDC contenant T :", mean(resultats_SRS$Contient_T), "\n")

# Strates par quartiles 
donnees$strate <- cut(donnees$Population.totale,
                      breaks = quantile(donnees$Population.totale, probs = seq(0, 1, 0.25), na.rm = TRUE),
                      labels = c("1", "2", "3", "4"),
                      include.lowest = TRUE)

#Préparation pour stratifié 
datastrat <- donnees
Nh <- table(datastrat$strate)
n <- 100
nh <- round(n * Nh / sum(Nh))
if (sum(nh) != n) nh[which.max(nh)] <- nh[which.max(nh)] + (n - sum(nh))

# Fonction d’échantillonnage 
realiser_STRAT <- function(seed = 1234) {
  set.seed(seed)
  ech <- datastrat %>%
    group_by(strate) %>%
    group_split() %>%
    Map(function(df, size) df[sample(1:nrow(df), size), ], ., nh)
  
  moyennes <- sapply(ech, function(df) mean(df$Population.totale))
  variances <- sapply(ech, function(df) var(df$Population.totale))
  gh <- Nh / N
  fh <- nh / Nh
  
  Xbarst <- sum(gh * moyennes)
  varXbarst <- sum((gh^2) * (1 - fh) * variances / nh)
  se <- sqrt(varXbarst)
  
  # IDC 95%
  ic <- c(Xbarst - qnorm(0.975) * se, Xbarst + qnorm(0.975) * se)
  T_est <- Xbarst * N
  idc_T <- ic * N
  marge <- (idc_T[2] - idc_T[1]) / 2
  
  return(data.frame(
    T_exact = T,
    T_estimee = T_est,
    Borne_inf = idc_T[1],
    Borne_sup = idc_T[2],
    Marge_erreur = marge,
    Contient_T = (T >= idc_T[1] & T <= idc_T[2])
  ))
}

#  Répétition du STRAT 10 fois
resultats_STRAT <- do.call(rbind, lapply(seeds, realiser_STRAT))

# Résumé 
print(resultats_STRAT)
cat("Proportion d'IDC contenant T (STRAT):", mean(resultats_STRAT$Contient_T), "\n")

#Comparaison des marges d'erreur
cat("Marge moyenne SRS:", round(mean(resultats_SRS$Marge_erreur)), "\n")
cat("Marge moyenne STRAT:", round(mean(resultats_STRAT$Marge_erreur)), "\n")
cat("Réduction en %:", round(100 * (1 - mean(resultats_STRAT$Marge_erreur) / mean(resultats_SRS$Marge_erreur)), 2), "%\n")

