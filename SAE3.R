library(sampling)
#import du fichier csv
data <- read.csv2("population_francaise_communes.csv", fileEncoding = "latin1")

# Filtrage des données pour la région "Centre-Val de Loire"
donnees <- subset(data, `Nom.de.la.région` == "Centre-Val de Loire")
donnees=subset(donnees,select =c(Commune,Population.totale,Code.département))
head(donnees)


#Variable U avec le nombre communes de Centre-Val de Loire
U=donnees$Commune
head(U)

#nombre de commune de Centre val de loire
N <- length(U)
print(N)
#Donc il y'a 1757 communes dans cette region

T <- sum(donnees$Population.totale, na.rm = TRUE)
print(T)
#la population totale est de 2,6millions d'habitant

#Tirage aléatoire simple d'un echantillon de taille n=100
n=100
E=sample(U,n)
head(E)
donnees_unique <- donnees[!duplicated(donnees$Commune), ]
# toutes les donnees avec les 50 communes de l’\’echantillon
donnees1= donnees[donnees$Commune %in% E, ]
head(donnees1)

# moyenne d’echantillon
xbar= mean(donnees1$Population.totale)
xbar

# IDC de \mu
idcmoy = t.test(donnees1$Population.totale)$conf.int
idcmoy

# Nbre d’habitants total estim\’e
T_est = N*xbar
T_est

# IDC de T
idcT = idcmoy*N
idcT

# marge d’erreur
marge=(idcT[2]-idcT[1])/2
marge

# ---------------------------------------------------
# SAE - Partie 1.2 : Sondage aléatoire stratifié
# ---------------------------------------------------

# 1. On utilise les mêmes données filtrées que précédemment
# Création des strates par quantiles de la Population.totale

# Vérification des quantiles
summary(donnees$Population.totale)

# Création de 4 strates selon les quantiles
donnees$strate = cut(donnees$Population.totale,
                     breaks = c(0, 230, 500, 1350, 45000),
                     include.lowest = TRUE,
                     labels = c(1, 2, 3, 4))

# Nouvelle table avec strates
datastrat = donnees
head(datastrat)

datastrat2 = datastrat[order(datastrat$strate), ]
head(datastrat2)


# 2. Taille totale de la population et effectif par strate
N = nrow(datastrat2)
Nh = table(datastrat2$strate)
gh = Nh / N  # poids des strates
n = 100

# Taille de l’échantillon dans chaque strate (proportionnelle)
nh=round(c(n*Nh[1]/N, n*Nh[2]/N, n*Nh[3]/N, n*Nh[4]/N))
nh = as.numeric(round(n * Nh / N))

# Vérifier si la somme fait bien 100 (sinon ajuster manuellement)
sum(nh)
nh  # Exemple : nh = c(25, 25, 25, 25) si arrondis simples
# 3. Tirage stratifié (utilisation du package "sampling")

# Tirage sans remise
st = strata(datastrat2, stratanames = c("strate"), size = nh, method = "srswr")

data_ech = getdata(datastrat2, st)

# Vérification
head(data_ech)
nrow(data_ech)

# 4. Définir les 4 sous-échantillons
ech1 = subset(data_ech, strate == 1)
ech2 = subset(data_ech, strate == 2)
ech3 = subset(data_ech, strate == 3)
ech4 = subset(data_ech, strate == 4)

# Moyennes et variances par strate
m1 = mean(ech1$Population.totale); v1 = var(ech1$Population.totale)
m2 = mean(ech2$Population.totale); v2 = var(ech2$Population.totale)
m3 = mean(ech3$Population.totale); v3 = var(ech3$Population.totale)
m4 = mean(ech4$Population.totale); v4 = var(ech4$Population.totale)

# 5. Estimation de la moyenne globale µ
Xbar_strat = (Nh[1]*m1 + Nh[2]*m2 + Nh[3]*m3 + Nh[4]*m4) / N

# Estimation de la variance de la moyenne
var_Xbar = (gh[1]^2 * (1 - nh[1]/Nh[1]) * v1 / nh[1]) +
  (gh[2]^2 * (1 - nh[2]/Nh[2]) * v2 / nh[2]) +
  (gh[3]^2 * (1 - nh[3]/Nh[3]) * v3 / nh[3]) +
  (gh[4]^2 * (1 - nh[4]/Nh[4]) * v4 / nh[4])

# Intervalle de confiance à 95% pour la moyenne
alpha = 0.05
z = qnorm(1 - alpha/2)
IC_moyenne = c(Xbar_strat - z * sqrt(var_Xbar),
               Xbar_strat + z * sqrt(var_Xbar))

# 6. Estimation du total et de l'intervalle de confiance pour T
Tstrat = N * Xbar_strat
IC_total = N * IC_moyenne
marge_erreur = (IC_total[2] - IC_total[1]) / 2

# Affichage des résultats
cat("Estimation de la moyenne µ :", Xbar_strat, "\n")
cat("Intervalle de confiance pour µ :", IC_moyenne, "\n")
cat("Estimation du total T :", Tstrat, "\n")
cat("Intervalle de confiance pour T :", IC_total, "\n")
cat("Marge d’erreur :", marge_erreur, "\n")
# -------------------------------------------------------

# Partie 2 : Analyse d’enquête - Variable sport × sexe

# 1. Charger les données (après export depuis Excel en .csv)
enquete = read.csv2("EnqueteSportEtudiant2024.csv", sep=";", header=TRUE)

# 2. Vérifier les premières lignes
head(enquete[c("sport", "sexe")])

# 3. Nettoyer les données : garder les lignes sans valeurs manquantes
donnees = na.omit(enquete[, c("sport", "sexe")])

# 4. Construire le tableau croisé
tab = table(donnees$sport, donnees$sexe)
print(tab)

# 5. Test d’indépendance du khi-deux
test = chisq.test(tab)
print(test)

# 6. Contributions au chi2
print(round(test$residuals^2, 2))

# 7. Calcul du V de Cramer
# V = sqrt(χ² / (n * (min(k, r) - 1)))
n = sum(tab)
k = ncol(tab)
r = nrow(tab)
V = sqrt(test$statistic / (n * (min(k, r) - 1)))
cat("V de Cramer :", round(V, 3), "\n")
