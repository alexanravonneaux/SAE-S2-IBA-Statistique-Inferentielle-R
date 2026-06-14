library(sampling)

# ---- Importation des données ----
data <- read.csv2("population_francaise_communes.csv", fileEncoding = "latin1")

# ---- Filtrage pour la région Centre-Val de Loire ----
donnees <- subset(data, `Nom.de.la.région`  == "Centre-Val de Loire")
donnees <- subset(donnees, select = c(Commune, Population.totale, Code.département))

# Nettoyage de la variable de population
donnees$Population.totale <- as.numeric(gsub(" ", "", donnees$Population.totale))

# ---- Données de base ----
U <- donnees$Commune
N <- length(U)
cat("Nombre total de communes :", N, "\n")

T <- sum(donnees$Population.totale, na.rm = TRUE)
cat("Population totale exacte :", T, "\n")

# ---- Sondage aléatoire simple (SRS) ----
n <- 100
E <- sample(U, n)
donnees_unique <- donnees[!duplicated(donnees$Commune), ]
donnees1 <- donnees[donnees$Commune %in% E, ]

xbar <- mean(donnees1$Population.totale)
idcmoy <- t.test(donnees1$Population.totale)$conf.int
T_est <- N * xbar
idcT <- idcmoy * N
marge <- (idcT[2] - idcT[1]) / 2

cat("\n--- SRS ---\n")
cat("Estimation T :", T_est, "\n")
cat("IDC T :", idcT, "\n")
cat("Marge d'erreur :", marge, "\n")

# ---- Sondage stratifié (STRAT) ----
donnees$strate <- cut(donnees$Population.totale,
breaks = c(0, 230, 500, 1350, 45000),labels = c(1, 2, 3, 4),include.lowest = TRUE)

donneesstrat <- donnees[, c("Commune", "Population.totale", "strate")]
data <- donneesstrat[order(donneesstrat$strate), ]

Nh <- table(data$strate)
N <- sum(Nh)
gh <- Nh / N
n <- 100
nh <- round(n * Nh / N)
if (sum(nh) != n) nh[which.max(nh)] <- nh[which.max(nh)] + (n - sum(nh))
fh <- nh / Nh
fh
# Tirage stratifié
st <- strata(data, stratanames = c("strate"), size = nh, method = "srswr")
data1 <- getdata(data, st)

# Sous-échantillons
ech1 <- data1[data1$strate == 1, ]
ech2 <- data1[data1$strate == 2, ]
ech3 <- data1[data1$strate == 3, ]
ech4 <- data1[data1$strate == 4, ]

# Moyennes et variances
m1 <- mean(ech1$Population.totale)
m2 <- mean(ech2$Population.totale)
m3 <- mean(ech3$Population.totale)
m4 <- mean(ech4$Population.totale)

var1 <- var(ech1$Population.totale)
var2 <- var(ech2$Population.totale)
var3 <- var(ech3$Population.totale)
var4 <- var(ech4$Population.totale)

# Estimation moyenne
Xbarst <- (Nh[1]*m1 + Nh[2]*m2 + Nh[3]*m3 + Nh[4]*m4) / N

# Variance
varXbarst <- ((gh[1])^2)*(1 - fh[1])*var1/nh[1] +
  ((gh[2])^2)*(1 - fh[2])*var2/nh[2] +
  ((gh[3])^2)*(1 - fh[3])*var3/nh[3] +
  ((gh[4])^2)*(1 - fh[4])*var4/nh[4]

# IDC
alpha <- 0.05
binf <- Xbarst - qnorm(1 - alpha / 2) * sqrt(varXbarst)
bsup <- Xbarst + qnorm(1 - alpha / 2) * sqrt(varXbarst)
idcmoy <- c(binf, bsup)

# Estimation totale
Tstr <- N * Xbarst
idcT <- idcmoy * N
marge <- (idcT[2] - idcT[1]) / 2

cat("\n--- STRAT ---\n")
cat("Estimation T :", Tstr, "\n")
cat("IDC T :", idcT, "\n")
cat("Marge d'erreur :", marge, "\n")
cat("Différence avec vraie valeur T :", abs(Tstr - T), "\n")
