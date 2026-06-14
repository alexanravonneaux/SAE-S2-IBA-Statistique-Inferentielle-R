library (sampling)
data("belgianmunicipalities")
str(belgianmunicipalities)
head(belgianmunicipalities)

donnees = data.frame(belgianmunicipalities)

#commune
U = donnees$Commune

#nombre de commune
N = length(U)

#nombres d'habitants
T = sum(donnees$Tot04)

#----------------------------------------------------------------------
# tirage aléatoire simple pour n=50
n = 50
E = sample (U,n)
head(E)

donnees1 = donnees[donnees$commune %in% E,]

donnees2 = subset(donnees1, select = c(Commune, Tot04))

xbar = mean(donnees2$Tot04)