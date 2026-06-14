#import du fichier csv
data <- read.csv2("population_francaise_communes.csv", fileEncoding = "latin1")
#Import des librairies
library(sampling)


# Filtrage des données pour la région "Centre-Val de Loire"
new_data <- subset(data, `Nom.de.la.région` == "Centre-Val de Loire")
head(new_data)


#Variable U avec le nombre communes de Centre-Val de Loire
Pop=new_data$Commune
head(Pop)

#nombre de commune de Centre val de loire
nombre_communes <- length(Pop)
print(nombre_communes)
#Donc il y'a 1757 communes dans cette region

# Assurez-vous que la colonne Population.totale est numérique
new_data$Population.totale <- as.numeric(gsub(",", "", new_data$Population.totale))

# Calcul de la somme de la population
T = sum(new_data$Population.totale, na.rm = TRUE)
print(T)
#la population totale est de 2,6millions d'habitant
#Tirage aléatoir simple d'un echantillon de taille n=50
n=100
E=sample(Pop,n)
head(E)

#toutes les données avec les 50communes
# Supprimer les doublons avant de tirer l’échantillon
new_data_unique <- new_data[!duplicated(new_data$Commune), ]

# Tirage aléatoire de 100 communes uniques
E <- sample(new_data_unique$Commune, 100)

# Sélection des lignes correspondantes
data1 <- new_data_unique[new_data_unique$Commune %in% E, ]


#L'echantillon de commune avec le nbre d'habitants dans une table ,et le département
donne2=subset(data1,select =c(Commune,Population.totale,Code.département))




# Calcul de la moyenne de la population dans l'échantillon (en ignorant les NA)
xbar <- mean(donne2$Population.totale, na.rm = TRUE)
xbar

#IDC  de \mu
idcmoy=t.test(donne2$Population.totale)$conf.int
idcmoy

#Nombre d'habitants
T_est=nombre_communes*xbar
T_est


#IDC de t
idcT=idcmoy*nombre_communes
idcT
#marge d'erreur
marge=(idcT[2]-idcT[1])/2
marge


#Refaire un graphique 



#Partie 2


# Résumé statistique
summary(donne2$Population.totale)

# Supposons que les quartiles sont : min = 82, Q1 = 228.8, médiane = 487, Q3 = 1348, max = 13858
# On crée 4 strates selon ces bornes :
donne2$strate <- cut(
  donne2$Population.totale,
  breaks = c(0, 230, 487, 1348, 44890),  # bornes triées et cohérentes
  labels = c(1, 2, 3, 4),
  include.lowest = TRUE
)


# Vérification du nombre d'observations par strate
table(donne2$strate)

# Nouveau data.frame avec colonnes utiles
donne2strat <- donne2[, c("Commune", "Population.totale", "strate")]

# Affichage des premières lignes
head(donne2strat)




# Affichage des premières lignes
head(donne2strat)

# Effectif des strates 
data1 = donne2strat[order(donne2strat$strate),]
Nh = table(donne2strat$strate)
Nh
N=sum(Nh)


#Tirage d'un echantillon stratife taille= n=100
n1=100
nh1=round(c(n1*Nh[1]/N,n1*Nh[2]/N,n1*Nh[3]/N,n1*Nh[4]/N))
nh1
nh1=c(7,19,18,12)

#Taux de sondage ds les strates
fh=nh1/Nh
fh

