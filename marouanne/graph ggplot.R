# 📦 Charger ggplot2
library(ggplot2)

# 📋 Créer le tableau des résultats avec Pop Totale et Pop Estimée
df <- data.frame(
  Tirage = 1:11,
  Pop_Totale = rep(236283, 11),  # Pop Totale constante pour chaque tirage
  Pop_Estimée = rep(2139261, 11),  # Pop Estimée constante pour chaque tirage
  IDC_Moyenne = c(1445.223, 1322, 1257, 1217, 1308, 1275, 1340, 1298, 1235, 1205, 1311),
  Borne_Inf = c(997, 951, 902, 876, 984, 958, 1010, 980, 887, 860, 990)
)

# ➕ Calcul de la borne supérieure et marge d'erreur
df$Borne_Sup <- 2 * df$IDC_Moyenne - df$Borne_Inf  # borne sup = moyenne + (moyenne - inf)
df$Marge_Erreur <- df$IDC_Moyenne - df$Borne_Inf

# 📈 Graphique combiné avec Pop Totale, Pop Estimée et barres d'erreur
ggplot(df, aes(x = Tirage)) +
  # Courbe pour IDC Moyenne
  geom_line(aes(y = IDC_Moyenne), color = "blue", size = 1) +
  geom_point(aes(y = IDC_Moyenne), size = 3, color = "darkblue") +
  # Courbe pour Pop Totale
  geom_line(aes(y = Pop_Totale / 1000), color = "green", linetype = "dashed", size = 1) + # Divisé par 1000 pour mieux visualiser
  # Courbe pour Pop Estimée
  geom_line(aes(y = Pop_Estimée / 1000), color = "orange", linetype = "dotted", size = 1) + # Divisé par 1000 pour mieux visualiser
  # Barres d'erreur pour IDC
  geom_errorbar(aes(ymin = Borne_Inf, ymax = Borne_Sup), width = 0.2, color = "red") +
  labs(
    title = "Évolution des IDC Moyennes, Pop Totale et Pop Estimée avec Marge d'Erreur",
    x = "Numéro du Tirage",
    y = "Valeurs (en milliers)"
  ) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 1000, name = "Pop Totale / Estimée")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  ) +
  theme(legend.position = "bottom") +
  scale_color_manual(name = "Légende", values = c("blue", "green", "orange", "red")) +
  labs(color = "Légende")
