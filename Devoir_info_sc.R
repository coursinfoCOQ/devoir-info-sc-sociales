# Cours: Informatique Pour Sciences Sociales
# Date: Le 12-04-2022
# Membres du groupe:
# Jean Woodly COQ-CO184448
# Cassandra MOISE-Mo173612
# Junior DAVID-DA182339

# Objectif: Réaliser deux graphiques
#   Graphique 1: Ratio des réserves liquides des banques sur leurs actifs
#   Graphique 2: Evolution des transferts annuels (en % du PIB)


#--------------------------------------------------------------------------#


# Importer les données via un fichier Excel.
# Nous allons utiliser le package tidyverse pour le long de l'exercise

install.packages("tidyverse")
library(readxl)
ht_data <- read_excel("WB_HT_DATA.xlsx")

# Inspecter les données

head(ht_data)

# Installer les packages pour le nettoyage des données

library(tidyr)
library(dplyr)

install.packages("janitor")
library("janitor")

# Nettoyer les colonnes

ht_data_clean <- clean_names(ht_data)
View(ht_data_clean)

# Selectionner les données pour transfert et liquidité seulement

ht_data_filter <- ht_data_clean %>%
  select(indicator_name, x2000:x2020) %>% 
  filter(indicator_name == "Personal remittances, received (% of GDP)" | 
           indicator_name == "Bank liquid reserves to bank assets ratio (%)")

# Transformer les données du format large en format long

ht_liquidite <- ht_data_filter %>%
  filter(indicator_name == "Bank liquid reserves to bank assets ratio (%)") %>% 
  pivot_longer(-indicator_name, names_to = "annee", values_to = "liquidite") %>% 
  select(annee, liquidite)

ht_transfert <- ht_data_filter %>%
  filter(indicator_name == "Personal remittances, received (% of GDP)") %>% 
  pivot_longer(-indicator_name, names_to = "annee", values_to = "transfert") %>% 
  select(annee, transfert)

# Mettre les données dans un dataframe (utilisant les années 2000 à 2020)

df <- data.frame(annee = c(2000:2020), liquidite = ht_liquidite$liquidite, 
                 transfert = ht_transfert$transfert)
head(df)

# Installer le package ggplot2 pour les graphiques
# ggplot2 fait partir du package tidyverse que l'on a deja installé

library(ggplot2)

# 1) Réaliser la Figure 2.4: "Ratio des réserves liquides des banques sur leurs actifs (%)

## Plotter le graphique

ggplot(data=df, aes(x=annee, y=liquidite)) +
  geom_line(aes(linetype = "Liquidité"), col="green4", size=1.2)+
  geom_smooth(aes(color = "Tendance"), se=FALSE, size=1.2)+
  ggtitle("Ratio des réserves liquides des banques sur leurs actifs (%)") +
  xlab("Années") + ylab("% PIB") +
  
  ## Netoyer et Personaliser le graphique, utilisant theme() et guides()
  
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         plot.title = element_text(hjust = 0.5),
         axis.ticks.x = element_blank(), axis.ticks.y = element_blank()
  )+
  guides(linetype = guide_legend(title = " "))+
  guides(color = guide_legend(title = " "))+
  theme(legend.position = "top")+
  
  ## Ajouter les interceptes, utilisant geom_vline()
  
  geom_vline(xintercept = 2008, linetype="dashed", color="red")+
  geom_vline(xintercept = 2010, linetype="dashed", color="red")+
  geom_vline(xintercept = 2015, linetype="dashed", color="red")+
  geom_vline(xintercept = 2016, linetype="dashed", color="red")+
  geom_vline(xintercept = 2020, linetype="dashed", color="red")+
  
  ## Ajouter les textes, utilisant annotate()
  
  annotate("text", label = "Crise financière internationale",
           x = 2007.60, y = 75, angle = 90)+
  annotate("text", label = "Tremblement de terre",
           x = 2009.60, y = 70, angle = 90)+
  annotate("text", label = "BIC opérationnel",
           x = 2014.60, y = 76, angle = 90)+
  annotate("text", label = "Cyclonne Matthieu",
           x = 2015.60, y = 80, angle = 90)+
  annotate("text", label = "COVID-19",
           x = 2019.60, y = 88, angle = 90)


# 2) Réaliser la Figure 3.1: "Evolution des transferts annuels (en % du PIB)"

ggplot(data=df, aes(x=annee, y=transfert)) +
  geom_line(col="green4", size=1.2)+
  ggtitle("Evolution des transferts annuels (en % du PIB)") +
  xlab("Années") + ylab("% du PIB") +
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         plot.title = element_text(hjust = 0.5),
         axis.ticks.x = element_blank(), axis.ticks.y = element_blank())+
  
  ## Ajouter les interceptes
  
  geom_vline(xintercept = 2008, linetype="dashed", color="red")+
  geom_vline(xintercept = 2010, linetype="dashed", color="red")+
  geom_vline(xintercept = 2016, linetype="dashed", color="red")+
  geom_vline(xintercept = 2019, linetype="dashed", color="red")+
  geom_vline(xintercept = 2020, linetype="dashed", color="red")+
  
  ## Ajouter les textes
  
  annotate("text", label = "Crise financière internationale",
           x = 2007.60, y = 18, angle = 90)+
  annotate("text", label = "Tremblement de terre",
           x = 2009.60, y = 18, angle = 90)+
  annotate("text", label = "Cyclonne Matthieu",
           x = 2015.60, y = 18.5, angle = 90)+
  annotate("text", label = "2019",
           x = 2018.9, y = 12, angle = 90)+
  annotate("text", label = "COVID-19",
           x = 2019.50, y = 18, angle = 90)+
  annotate("text", label = "2020",
           x = 2020.1, y = 13, angle = 90)+
  
  ## Ajouter un rectangle pour hachurer l'espace entre 2019 et 2020,
  # utilisant une combinaison entre annotate() et geom_rect()
  
  annotate(geom = "rect", xmin = 2019, xmax = 2020, ymin = -Inf, ymax = Inf,
           fill = "cadetblue2", colour = "transparent", alpha = 0.3)

#-------------------------- Fin -------------------------------------#
