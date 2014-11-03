library(shiny)
library(shinyBS)
library(ShinyDash)
library(rCharts)
library(leaflet)
library(ggplot2)
library(data.table)
library(bit64)
library(plyr)
library(dplyr)
library(reshape2)
library(parallel)
library(doParallel)
library(foreach)

# Attributs de la data table index
varIndex <- c("siret", "rs", "naf", "ville", "lat", "lon", "ca")

# Attributs de la data table pour la carte
varMap <- c('siret', 'rs', 'date', 'naf', 'ville', 'lat', 'lon', 'ca', 'montant', 'transaction')

# Chargement de la data table répertoriant l'ensemble les informations sur tous les commerçants
index <- fread("C:/Users/CAvatar/Desktop/INDEX/index.txt", sep="\t")
setnames(x = index, old=names(index), new=varIndex)

# Structuration des données
index$siret <- as.character(index$siret)


# Variable de stockage des données pour le graphique panier
df.panier <- data.frame()
crit.panier <- data.frame()

# Initialisation de la data.table df.p pour permettre le lancement
# Cette table est par la suite override
df.p <- data.table()

# Attributs des data tables df.s et df.n
var.n <- c('montant','date','heure','siret','rs','naf','villecom','lon','lat','affilie','client','age','sexe','csp','optel','opmail','mail',
           'enfant','situation','anciennete','segment','score','avoir','appinternet','appmobile','villeclient','uu','carte','libelle','reseau',
           'paiement','retrait', 'niveau','typecompte','credit','debit','mensualite','restant','datepremire','datederniere','vide','ca',
           'delta','poids.cum','Glon','Glat')

var <- c('montant','date','heure','siret','rs','naf','villecom','lon','lat','affilie','client','age','sexe','csp','optel','opmail','mail',
         'enfant','situation','anciennete','segment','score','avoir','appinternet','appmobile','villeclient','uu','carte','libelle','reseau',
         'paiement','retrait', 'niveau','typecompte','credit','debit','mensualite','restant','datepremire','datederniere','vide','ca')

# Variables utiles à l'initialisation de la carte dans la méthode comInBounds()
north <- 0.00342
east <- 0.0204407
south <- 0.00271
west <- 0.0203933

# Récupération du nom de la table SIRET du commerçant connecté
getNameSIRET <- function() {
  df.name <- paste('C:/Users/CAvatar/Desktop/SIRET/','NAF',KEY$naf,'/',KEY$siret,'.txt',sep = "")
  return(df.name)
}

# Récupération du nom de la table NAF du commerçant connecté
getNameNAF <- function() {
  df.name <- paste('C:/Users/CAvatar/Desktop/NAF/NAF/',KEY$naf,'.txt',sep = "")
  return(df.name)
}

# Rayon de la Terre (km), nécessaire pour le calcul de la prospection
rayon = 6378.137

## Configuration du cluster
# spec = c("node001","node002","node003","node004","node005","node006","node007","node008","node009","node010")
# cluster.ca = makeCluster(spec = spec, master = spec[9],
#                          type = "PSOCK", port = 10187)

## On enregistre le cluster dans le 'parallel backend' afin qu'il puisse être utilisé à traver foreach(...) %dopar% ...
# registerDoParallel(cl = cluster.ca, cores=detectCores())