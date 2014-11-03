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

Logged = FALSE;

shinyServer(function(input, output, session) {
  
  session$sendCustomMessage(type='jsCode', list(value = "connexion = false"))
  session$sendCustomMessage(type='jsCode', list(value = "var_carte = false"))
  session$sendCustomMessage(type='jsCode', list(value = "var_load_data = false"))
  session$sendCustomMessage(type='jsCode', list(value = "var_ca = false"))
  session$sendCustomMessage(type='jsCode', list(value = "var_profil = false"))
  session$sendCustomMessage(type='jsCode', list(value = "var_prospection = false"))
  
  # Appel de l'interface de connexion
  source('www/login.r',  local = TRUE)
  
  # Observateur lisant l'intégralité du code lors de l'appui sur le bouton de connexion
  observe({
    # Encapsuleur donnant accès à l'application sous condition que la connexion soit validée
    if (USER$Logged == TRUE) {
      
      # Construction des élements du fond de commerce
      source('www/map.r', local = TRUE)
      
      # Chargement de la data table des transactions du commerçant connecté
      df.s <<- fread(getNameSIRET(), sep = "\t")
      setnames(x = df.s, old=names(df.s), new = var)
      
      # Chargement de la data table des transactions NAF selon le commerçant connecté  
      df.n <<- fread(getNameNAF(), sep = "\t")
      setnames(x = df.n, old=names(df.n), new = var.n)
      
      # Chargement de la data table du résumé des transactions des commerçants d'un même NAF
      df.r <<- fread(paste("C:/Users/CAvatar/Desktop/MAP/", KEY$naf, ".txt", sep = ""), sep = "\t")
      setnames(x = df.r, old=names(df.r), new = varMap)
      
      # Structuration de la data table
      df.r$siret <<- as.character(df.r$siret)
      df.r$date <<- as.IDate(df.r$date, format = "%d/%m/%Y")
      
      # Chargement de la data table des transactions du commerçant connecté pour le calcul du panier
      df.a <<- subset(df.s, select = c('montant','date','client','age','sexe','csp','situation','villeclient'))
      
      # Structuration de la data table
      df.a$date <<- as.IDate(df.a$date, format = "%d/%m/%Y")
      
      # Coordonnées géospatiales du commerçant transformées en radians (calcul de la prospection)
      latC <<- KEY$lat * pi/180
      lonC <<- KEY$lon * pi/180
      
      # Créaction d'un 'proxy' de la carte à l'aide duquel les cercles seront construits
      map <- createLeafletMap(session, 'map')
      
      # MAJ de var_load-data et effacement de la fenêtre de connexion
      session$sendCustomMessage(type='jsCode', list(value = "$('div.login').remove();"))
      session$sendCustomMessage(type='jsCode', list(value = "var_load_data = true"))
      
      # Récapitulatif des informations du commerçant
      output$mon_commerce <- renderUI({
        as.character(tagList(
          tags$h3(KEY$rs),
          tags$strong("Siret : "), sprintf("%s", KEY$siret), tags$br(),
          tags$strong("NAF : "), sprintf("%s", KEY$naf), tags$br(),
          tags$strong("Latitude : "), sprintf("%s", KEY$lat), tags$br(),
          tags$strong("Longitude : "), sprintf("%s", KEY$lon), tags$br()
        ))
      })
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # START Fonctions pour le traitement de la carte et du graph ca
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      # Récupération début/fin dates de visualisation pour la carte
      get_date_carte <- reactive({
        if(is.null(input$date_visualisation_carte[1]) & is.null(input$date_visualisation_carte[2])){
          out <- c(as.IDate("2013-05-01"), as.IDate("2013-05-08"))
        } else {
          out <- c(input$date_visualisation_carte[1], input$date_visualisation_carte[2])
        }
      })
      
      # Récupération début/fin dates de visualisation pour le graphique
      get_date_ca <- reactive({
        if(is.null(input$date_visualisation_ca[1]) & is.null(input$date_visualisation_ca[2])){
          out <- c(as.IDate("2013-05-01"), as.IDate("2013-05-08"))
        } else {
          out <- c(input$date_visualisation_ca[1], input$date_visualisation_ca[2])
        }
      })
      
      # Récupération début/fin dates de visualisation pour les pie charts du profiling
      get_date_profil <- reactive({
        print(input$date_visualisation_profil)
        if(is.null(input$date_visualisation_profil)){
          out <- c(as.IDate("2013-05-01"), as.IDate("2013-05-08"))
        } else {
          out <- c(input$date_visualisation_profil[1], input$date_visualisation_profil[2])
        }
      })
      
      # Récupération data 'carte' lors de la modification de la date de viz
      get_donnee_carte <- function() {
        periode_date_carte <- get_date_carte()
        
        df <- df.r[df.r$date >= periode_date_carte[1]
                   & df.r$date <= periode_date_carte[2], ]
        df <- df[order(df$siret), ]
        df <-group_by(df, siret, rs, naf, ville, lat, lon, ca)
        df <- summarise(.data = df, montant = sum(montant), transaction = sum(transaction))
        df
      }
      
      # Type de comparaison pour la 'carte'
      compare_by_carte <- reactive({
        if (is.null(input$type_comparaison_carte))
          return("montant")
        if (input$type_comparaison_carte == "Montants") {
          return("montant")
        } else {return("transaction")}
      })
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # END Fonctions pour le traitement de la carte et du graph ca
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # START Gestion de l'affichage de la carte
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      makeReactiveBinding('selectedCom')
      
      # Méthode récupérant les commerçants sur le champ de vision de la carte
      com_in_bounds <- reactive({
        
        # Au lancement
        if(is.null(input$map_bounds)) {
          latRng <- c(KEY$lat + north, KEY$lat - south)
          lngRng <- c(KEY$lon - west, KEY$lon + east)
          
          in_bounds <- subset(get_donnee_carte(),
                              lat >= latRng[1] & lat <= latRng[2]
                              & lon >= lngRng[1] & lon <= lngRng[2])
          return(in_bounds[order(in_bounds[[compare_by_carte()]], decreasing = TRUE), ])
        } else {
          # Une fois l'application qui tourne
          bounds <- input$map_bounds
          latRng <- range(bounds$north, bounds$south)
          lngRng <- range(bounds$east, bounds$west)
          in_bounds <- subset(get_donnee_carte(),
                              lat >= latRng[1] & lat <= latRng[2]
                              & lon >= lngRng[1] & lon <= lngRng[2])
          return(in_bounds[order(in_bounds[[compare_by_carte()]], decreasing = TRUE), ])
        }
      })
      
      # Observation des clicks sur la carte. Si une pop-up était visible, le click sur un point quelconque 
      # de la carte (hors commerçants) ferme la pop-up actuelle en réinitialisant le commerçant sélectionné (selectedCom)   
      observe({
        if (is.null(input$map_click))
          return()
        selectedCom <<- NULL
      }) # END observateur 'map_click'
      
      # Observe la navigation de l'utilisateur sur la carte et met à jour l'affichage des commerçants
      observe({
        map$clearShapes()
        
        # Récupération des commerçants à afficher sur la carte
        liste_commercants <<- com_in_bounds()
        
        if (nrow(liste_commercants) == 0)
          return()
        
        # Ajout des cercles sur la carte
        map$addCircle(
          liste_commercants$lat,
          liste_commercants$lon,
          4 * sqrt(liste_commercants[[compare_by_carte()]]) / (0.2 * input$map_zoom),
          row.names(liste_commercants),
          list(
            weight = 1.2,
            fill = TRUE,
            color = get_color_circle())
        )
      }) # END observateur pour la construction des cercles sur le plan
      
      # Coloriage des cercles des commerçants selon leur affiliation à CASA
      get_color_circle <- function() {
        
        ids <- c(row.names(liste_commercants))
        category <- vector("list", nrow(liste_commercants))
        
        for(i in 1:nrow(liste_commercants)) {
          # Commerçants non affiliés
          if (liste_commercants[row.names(liste_commercants) == ids[i], ]$ca == 'N') {
            category[i] <- '#F03'
          } else {
            # Commerçant connecté
            if(liste_commercants[row.names(liste_commercants)== ids[i],]$siret == KEY$siret) {
              category[i] <- '#00F'
              # Commerçant non connecté mais affilié
            } else {category[i] <- '#4A9'}
          }
        }
        category
      }
      
      observe({ # Observateur pour l'affichage du pop-up d'un commerçant lors d'un click
        
        # Récupération des infos sur l'object clické
        event <- input$map_shape_click
        if (is.null(event))
          return()
        
        # Si click sur un cercle, alors on efface les pop-up précédents et affiche celui correspondant
        map$clearPopups()
        
        isolate({
          com <- liste_commercants[row.names(liste_commercants) == event$id,]
          selectedCom <<- com
          content <- as.character(tagList(
            tags$strong(com$rs), tags$br(),
            tags$strong("Siret : "), sprintf("%s", com$siret), tags$br(),
            tags$strong("Affiliation au Crédit Agricole : "), sprintf("%s", com$ca), tags$br(),
            tags$strong("NAF : "), sprintf("%s", com$naf), tags$br(),
            tags$strong("Montant des transactions : "), sprintf("%s €", com$montant), tags$br(),
            tags$strong("Nombre de transactions : "), sprintf("%s", com$transaction), tags$br()
          ))
          map$showPopup(event$lat, event$lng, content, event$id)
        })
      }) # END observateur pour la construction des pop-up
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # END Gestion de l'affichage de la carte
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # START Gestion 'Afficher carte' - 'Retour au dashbord'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      observe({ # Observateur pour afficher la carte
        if(input$btn_map>0) {
          input$btn_map
          #           session$sendCustomMessage(type='jsCode', list(value = "$('li#id_mon_commerce').hide()"))
          #           session$sendCustomMessage(type='jsCode', list(value = "$('li#id_visualiser_carte').hide()"))
          #           session$sendCustomMessage(type='jsCode', list(value = "$('li#id_visualiser_ca').hide()"))
          #           session$sendCustomMessage(type='jsCode', list(value = "$('li#id_mon_commerce').hide()"))
          session$sendCustomMessage(type='jsCode', list(value = "$(document.getElementById('gridster_dashbord')).css('display','none');"))
          session$sendCustomMessage(type='jsCode', list(value = "var_carte = true"))
        }
      }) # END observateur pour l'affichage de la carte
      
      observe({ # Observateur pour revenir au dashbord
        if(!is.null(input$btn_map_retour)) {
          if(input$btn_map_retour>0) {
            input$btn_map_retour
            session$sendCustomMessage(type='jsCode', list(value = "var_carte = false"))
            session$sendCustomMessage(type='jsCode', list(value = "$(document.getElementById('gridster_dashbord')).css('display','block');"))
            #             session$sendCustomMessage(type='jsCode', list(value = "$('li#id_mon_commerce').show()"))
            #             session$sendCustomMessage(type='jsCode', list(value = "$('li#id_visualiser_carte').show()"))
            #             session$sendCustomMessage(type='jsCode', list(value = "$('li#id_visualiser_ca').show()"))
          }
        }
      }) # END observateur pour revenir au dashbord
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # END Gestion 'Afficher carte' - 'Retour au dashbord'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # START Graphique 'chiffre d'affaires'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Récupération données pour les graphiques
      get_donnee_ca <- reactive({
        
        # Structuration des données du commerçant KEY
        tr <- df.s
        tr$date <- with(tr, as.IDate(x = date, format = "%d/%m/%Y")) 
        tr$heure <- with(tr, as.ITime(x = heure, format = "%H:%M:%S"))
        
        # Récupération de la période de visualisation        
        periode_date_ca <- get_date_ca()        
        tr <- tr[date %between% c(periode_date_ca[1],periode_date_ca[2])]
        
        # Tables selon la période de la journée. La séquence générée est par défaut par secondes
        matin <- tr[heure %in% seq(as.ITime("08:00:00", format = "%H:%M:%S"), as.ITime("12:00:00", format = "%H:%M:%S")), ]
        midi <- tr[heure %in% seq(as.ITime("12:00:01", format = "%H:%M:%S"), as.ITime("14:00:00", format = "%H:%M:%S")), ]
        pm <- tr[heure %in% seq(as.ITime("14:00:01", format = "%H:%M:%S"), as.ITime("17:00:00", format = "%H:%M:%S")), ]
        soir <- rbindlist(list(tr[heure %in% seq(as.ITime("17:00:01", format = "%H:%M:%S"), as.ITime("23:59:59", format = "%H:%M:%S")), ],
                               tr[heure %in% seq(as.ITime("00:00:00", format = "%H:%M:%S"), as.ITime("07:59:59", format = "%H:%M:%S")), ]))
        
        # La classe IDate contient un attribut qui ordonne automatiquement les données
        matin.s <- matin %>%
          group_by(date) %>%
          summarise(montant = sum(montant),
                    transaction = n())
        # Ajout de la dernière colonne pour le tracé
        matin.s[,period:="matin"]
        
        midi.s <- midi %>%
          group_by(date) %>%
          summarise(montant = sum(montant),
                    transaction = n())
        midi.s[,period:="midi"]
        
        pm.s <- pm %>%
          group_by(date) %>%
          summarise(montant = sum(montant),
                    transaction = n())
        pm.s[,period:="pm"]
        
        soir.s <- soir %>%
          group_by(date) %>%
          summarise(montant = sum(montant),
                    transaction = n())
        soir.s[,period:="soir"]
        
        # Fusion de l'ensemble des tables
        df <- rbindlist(list(matin.s, midi.s, pm.s, soir.s))
        
        # Modification de la structure pour compatibilité avec la construction du graphique
        df$period <- factor(df$period, c("soir", "pm", "midi", "matin"))
        df$date <- as.factor(df$date)
        
        # Traitement du use case : 'aucune transaction dans la période de visualisation' (exemple : visualisation sur une journée)
        if(nrow(df)>0) {
          # MAJ du message d'erreur
          output$graph_ca_msg <- renderUI({})
          return(df)
          # Si df est vide : affichage d'un message à l'utilisateur
        } else {
          output$graph_ca_msg <- renderUI({tags$h5(paste("Aucune transaction entre le", input$periode_date_ca[1], "et le", input$periode_date_ca[2]))})
        }
      })
      
      # Graphique ca ~ montant
      output$graph_ca_montant <- renderChart({
        donnee_montant <- get_donnee_ca()
        nGraph <- nPlot(
          montant ~ date,
          group = "period",
          data = donnee_montant,
          type = "multiBarChart",
          width = 800,
          height = 400
        )
        nGraph$set(dom = "graph_ca_montant")
        # Paramètres du graphique facultatifs
        # nGraph$xAxis( tickFormat="#!function(d) {return d3.time.format('%X')(new Date(d));}!#" )
        # nGraph$setLib(lib="libraries/widgets/nvd3")
        # nGraph$params$facet="date"
        # nGraph$templates$script = system.file("/libraries/nvd3/layouts/nvd3FacetPlot.html", package = "rCharts")
        return(nGraph)
      })
      
      # Graphique ca ~ transaction
      output$graph_ca_transaction <- renderChart({
        donnee_transaction <- get_donnee_ca()
        nGraph <- nPlot(
          transaction ~ date,
          group = "period",
          data = donnee_transaction,
          type = "multiBarChart"
          #           width = 800,
          #           height = 400
        )
        nGraph$set(dom = "graph_ca_transaction")
        return(nGraph)
      })
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # END Graphique 'chiffre d'affaires'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # START Gestion 'Afficher graph ca' - 'Retour au dashbord'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Panneau pour la visualisation selon une date des graphiques
      output$panneau_comparaison_ca <- renderUI({
        absolutePanel(id = "composants_compare_ca", class = "modal modal_ca", fixed = FALSE, draggable = TRUE,
                      top = 57, left = 330, right = "auto", bottom = "auto",
                      width = "auto", height = "auto",
                      dateRangeInput("date_visualisation_ca", label = "P\u00E9riode de visualisation des donn\u00E9es", start = "2013-05-01", end = "2013-05-08", min = "2012-09-28", max = "2013-10-02",
                                     format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "fr", separator = " \u00E0 "),
                      bsActionButton(inputId = "btn_ca_retour", label = "Retour", style = "success")
        )
      })
      
      observe({ # Observateur pour afficher les graphiques ca
        if(input$btn_ca>0) {
          input$btn_ca
          session$sendCustomMessage(type='jsCode', list(value = "$(document.getElementById('gridster_dashbord')).css('display','none');"))
          session$sendCustomMessage(type='jsCode', list(value = "var_ca = true"))
        }
      }) # END observateur pour afficher les graphiques ca
      
      observe({ # Observateur pour revenir au dashbord
        if(!is.null(input$btn_ca_retour)) {
          if(input$btn_ca_retour>0) {
            input$btn_ca_retour
            session$sendCustomMessage(type='jsCode', list(value = "var_ca = false"))
            session$sendCustomMessage(type='jsCode', list(value = "$(document.getElementById('gridster_dashbord')).css('display','block');"))
          }
        }
      }) # END observateur pour revenir au dashbord
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # END Gestion 'Afficher graph ca' - 'Retour au dashbord'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # START Module 'profil des clients'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
      # Construction de la table de fidélité (répartition prospects, clients fidèles et infidèles)
      get_donnee_profil <- reactive({
        
        # Récupération de la période de visualisation et trigger de la fonction
        periode_date_profil <- get_date_profil()
        
        # Si la data frame data_profil existe déjà on l'efface pour ne pas avoir de concurrence d'accès dans la suite des calculs
        if(exists("data_profil"))
          rm("data_profil", pos = ".GlobalEnv")
        
        # Recopiage de la table des transactions NAF en local
        tr <- df.n
        # Structuration de l'ensemble des transactions
        tr$siret <- as.factor(tr$siret)
        
        # Subset selon la période de visualisation
        tr <- tr[date %between% c(periode_date_profil[1],periode_date_profil[2])]
        
        # Récupération des 'reliques' transactionnelles
        tr.c <- unique(tr,by=c("client","siret"))[j=list(client,siret)]
        
        # On regroupe la table par client puis on test si le siret du commerçant est présent dans au moins une des transactions du client
        c.group <- group_by(tr.c, client)
        
        # On garde les clients ayant effectué au moins une transaction chez le commerçant connecté
        clients <- filter(c.group, KEY$siret %in% siret)
        
        # Si j'ai au moins un client potentiellement fidèle ou infidèle alors je les tag selon leur type de fidélité
        if(nrow(clients)>0) {
          
          clients <- summarise(clients, n=n())
          clients$fid <- numeric(length = nrow(clients))
          
          # On récupère les informations sur la fidélité des clients avant de supprimer la colonne
          data_profil <<- c(table(clients$n==1))
          
          # On tag les clients selon qu'ils sont des clients fidèles ou infidèles avec la variable fid
          # fid = 1 : clients fidèles
          # fid = 2 : clients infidèles
          for(i in 1:nrow(clients)) {
            if(clients$n[i]==1) {
              clients$fid[i]=1
            }
            else {clients$fid[i]=2}
          }
          clients$n <- NULL
        } else {
          clients$siret <- NULL
          clients$fid <- numeric()
          data_profil <<- numeric(length = 3)
        }
        
        # index.fidelite est la data table permettant de connaître le statut de fidélité des clients ayant effectué des achats durant la
        # période de visualisation des données
        # Pour mémo :
        # fid = 1 : clients fidèles
        # fid = 2 : clients infidèles
        # fid = 0 : prospects
        index.fidelite <- rbind.data.frame(data.frame(client = unique(c.group[!(client %in% clients$client)]$client),
                                                      fid = numeric(length = length(unique(c.group[!(client %in% clients$client)]$client)))),
                                           clients)
        
        # Notre data table pour la prospection est prête à l'emploi
        # Cette table est construire par l'algorithme de prospection. Permet de bien scinder la prospection et le profiling
        #df.p <<- merge(tr, index.fidelite, by = "client")
        
        # On termine de construire la table de fidélité pour le camembert
        data_profil[3] <<- nrow(unique(tr,by=c("client")))-(sum(data_profil))
        data_profil <<- cbind(melt(data_profil), type = c("Infidèles","Fidèles","Prospects"))
        # Remplacement des NAs par des 0 (cf ci-dessous)
        data_profil$value[is.na(data_profil$value)] <<- 0
        return(data_profil)
      })
      
      # Graphique 'Répartition de la clientèle'
      output$graph_profil <- renderChart({
        # Code JavaScript envoyé au serveur afin de modifier le fichier HTML généré.
        # On passe ainsi de <html class> à <html class='shiny-busy'> et donc busy.js prend la relève
        # L'utilisateur est donc prévenu de l'exécution des calculs en cours
        session$sendCustomMessage(type='jsCode', list(value = "$('html').attr('class','shiny-busy');"))
        
        data_profil <- get_donnee_profil()
        nPie <- nPlot(
          value ~ type,
          data = data_profil,
          type = "pieChart",
          width = 400,
          height = 400
        )
        nPie$set(dom = "graph_profil")
        nPie$chart(donut = FALSE, showLegend = TRUE)
        nPie
      })
      
      # Renvoie la table data_profil lorsque la période de visualisation est modifiée (astuce pour rendre data_profil réactive)
      data_profil_load <- reactive({
        get_date_profil()
        data_profil
      })
      
      # Récupération des données clients selon la visualisation
      get_donnee_profil_pie <- reactive({
        periode_date_profril <- get_date_profil()
        df.s$date <- as.IDate(df.s$date, format = "%d/%m/%Y")
        df <- subset(df.s, date %between% c(periode_date_profril[1],periode_date_profril[2]))
        df <- subset(df, select = c("client","age","sexe","csp","situation"))
        df <- unique(df) %>%
          ddply(.(age,sexe,csp,situation), summarise, freq=length(age)) %>%
          arrange(desc(freq))
        df$rec <- df$freq / sum(df$freq) * 100
        df$rec.cum <- cumsum(df$rec)
        df
      })
      
      # Graphique 'Répartition des clients selon le genre'
      output$graph_profil_genre <- renderChart({
        df_sexe <- as.data.frame(get_donnee_profil_pie() %>% group_by(sexe) %>% summarise(n = n()))
        nPie <- nPlot(
          n ~ sexe,
          data = df_sexe,
          type = "pieChart",
          width = 400,
          height = 400
        )
        nPie$set(dom = "graph_profil_genre")
        nPie$chart(donut = TRUE, showLegend = TRUE)
        nPie
      })
      
      # Graphique 'Répartition des clients selon l'âge'
      output$graph_profil_age <- renderChart({
        df_age <- get_donnee_profil_pie()
        df_age <- data.frame(pallier = c("jeunes","adultes","senior","retraite"),
                             freq = c(as.numeric(table(df_age$age %between% c(0,25))[2]),
                                      as.numeric(table(df_age$age %between% c(26,38))[2]),
                                      as.numeric(table(df_age$age %between% c(39,49))[2]),
                                      as.numeric(table(df_age$age %between% c(50,130))[2])))
        ## Alternative pour remplacer les NA par des 0. Le hic c'est que toutes les légendes apparaissent alors sur le graphique (ie celles avec 0% inutiles...)
        # df_age$freq[is.na(df_age$freq)] <- 0
        
        nPie <- nPlot(
          freq ~ pallier,
          data = df_age,
          type = "pieChart",
          width = 400,
          height = 400
        )
        nPie$set(dom = "graph_profil_age")
        nPie$chart(donut = TRUE, showLegend = TRUE)
        nPie
      })
      
      # Graphique 'Répartition des clients selon la csp'
      output$graph_profil_csp <- renderChart({
        df_csp <- as.data.frame(get_donnee_profil_pie() %>% group_by(csp) %>% summarise(n = n()))
        nPie <- nPlot(
          n ~ csp,
          data = df_csp,
          type = "pieChart",
          width = 800,
          height = 400
        )
        nPie$set(dom = "graph_profil_csp", class = "graph-profil-csp-class")
        nPie$chart(donut = TRUE, showLegend = TRUE)
        nPie
      })
      
      # Graphique 'Répartition des clients selon la situation'
      output$graph_profil_situation <- renderChart({
        df_situation <- as.data.frame(get_donnee_profil_pie() %>% group_by(situation) %>% summarise(n = n()))
        nPie <- nPlot(
          n ~ situation,
          data = df_situation,
          type = "pieChart",
          width = 800,
          height = 400
        )
        nPie$set(dom = "graph_profil_situation")
        nPie$chart(donut = TRUE, showLegend = TRUE)
        nPie
      })
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # END Module 'profil des clients'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # START Gestion 'Afficher module profil' - 'Retour au dashbord'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      observe({ # Observateur pour afficher les graphiques profil
        if(input$btn_profil>0) {
          input$btn_profil
          session$sendCustomMessage(type='jsCode', list(value = "$(document.getElementById('gridster_dashbord')).css('display','none');"))
          session$sendCustomMessage(type='jsCode', list(value = "$(document.getElementById('gridster_profil')).css('display','block');"))
          session$sendCustomMessage(type='jsCode', list(value = "var_profil = true"))
        }
      }) # END observateur pour afficher les graphiques ca
      
      observe({ # Observateur pour revenir au dashbord
        if(!is.null(input$btn_profil_retour)) {
          if(input$btn_profil_retour>0) {
            input$btn_profil_retour
            session$sendCustomMessage(type='jsCode', list(value = "var_profil = false"))
            session$sendCustomMessage(type='jsCode', list(value = "$(document.getElementById('gridster_profil')).css('display','none');"))
            session$sendCustomMessage(type='jsCode', list(value = "$(document.getElementById('gridster_dashbord')).css('display','block');"))
          }
        }
      }) # END observateur pour revenir au dashbord
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # END Gestion 'Afficher module profil' - 'Retour au dashbord'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # START 'module prospection'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Construction de la table de fidélité (répartition prospects, clients fidèles et infidèles)
      # Répétition de code avec get_donnee_profil...
      get_donnee_prospection <- reactive({
        
        # Récupération de la période de visualisation et trigger de la fonction
        periode_date_profil <- c(input$date_prospection_champ_action, input$date_visualisation_profil[2])
        
        # Si la data frame data_prospection existe déjà on l'efface pour ne pas avoir de concurrence d'accès dans la suite des calculs
        if(exists("data_prospection"))
          rm("data_prospection", pos = ".GlobalEnv")
        
        # Recopiage de la table des transactions NAF en local
        tr <- df.n
        # Structuration de l'ensemble des transactions
        tr$siret <- as.factor(tr$siret)
        
        # Subset selon la période de visualisation
        tr <- tr[date %between% c(periode_date_profil[1],periode_date_profil[2])]
        
        # Récupération des 'reliques' transactionnelles
        tr.c <- unique(tr,by=c("client","siret"))[j=list(client,siret)]
        
        # On regroupe la table par client puis on test si le siret du commerçant est présent dans au moins une des transactions du client
        c.group <- group_by(tr.c, client)
        
        # On garde les clients ayant effectué au moins une transaction chez le commerçant connecté
        clients <- filter(c.group, KEY$siret %in% siret)
        
        # Si j'ai au moins un client potentiellement fidèle ou infidèle alors je les tag selon leur type de fidélité
        if(nrow(clients)>0) {
          
          clients <- summarise(clients, n=n())
          clients$fid <- numeric(length = nrow(clients))
          
          # On récupère les informations sur la fidélité des clients avant de supprimer la colonne
          data_prospection <<- c(table(clients$n==1))
          
          # On tag les clients selon qu'ils sont des clients fidèles ou infidèles avec la variable fid
          # fid = 1 : clients fidèles
          # fid = 2 : clients infidèles
          for(i in 1:nrow(clients)) {
            if(clients$n[i]==1) {
              clients$fid[i]=1
            }
            else {clients$fid[i]=2}
          }
          clients$n <- NULL
        } else {
          clients$siret <- NULL
          clients$fid <- numeric()
          data_prospection <<- numeric(length = 3)
        }
        
        # index.fidelite est la data table permettant de connaître le statut de fidélité des clients ayant effectué des achats durant la
        # période de visualisation des données
        # Pour mémo :
        # fid = 1 : clients fidèles
        # fid = 2 : clients infidèles
        # fid = 0 : prospects
        index.fidelite <- rbind.data.frame(data.frame(client = unique(c.group[!(client %in% clients$client)]$client),
                                                       fid = numeric(length = length(unique(c.group[!(client %in% clients$client)]$client)))),
                                            clients)
        
        # Notre data table pour la prospection est prête à l'emploi
        df.p <<- merge(tr, index.fidelite, by = "client")
        
        # On termine de construire la table de fidélité pour le camembert
        data_prospection[3] <<- nrow(unique(tr,by=c("client")))-(sum(data_prospection))
        data_prospection <<- cbind(melt(data_prospection), type = c("Infidèles","Fidèles","Prospects"))
        # Remplacement des NAs par des 0 (cf ci-dessous)
        data_prospection$value[is.na(data_prospection$value)] <<- 0
        return(data_prospection)
      })
      
      output$prospection_resultats <- renderUI({
        # Le bouton de prospection est le trigger
        input$btn_prospection_prospecter
        if(exists("df.prospection")) {
          if(!is.null(df.prospection)) {
          fluidRow(
            column(width = 8, offset = 2, id = "div_prospection_resultats",
                   "Il y a", strong(nrow(df.prospection[df.prospection$dist==1,])), "clients potentiels avec un panier moyen de", strong(round(mean(df.prospection[df.prospection$dist==1,]$panier),1)), "€."
            )
          )
          } else {
            fluidRow(
              column(width = 8, offset = 2, id = "div_prospection_resultats",
                     "Il y a", strong("0"), "clients potentiels. Penser à revoir vos critères de filtrage."
              )
            )
          }
        } else {
          fluidRow(
            column(width = 8, offset = 2, id = "div_prospection_resultats",
                   "Lancer l'algorithme de prospection."
            )
          )
        }
      })
      
      observe({ # observateur isolant le bouton de lancement de la prospection (évite de charger les tables à nouveau)
        
        # Graphique 'Répartition de la clientèle'
        output$graph_profil_prospection <- renderChart({
          # Code JavaScript envoyé au serveur afin de modifier le fichier HTML généré.
          # On passe ainsi de <html class> à <html class='shiny-busy'> et donc busy.js prend la relève
          # L'utilisateur est donc prévenu de l'exécution des calculs en cours
          session$sendCustomMessage(type='jsCode', list(value = "$('html').attr('class','shiny-busy');"))
          
          # On passe en argument uniquement la date jusqu'à laquelle on remonte dans la prospection
          data_profil_prospection <- get_donnee_prospection()
          nPie <- nPlot(
            value ~ type,
            data = data_profil_prospection,
            type = "pieChart"
          )
          nPie$set(dom = "graph_profil_prospection")
          nPie$chart(donut = FALSE, showLegend = TRUE)
          nPie
        })
        
        # Affichage du nombre de clients atteints par la prospection
        output$prospection_previsions <- renderUI({
          # si la prospection n'a pas encore été lancé, on prévient le commerçant de la lancer
          if(!exists("df.prospection")) {
            tagList(tags$h1("0"), tags$h3("Clients potentiels"),
                    tags$h3("Penser à visualiser la répartition de la fidélité des clients puis à lancer la prospection !")
            )
          } else {
            # Affichage des résultats de la prospection
            tagList(tags$h1(nrow(df.prospection[df.prospection$dist==1,])), tags$h3("Clients potentiels"),
                    tags$h1(ifelse(length(df.prospection$dist)!=0,paste(round(mean(df.prospection$panier),1), "€"),"0 €")), tags$h3("Panier moyen client"),
                    tags$h1(ifelse(length(df.prospection$dist)!=0,paste(nrow(df.prospection[df.prospection$dist==1,])*floor(mean(df.prospection$panier)), "€"), "0 €")), tags$h3("Bénéfices potentiels*")
            )
          }
        })
        
        # Fonction construisant la chaîne de caractères pour les critères situation familiale et CSP
        chainage_criteres_prospection <- function(e) {
          if(length(e)!=0) {
            e.chaine <- ""
            if(length(e)==1) {
              e.chaine <- HTML(paste(e.chaine, tags$strong(e)))
            } else {
              for(i in 1:(length(e)-1)) {
                e.chaine <- HTML(paste(e.chaine, tags$strong(e[i]), ", ", sep = ""))  
              }
              e.chaine <- HTML(paste(e.chaine, tags$strong(e[length(e)])))
            }
          }
        }
        
        # Algorithme de prospection : submit
        algo_prospection <- reactive({
          # Trigger pour l'affichage du panneau "Calcul en cours..."
          session$sendCustomMessage(type='jsCode', list(value = "$('html').attr('class','shiny-busy');"))
          
          # Est directement dépendant de l'action sur la bouton input$btnChampAction
          # Lorsque btnChampAction est appuyé, toutes les fonctions ci-dessous sont exécutées
          input$btn_prospection_prospecter
          
          # On isole la parallélisation du calcul de prospection afin qu'il ne soit exécuté uniquement lorsque le client le désire réellement 
          isolate({
            
            if(nrow(df.p)==0) {
              # Faire apparaître une fenêtre modale ordonnant l'utilisteur de visualiser le profil des clients
              toggleModal(session, "modal_prospection_warning")
            } else {
              # On subset la table de prospection pour ne garder les choix du commerçant
              df.prospection <- unique(df.p %>%
                                         group_by(client) %>%
                                         summarise(panier=mean(montant),
                                                   date=max(date),
                                                   age=age,
                                                   sexe=sexe,
                                                   csp=csp,
                                                   situation=situation,
                                                   villeclient=villeclient,
                                                   libelle=libelle,
                                                   reseau=reseau,
                                                   paiement=paiement,
                                                   retrait=retrait,
                                                   typecompte=typecompte,
                                                   Glat=Glat,
                                                   Glon=Glon,
                                                   fid=fid))
              
              # Les filtres ne s'appliquent qu'aux prospects
              # Si le commerçant décide de prospecter les clients fidèles et/ou infidèles alors ils ne sont pas comptabilisés
              # Cas où il veut prospecter que les clients infidèles : alors les clients fidèles sont soumis aux mêmes filtres que les prospects
              print(input$btngrp_prospection_clients)
              if(!is.null(input$btngrp_prospection_clients)) {
                df.prospection.fideles <- cbind(subset(df.prospection, fid %in% input$btngrp_prospection_clients), dist = c(1))
                df.prospection <- subset(df.prospection, !(fid %in% input$btngrp_prospection_clients))
              }
              # On subset la data table contenant les clients à prospecter en fonction des critères du commerçant
              # D'abord on subset par sexe et âge
              if(!is.null(input$btngrp_prospection_genre)) {
                df.prospection <- subset(df.prospection, sexe %in% input$btngrp_prospection_genre)
              }
              if(!is.null(input$slider_input_prospection_age)) {
                df.prospection <- subset(df.prospection, age %between% input$slider_input_prospection_age)
              }
              # La condition if empêche le subset dans le cas où le critère n'a pas été précisé
              # Puis par situation
              if(!is.null(input$btngrp_prospection_situation)) {
                df.prospection <- subset(df.prospection, situation %in% input$btngrp_prospection_situation)
              }
              
              # Enfin par csp
              if(!is.null(input$btngrp_prospection_csp)) {
                df.prospection <- subset(df.prospection, csp %in% input$btngrp_prospection_csp)
              }
              
              # Pour mémo :
              # fid = 1 : clients fidèles
              # fid = 2 : clients infidèles
              # fid = 0 : prospects
              # Le vecteur input$checkBoxFideliteProspection contient les valeurs 1 et 2 selon le choix du commerçant
              
              # Data table finale
              # dist = 0 : n'est pas présent dans le champ d'action
              # dist = 1 : est présent dans le champ d'action
              
              # Condition if : le commerçant souhaite t-il prospecter nécessairement les clients fidèles et/ou infidèles ?
              df.prospection <<- if(!is.null(input$btngrp_prospection_clients)) {
                # Si oui, la prospection porte t-elle EXCLUSIVEMENT sur ces clients ?
                if(nrow(df.prospection)!=0) {
                  # Si df.prospection est non nul, alors les critères sont tels que la prospection est porte aussi sur des cliens de fid = 0
                  rbind.data.frame(
                    # Résidu de prospection hors choix du commerçant sur les clients fidèles et infidèles
                    distance_cc(df.prospection),
                    # Table contenant le choix du commerçant sur les clients fidèles et infidèles
                    df.prospection.fideles
                  )
                } else {
                  # Les critères sont tels qu'il n'y a pas de prospects
                  # On retourne seulement la table des fidèles/infidèles car nous savons qu'elle existe
                  df.prospection.fideles
                }
              } else {
                # Si le commerçant souhaite prospecter tout le monde indifférement, clients fidèles/infidèles et prospects
                if(nrow(df.prospection)!=0) {
                  # Si les critères font qu'il y a des prospects, alors on l'envoi dans distance_cc
                  distance_cc(df.prospection)
                }
              }
            }
            # Fin de l'isolement : supprime la dépendance de input$numericInputChampAction
          })
        })
        
        distance_cc <- function(df) {
          cbind(df,
                # Variable catégorique donnant la présence ou non du client dans le champ d'action du commerçant                        
                dist = foreach(line = iter(df, by = 'row'), .combine = 'c', .packages = c('parallel','doParallel','foreach')) %do% {
                  val <- acos(sin(latC)*sin(line$Glat*pi/180)+cos(latC)*cos(line$Glat*pi/180)*cos(line$Glon*pi/180-lonC)) * rayon
                  if(!is.na(val)) {
                    ifelse(val <= input$numeric_input_prospection_action, 1, 0)
                  } else {
                    print(line)
                    return(0)
                  }
                })
        }
        
        algo_prospection() # Listener sur l'algorithme de prospection ci-desssus
        
      }) # Fin observateur 'propsection'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # END 'module prospection'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # START Gestion 'Afficher module prospection' - 'Retour au dashbord'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      observe({ # Observateur pour afficher le module de prospection
        if(input$btn_prospection>0) {
          input$btn_prospection
          session$sendCustomMessage(type='jsCode', list(value = "$(document.getElementById('gridster_dashbord')).css('display','none');"))
          session$sendCustomMessage(type='jsCode', list(value = "$(document.getElementById('gridster_profil')).css('display','none');"))
          session$sendCustomMessage(type='jsCode', list(value = "var_prospection = true"))
        }
      }) # END observateur pour afficher le module de prospection
      
      observe({ # Observateur pour revenir au dashbord
        if(!is.null(input$btn_prospection_retour)) {
          if(input$btn_prospection_retour>0) {
            input$btn_prospection_retour
            session$sendCustomMessage(type='jsCode', list(value = "var_prospection = false"))
            session$sendCustomMessage(type='jsCode', list(value = "$(document.getElementById('gridster_profil')).css('display','none');"))
            session$sendCustomMessage(type='jsCode', list(value = "$(document.getElementById('gridster_dashbord')).css('display','node');"))
          }
        }
      }) # END observateur pour revenir au dashbord
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # END Gestion 'Afficher module prospection' - 'Retour au dashbord'
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    } # END if(USER$Logged==TRUE)
  }) # END observe global
})