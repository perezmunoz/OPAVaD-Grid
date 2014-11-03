library(shiny)
library(shinyBS)
library(rCharts)
library(ShinyDash)
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

shinyUI(fluidPage(
  # Scripts
  tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
            tags$script(type="text/javascript", src = "busy.js"),
            tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",
                             function(message) {
                             eval(message.value);
                             }
            );
                             '))),
  # Chargement de cours...
  div(class = "busy", id = "loading",
      p("Affichage en cours..."),
      img(src="ajaxloader.gif")
  ),
  # UI de connexion
  div(class = "login",
      uiOutput("uiLogin")
  ),
  # UI dashbord
  conditionalPanel(condition = 'connexion == true & var_load_data == true', id = "panel_dashbord",
                   h1("Dashbord", style = "color: black;"),
                   gridster(tile.width = 200, tile.height = 200, id = 'gridster_dashbord',
                            gridsterItem(id = "id_mon_commerce", row = 1, col = 2, size.x = 1, size.y = 1,
                                         uiOutput("mon_commerce")
                            ),
                            gridsterItem(id = "id_visualiser_carte", row = 1, col = 3, size.x = 1, size.y = 1,
                                         h4("Pour visualiser la carte interactive, cliquer sur le bouton."),
                                         bsActionButton("btn_map", label = "Carte", style = "primary")
                            ),
                            gridsterItem(id = "id_visualiser_ca", row = 1, col = 4, size.x = 1, size.y = 1,
                                         h4("Pour visualiser la distribution de votre chiffre d'affaires sur une période donnée, cliquer sur le bouton."),
                                         bsActionButton("btn_ca", label = "Turnover", style = "primary")
                            ),
                            gridsterItem(id = "id_visualiser_profil", row = 2, col = 3, size.x = 1, size.y = 1,
                                         h4("Pour visualiser la distribution de votre chiffre d'affaires sur une période donnée, cliquer sur le bouton."),
                                         bsActionButton("btn_profil", label = "Profil", style = "primary")
                            ),
                            gridsterItem(id = "id_visualiser_prospection", row = 2, col = 4, size.x = 1, size.y = 1,
                                         h4("Pour visualiser la page dédiée à la prospection, cliquer sur le bouton."),
                                         bsActionButton("btn_prospection", label = "Prospection", style = "primary")
                            )
                   )
  ),
  # UI carte
  conditionalPanel(condition = 'var_carte == true',
                   uiOutput("carte"),
                   uiOutput("panneau_comparaison_carte")
  ),
  # UI ca
  conditionalPanel(condition = 'var_ca == true',
                   uiOutput("panneau_comparaison_ca"),
                   showOutput("graph_ca_montant", "nvd3"),
                   showOutput("graph_ca_transaction", "nvd3")
  ),
  # UI profil
  conditionalPanel(condition = 'var_profil == true',
                   gridster(tile.width = 200, tile.height = 200, id = 'gridster_profil',
                            gridsterItem(id = "id_comparer_profil", row = 1, col = 2, size.x = 1, size.y = 1,
                                         # La construction est immédiate pour éviter de charger deux fois les piegraphs
                                         dateRangeInput("date_visualisation_profil", label = "P\u00E9riode de visualisation des donn\u00E9es", start = "2013-04-08", end = "2013-04-15", min = "2012-09-28", max = "2013-10-02",
                                                        format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "fr", separator = " \u00E0 "),
                                         bsActionButton(inputId = "btn_profil_retour", label = "Retour", style = "success")
                            ),
                            gridsterItem(id = "id_profil", row = 1, col = 3, size.x = 2, size.y = 2,
                                         showOutput("graph_profil", "nvd3")
                            ),
                            gridsterItem(id = "id_profil_genre", row = 3, col = 4, size.x = 2, size.y = 2,
                                         showOutput("graph_profil_genre", "nvd3")
                            ),
                            gridsterItem(id = "id_profil_age", row = 3, col = 5, size.x = 2, size.y = 2,
                                         showOutput("graph_profil_age", "nvd3")
                            ),
                            gridsterItem(id = "id_profil_situation", row = 2, col = 2, size.x = 3, size.y = 2,
                                         showOutput("graph_profil_situation", "nvd3")
                            ),
                            gridsterItem(id = "id_profil_csp", row = 1, col = 4, size.x = 3, size.y = 2,
                                         showOutput("graph_profil_csp", "nvd3")
                            )
                   )
  ),
  # UI prospection
  conditionalPanel(condition = 'var_prospection == true',
                   #                    bsModal("modal_prospection_warning", "Profil des clients", trigger = "",
                   #                            p("Test")
                   #                    )
                   fluidRow(
                     column(width = 8, offset = 2, id = "div_btn_prospection_retour",
                            bsActionButton(inputId = "btn_prospection_retour", label = "Retour", style = "success")
                     )
                   ),
                   fluidRow(class = "div_collapse",
                            column(width = 8, offset = 2,
                                   bsCollapse(multiple = FALSE, id = "collapse_groupe",
                                              bsCollapsePanel("Champ spatio-temporel de la prospection", id = "collapse_action", value = "collapse_action_val",
                                                              "La prospection des clients est à deux dimensions :", strong("spatiale"), "et", strong("temporelle."), br(),
                                                              paste("Sélectionner la date jusqu'à laquelle portera le calcul de prospection (le point d'origine étant aujourd'hui", "2013-04-15"),
                                                              dateInput(inputId = "date_prospection_champ_action", label = "", value = "2013-04-08", min = "2012-09-28", max = "2013-04-15"),
                                                              "Sélectionner le rayon du cercle", strong("en km"), "par rapport auquel l'algorithme piochera les clients.",
                                                              numericInput(inputId = "numeric_input_prospection_action", label = "", value = 10, min = 0, step = 1)
                                              ),
                                              bsCollapsePanel("Forcer la prospection des clients fidèles et infidèles", id = "collapse_fideles", value = "collapse_fideles_val",
                                                              "En cochant les cases, l'algorithme de prospection inclut les client fidèles et/ou infidèles sans tenir compte de leur caractéristiques.",
                                                              bsButtonGroup("btngrp_prospection_clients", label = "", toggle = "checkbox", value = "",
                                                                            bsButton("btn_clients_fideles", label = "Clients fidèles", value = "1"),
                                                                            bsButton("btn_clients_infideles", label = "Clients infidèles", value = "2")
                                                              )
                                              ),
                                              bsCollapsePanel("Genre", id = "collapse_genre", value = "collapse_genre_val",
                                                              "Sélectionner le genre des prospects",
                                                              bsButtonGroup("btngrp_prospection_genre", label = "", toggle = "checkbox", value = "",
                                                                            bsButton("btn_genre_male", label = "Homme", value = "M"),
                                                                            bsButton("btn_genre_femelle", label = "Femme", value = "F"))
                                              ),
                                              bsCollapsePanel("Age", id = "collapse_age", value = "collapse_age_val",
                                                              "Sélectionner l'âge des prospects",
                                                              sliderInput("slider_input_prospection_age", label = "", min = 0, 
                                                                          max = 100, value = c(18, 25))
                                              ),
                                              bsCollapsePanel("Situation familiale", id = "collapse_situation", value = "collapse_situation_val",
                                                              "Sélectionner la situation familiale des prospects",
                                                              bsButtonGroup("btngrp_prospection_situation", label = "", toggle = "checkbox", value = "",
                                                                            bsButton("btn_situation_celib", label = "Célibataire", value = "CELIBATAIRE"),
                                                                            bsButton("btn_situation_concubin", label = "Concubin", value = "CONCUBIN"),
                                                                            bsButton("btn_situation_divorce", label = "Divorcé", value = "DIVORCE"),
                                                                            bsButton("btn_situation_init", label = "Init", value = "INIT"),
                                                                            bsButton("btn_situation_marie", label = "Marié", value = "MARIE"),
                                                                            bsButton("btn_situation_pacsé", label = "Pacsé", value = "PACSE"),
                                                                            bsButton("btn_situation_separe", label = "Séparé", value = "SEPARE"),
                                                                            bsButton("btn_situation_veuf", label = "Veuf", value = "VEUF"))
                                              ),
                                              bsCollapsePanel("Catégorie socioprofessionnelle", id = "collapse_csp", value = "collapse_csp_val",
                                                              "Sélectionner la csp des prospects",
                                                              bsButtonGroup("btngrp_prospection_csp", label = "", toggle = "checkbox", value = "",
                                                                            bsButton("btn_csp_agriculteurs", label = "Agriculteurs", value = "AGRICULTEURS"),
                                                                            bsButton("btn_csp_artisans", label = "Artisans, Commerçants et Chefs d'Entreprises", value = "ARTISANTS COMMERCANTS ET CHEFS D'ENTREPRISES"),
                                                                            bsButton("btn_csp_chomage", label = "Sans activité prof.", value = "AUTRES SANS ACTIVITE PROF."),
                                                                            bsButton("btn_csp_cadres", label = "Cadres et prof. intellectuelles", value = "CADRES ET PROF INTELLECTUELLES"),
                                                                            bsButton("btn_csp_employes", label = "Employés", value = "EMPLOYES"),
                                                                            bsButton("btn_csp_ouvriers", label = "Ouvriers", value = "OUVRIERS"),
                                                                            bsButton("btn_csp_intermediaire", label = "Prof. intermédiaires", value = "PROFESSIONS INTERMEDIAIRES"),
                                                                            bsButton("btn_csp_retraite", label = "Retraités", value = "RETRAITES"))
                                              )
                                   )
                            )
                   ),
                   fluidRow(
                     column(width = 8, offset = 2, id = "div_btn_prospection",
                            bsActionButton(inputId = "btn_prospection_prospecter", label = "Prospecter", style = "success")
                     )
                   ),
                   uiOutput("prospection_resultats"),
                   showOutput("graph_profil_prospection", "nvd3")
  )
))