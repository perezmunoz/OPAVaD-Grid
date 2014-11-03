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
                   h1("Dashbord"),
                   gridster(tile.width = 200, tile.height = 200, id = 'gridster_dashbord',
                            gridsterItem(id = "id_mon_commerce", row = 1, col = 2, size.x = 1, size.y = 1,
                                         uiOutput("mon_commerce")
                            ),
                            gridsterItem(id = "id_visualiser_carte", row = 1, col = 3, size.x = 1, size.y = 1,
                                         h4("Pour visualiser la carte interactive, cliquer sur le bouton."),
                                         bsActionButton("btn_map", label = "Carte", style = "primary")
                            ),
                            gridsterItem(id = "id_visualiser_ca", row = 2, col = 2, size.x = 1, size.y = 1,
                                         h4("Pour visualiser la distribution de votre chiffre d'affaires sur une période donnée, cliquer sur le bouton."),
                                         bsActionButton("btn_ca", label = "Turnover", style = "primary")
                            ),
                            gridsterItem(id = "id_visualiser_profil", row = 2, col = 3, size.x = 1, size.y = 1,
                                         h4("Pour visualiser la distribution de votre chiffre d'affaires sur une période donnée, cliquer sur le bouton."),
                                         bsActionButton("btn_profil", label = "Profil", style = "primary")
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
                                         dateRangeInput("date_visualisation_profil", label = "P\u00E9riode de visualisation des donn\u00E9es", start = "2013-05-01", end = "2013-05-08", min = "2012-09-28", max = "2013-10-02",
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
  )
))