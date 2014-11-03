# Carte centrée sur le commerçant connecté à la connexion
output$carte <- renderUI({
  leafletMap(
    "map", "100%", "100%",
    initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
    initialTileLayerAttribution = HTML('Designed by <a href="mailto:juanmanuel.munozperez@gmail.com">Juan Manuel Munoz Perez</a>'),
    options = list(
      zoom = 16,
      center = c(KEY$lat, KEY$lon),
      maxBounds = list(list(47.408167, -2.644132), list(48.836306, -0.743497))
    )
  )
})

# Panneau pour la visualisation selon une date et un type de comparaison de la carte
output$panneau_comparaison_carte <- renderUI({
  absolutePanel(id = "composants_compare_carte", class = "modal", fixed = FALSE, draggable = TRUE,
                top = 57, left = 330, right = "auto", bottom = "auto",
                width = "auto", height = "auto",
                selectInput("type_comparaison_carte", "Comparer par", c("Montants", "Transactions")),
                dateRangeInput("date_visualisation_carte", label = "P\u00E9riode de visualisation des donn\u00E9es", start = "2013-05-01", end = "2013-05-08", min = "2012-09-28", max = "2013-10-02",
                               format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "fr", separator = " \u00E0 "),
                bsActionButton(inputId = "btn_map_retour", label = "Retour", style = "success")
  )
})