# Variable réactive pour la validation de la connexion
USER <- reactiveValues(Logged = Logged)

# UI de connexion
output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      isolate({textInput(inputId = "userName", label = "Num\u00E9ro de Siret :", value = "44031091000029")}),
      br(),
      actionButton("Login", "Connexion"),
      br(), br(),
      textOutput("pass")
    )
  }
})

# Validation de la connexion
output$pass <- renderText({
  if (USER$Logged == FALSE) {
    if (!is.null(input$Login)) {
      if(input$Login>0) {
        isolate({
          KEY <<- index[index$siret == input$userName, ]
        })
        if(nrow(KEY) != 0) {
          if (KEY$ca == 'O') {
            session$sendCustomMessage(type='jsCode', list(value = "connexion = true;"))
            USER$Logged <- TRUE
            "Connexion validée"
          } else  {
            session$sendCustomMessage(type='jsCode', list(value = "connexion = false;"))
            "Num\u00E9ro de Siret incorrecte !"
          }
        } else {
          session$sendCustomMessage(type='jsCode', list(value = "connexion = false;"))
          "Num\u00E9ro de Siret incorrecte !"
        }
      }
    }
  }
})