shinyUI(fluidPage(

  h4( "Instructions" ),

  helpText( "Type a sentence. Every time you type one or more spaces, the",
            "app will suggest the next word." ),

  hr(),

  textInput("userSentence", label = h4("Sentence input"), value = ""),
  tags$head( tags$style(type="text/css", "#userSentence {width: 600px}" )),

  hr(),

  h4( "Suggested next word" ),

  fluidRow(column(3, verbatimTextOutput( "nextWord" ))),

  actionButton( "acceptSuggestion", "Accept" )


))

