shinyUI(fluidPage(

  h4( "Instructions" ),

  helpText( "Start typing words. Every time you type one or more spaces, the",
            "app will suggest the next word for the sentence." ),

  hr(),

  textInput("userSentence", label = h4("Sentence input"), value = ""),

  hr(),

  fluidRow(column(3, verbatimTextOutput( "nextWord" )))

))

