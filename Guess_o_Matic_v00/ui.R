shinyUI(fluidPage(

  h1( "Guess-o-Matic™ Demonstration Page" ),

  h4( "Instructions" ),

  helpText( "The app takes a few seconds to initialize.",
            "Once you see the message \"WAITING FOR A SPACE ...\", it's ready." ),
  helpText( "Start typing a sentence.",
            "Every time you type a space, the app will suggest the next word." ),
  helpText( "If you like it, hit ACCEPT. Otherwise, just keep typing.",
            "Hit RESET or type a period (full stop) to clear the input." ),

  hr(),

  actionButton( "resetButton", "RESET" ),


  textInput("userSentence", label = h4("Type your input sentence here"), value = "" ),
  tags$head( tags$style(type="text/css", "#userSentence {width: 750px}" )),

  h4( "Suggested next word" ),

  fluidRow(column(3, verbatimTextOutput( "nextWord" ))),

  actionButton( "acceptButton", "ACCEPT" )


))

