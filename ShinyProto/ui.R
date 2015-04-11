shinyUI(fluidPage(

  h1( "Guess-o-Matic™ Demonstration Page" ),

  h4( "Instructions" ),

  helpText( "You might have to wait a few moments for data files to get loaded.",
            "Once you see the message \"WAITING FOR A SPACE ...\", it's ready." ),
  helpText( "Start typing a sentence.",
            "Every time you type one or more spaces, the app will suggest the next word." ),
  helpText( "If you like it, hit the ACCEPT button. Otherwise, just keep typing." ),
  helpText( "Use the RESET button to clear the input window" ),

  hr(),

  actionButton( "resetButton", "RESET" ),


  textInput("userSentence", label = h4("Type your input sentence here"), value = "" ),
  tags$head( tags$style(type="text/css", "#userSentence {width: 600px}" )),

  h4( "Suggested next word" ),

  fluidRow(column(3, verbatimTextOutput( "nextWord" ))),

  actionButton( "acceptButton", "ACCEPT" )


))

