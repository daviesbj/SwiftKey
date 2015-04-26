shinyUI(fluidPage(

  h1( "Guess-o-Matic™ Demonstration Page" ),

  h4( "Instructions" ),

  helpText( "(1) The app takes several seconds to start up.",
            "When you see the message \"WAITING FOR A SPACE ...\", it's ready." ),
  helpText( "(2) Start typing a sentence.",
            "Click RESET or type a period (full stop) to clear the input."
             ),
  helpText( "(3) When you type a space, the app will suggest the next word.",
            "If you like it, click ACCEPT, or just keep typing."
             ),
  helpText( "(4) Nothing will happen if you just type spaces -- you have to type",
            "at least one word."
             ),

  hr(),

  actionButton( "resetButton", "RESET" ),


  textInput("userSentence", label = h4("Type your input sentence here"), value = "" ),
  tags$head( tags$style(type="text/css", "#userSentence {width: 750px}" )),

  h4( "Suggested next word" ),

  fluidRow(column(3, verbatimTextOutput( "nextWord" ))),

  actionButton( "acceptButton", "ACCEPT" )


))

