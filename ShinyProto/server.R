load( "onePrev.Rdata" )
load( "twoPrev.Rdata" )
load( "threePrev.Rdata" )
load( "fourPrev.Rdata" )

source( "4_Modelling.R" )

shinyServer(function(input, output) {


  output$nextWord <- renderText({
    userSentence <- input$userSentence
    if(
        ( substr( userSentence, nchar( userSentence ), nchar( userSentence )) != " " ) ||
        ( grepl( "^ *$", userSentence ))
      ){
      nextWord <- "WAITING FOR A SPACE ..."
    }else{
      nextWord <- PredictFromLastFew( StringToTokens( userSentence ))
    }
  nextWord
  })

})
