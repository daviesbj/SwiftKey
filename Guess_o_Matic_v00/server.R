##
# Predictor tuning
##
lnWeights <- c( 9.171, 6.316, 12.497, 6.416 )

source( "Guess_o_Matic.R" )

weights <- NormalizeWeights( c( 1, exp( lnWeights )))
placeHolder <- "WAITING FOR A SPACE ..."

shinyServer( function( input, output, session ) {

  output$nextWord <- renderText({
    userSentence <- input$userSentence
    if(
        ( substr( userSentence, nchar( userSentence ), nchar( userSentence )) == " " ) &&
        ( ! grepl( "^ *$", userSentence ))
      ){
      nextWord <- PredictNext( OnlineTokener( userSentence ), weights = weights )
    } else if( substr( userSentence, nchar( userSentence ), nchar( userSentence )) %in% c( "?", "!", "." ) ){
      updateTextInput( session, "userSentence", value = "" )
      nextWord <- placeHolder
    } else {
      nextWord <- placeHolder
    }
  nextWord
  })

  observeEvent( input$acceptButton, {
    userSentence <- input$userSentence
    if(
      ( substr( userSentence, nchar( userSentence ), nchar( userSentence )) == " " ) &&
      ( ! grepl( "^ *$", userSentence ))
    ){
      nextWord <- PredictNext( OnlineTokener( userSentence ), weights = weights )
      newSentence <- paste0( c( input$userSentence, nextWord, " "), collapse = "" )
      updateTextInput( session, "userSentence", value = newSentence )
    }

  } )

  observeEvent( input$resetButton, {
    updateTextInput( session, "userSentence", value = "" )
  } )

})
