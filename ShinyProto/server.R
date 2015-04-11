remoteDataFiles <- c(
  "onePrev.Rdata",
  "twoPrev.Rdata",
  "threePrev.Rdata",
  "fourPrev.Rdata",
  "profanity.txt",
  "knownWords.Rdata"
)

library( curl )

for( dataFile in remoteDataFiles ){
  if( file.exists( dataFile )) next
  theURL <- paste( 'https://s3.amazonaws.com/daviesbjCoursera/ShinyProto/', dataFile, sep = '' )
  download.file( theURL, destfile = dataFile, method = "curl" )
}

attach( "onePrev.Rdata" )
attach( "twoPrev.Rdata" )
attach( "threePrev.Rdata" )
attach( "fourPrev.Rdata" )
attach( "knownWords.Rdata" )

source( "4_Modelling.R" )

shinyServer(function(input, output, session ) {

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

  observeEvent( input$acceptButton, {
    userSentence <- input$userSentence
    if(
      ( substr( userSentence, nchar( userSentence ), nchar( userSentence )) == " " ) &&
      ( ! grepl( "^ *$", userSentence ))
    ){
      nextWord <- PredictFromLastFew( StringToTokens( userSentence ))
      newSentence <- paste0( c( input$userSentence, nextWord, " "), collapse = "" )
      updateTextInput( session, "userSentence", value = newSentence )
    }

  } )

  observeEvent( input$resetButton, {
    updateTextInput( session, "userSentence", value = "" )
  } )

})
