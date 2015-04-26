# library( curl )
# source( "4_Modelling.R" )
#
# ##
# # Load datafiles
# ##
#
# for( dataFile in remoteDataFiles ){
#   if( file.exists( dataFile )) next
#   theURL <- paste( 'https://s3.amazonaws.com/daviesbjCoursera/ShinyProto/', dataFile, sep = '' )
#   download.file( theURL, destfile = dataFile, method = "curl" )
# }


source( "ServerFunctions.R" )
library( dplyr )

for( rDfile in list.files( ".", "*.Rdata" )) attach( rDfile )

profanities <- readLines( 'profanity.txt' )
profanities <- unique( profanities[ order( profanities )])
profanityPatterns <- paste0( profanities, collapse = '|' )
rm( profanities )

malenames <- readLines( 'boys.txt' )
maleposs <- c( paste0( malenames, "'s"), paste0( malenames, "s") )
femalenames <- readLines( 'girls.txt' )
femaleposs <- c( paste0( femalenames, "'s"), paste0( femalenames, "s") )
lastnames <- readLines( 'lastnames.txt' )
countries <- readLines( 'countries.txt' )
states <- readLines( 'us_states.txt' )
uscities <- readLines( 'us_cities.txt' )
worldcities <- readLines( 'world_cities.txt' )

##
# Predictor tuning
##

shinyServer(function(input, output, session ) {

  output$nextWord <- renderText({
    userSentence <- input$userSentence
    if(
        ( substr( userSentence, nchar( userSentence ), nchar( userSentence )) != " " ) ||
        ( grepl( "^ *$", userSentence ))
      ){
      nextWord <- "WAITING FOR A SPACE ..."
    }else{
      nextWord <- PredictNext( OnlineTokener( userSentence ))
    }
  nextWord
  })

  observeEvent( input$acceptButton, {
    userSentence <- input$userSentence
    if(
      ( substr( userSentence, nchar( userSentence ), nchar( userSentence )) == " " ) &&
      ( ! grepl( "^ *$", userSentence ))
    ){
      nextWord <- PredictNext( OnlineTokener( userSentence ))
      newSentence <- paste0( c( input$userSentence, nextWord, " "), collapse = "" )
      updateTextInput( session, "userSentence", value = newSentence )
    }

  } )

  observeEvent( input$resetButton, {
    updateTextInput( session, "userSentence", value = "" )
  } )

})
