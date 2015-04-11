source( "1_LoadingAndCleaning.R" )
load( "knownWords.Rdata" )

PredictFromLastFew <- function( tokenArray, verbose = TRUE ){
  nTokens <- length( tokenArray )
  nextTok <- "the"
  for( k in (min(length(tokenArray),4)):1 ){
    lastTokens <- tokenArray[(nTokens-k+1):(nTokens)]
    lastNgram <- paste0( lastTokens, collapse = ":" )
    if( k == 4 )  nextTokFrame <- subset( fourPrev, previous == lastNgram )
    else if( k == 3 )  nextTokFrame <- subset( threePrev, previous == lastNgram )
    else if ( k == 2 ) nextTokFrame <- subset( twoPrev, previous == lastNgram )
    else if( k == 1 ) nextTokFrame <- subset( onePrev, previous == lastNgram )
    else stop( "Can't get here" )
    if( length( nextTokFrame$nextTok ) > 0 ){
      nextTok <- as.character( nextTokFrame$nextTok )
      break
    }
  }
nextTok
}





StringToTokens <- function( charString, verbose = FALSE, addStart = TRUE ){

  if( verbose ) message(" ... lowercasing" )
  charString <- tolower( charString )

  # Protect possessive apostrophes
  for( iSpec in 1:nSpecial ){
    if( verbose ) message( " ... protecting ", specialRegex[ iSpec ], " to ", specialPlaceholder[ iSpec ] )
    charString <- gsub( specialRegex[ iSpec ], specialPlaceholder[ iSpec ], charString, perl = TRUE )
  }

  if( verbose ) message(" ... removing other apostrophes" )
  charString <- gsub( "'", "", charString, fixed = TRUE )

  # Restore possessive apostrophes
  for( iSpec in 1:nSpecial ){
    if( verbose ) message( " ... restoring ", specialPlaceholder[ iSpec ], " to ", specialFinal[ iSpec ] )
    charString <- gsub( specialPlaceholder[ iSpec ], specialFinal[ iSpec ], charString, fixed = TRUE )
  }

  # Remove all other characters
  if( verbose ) message(" ... removing other characters" )
  charString <- gsub( "[^abcdefghijklmnopqrstuvwxyz0123456789' ]", " ", charString, perl = TRUE )

  if( verbose ) message(" ... removing isolated possessives" )
  charString <- gsub( "^'s\\b|\\s's\\b", " ", charString, perl = TRUE ) # By itself for some reason

  if( verbose ) message(" ... compressing whitespace" )
  charString <- gsub( "\\s+", " ", charString, perl = TRUE )

  if( verbose ) message(" ... removing leading & trailing spaces" )
  charString <- gsub( "^ | $", "", charString, perl = TRUE )

  if( verbose ) message(" ... tokening" )
  tokArray <- strsplit( charString, ' ', fixed = TRUE )[[1]]

  if( verbose ) message(" ... flagging profanities" )
  tokArray <- NoProfWithPatterns( tokArray )

  if( verbose ) message(" ... identifying some non-words" )
  tokArray <- gsub( '^.*(.)\\1\\1.*$', 'NONWORD', tokArray, perl = TRUE )
  tokArray <- gsub( '^.*([hjkquwxy])\\1.*$', 'NONWORD', tokArray, perl = TRUE )
  tokArray <- gsub( '^.*x[^aeioy].*$', 'NONWORD', tokArray, perl = TRUE )
  tokArray <- gsub( '^[bcdefghjklmnopqrstuvwxyz]$', 'LETTER', tokArray, perl = TRUE )
  tokArray <- gsub( '^[0123456789]$', 'DIGIT', tokArray, perl = TRUE )

  if( verbose ) message(" ... identifying some alphanumerics" )
  tokArray <- gsub( '^[0123456789]+$', 'NUMBER', tokArray, perl = TRUE )
  tokArray <- gsub( '^[abcdefghijklmnopqrstuvwxyz]+[0123456789].*$', 'ALPHANUM', tokArray, perl = TRUE )
  tokArray <- gsub( '^[0123456789]+[abcdefghijklmnopqrstuvwxyz].*$', 'ALPHANUM', tokArray, perl = TRUE )


  if( verbose ) message(" ... Flagging hapaxes" )
  isHapax <- ! ( tokArray %in% knownWords | grepl( "[ABCDEFGHIJKLMNOPQRSTUVWXYZ]", tokArray, perl = TRUE ))

  tokArray[ isHapax ] <- "HAPAX"

  if( addStart ) tokArray <- c( "START", tokArray )

  tokArray
}

