specialRegex <-       c( "\\B's\\b", "\\bi'm\\b", "\\bi'd\\b", "\\Bn't\\b" )
specialPlaceholder <- c( "SS",       "IM",        "ID",       "NT" )
specialFinal <-       c("'s",       "i'm",        "i'd",      "n't" )
nSpecial <- length( specialRegex )
profanities <- readLines( '~/SwiftKey/profanity.txt' )
profanities <- unique( profanities[ order( profanities )])
profanityPatterns <- paste0( profanities, collapse = '|' )

ToSentenceArray <- function( inText ){
  unlist( sapply( as.list( inText ),
                  function(x) strsplit( x[1], '.', fixed = TRUE)
  ))
}

CleanSentenceArray <- function( workArray, verbose = TRUE ){

  if( verbose ) message(" ... lowercasing" )
  workArray <- tolower( workArray )

  # Protect possessive apostrophes
  for( iSpec in 1:nSpecial ){
    if( verbose ) message( " ... protecting ", specialRegex[ iSpec ], " to ", specialPlaceholder[ iSpec ] )
    workArray <- gsub( specialRegex[ iSpec ], specialPlaceholder[ iSpec ], workArray, perl = TRUE )
  }

  if( verbose ) message(" ... removing other apostrophes" )
  workArray <- gsub( "'", "", workArray, fixed = TRUE )

  # Restore possessive apostrophes
  for( iSpec in 1:nSpecial ){
    if( verbose ) message( " ... restoring ", specialPlaceholder[ iSpec ], " to ", specialFinal[ iSpec ] )
    workArray <- gsub( specialPlaceholder[ iSpec ], specialFinal[ iSpec ], workArray, fixed = TRUE )
  }

  # Remove all other characters
  if( verbose ) message(" ... removing other characters" )
  workArray <- gsub( "[^abcdefghijklmnopqrstuvwxyz' ]", " ", workArray, perl = TRUE )

  if( verbose ) message(" ... removing isolated possessives" )
  workArray <- gsub( "^'s\\b|\\s's\\b", " ", workArray, perl = TRUE ) # By itself for some reason

  if( verbose ) message(" ... compressing whitespace" )
  workArray <- gsub( "\\s+", " ", workArray, perl = TRUE )

  if( verbose ) message(" ... removing leading & trailing spaces" )
  workArray <- gsub( "^ | $", "", workArray, perl = TRUE )

  if( verbose ) message(" ... changing to a list" )
  textList <- as.list( workArray )
  rm( workArray )

  if( verbose ) message(" ... tokening" )
  textList <- sapply( textList, function(x) strsplit( x[1], ' ', fixed = TRUE ) )

  if( verbose ) message(" ... removing very short sentences" )
  textList[ sapply( textList, function(x){ length(x) < 2 } )] <- NULL

textList
}

NoProfWithPatterns <- function( someWords ){
  gsub( profanityPatterns, 'PROFANITY', someWords )
}
