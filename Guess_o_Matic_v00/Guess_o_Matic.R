library( dplyr )
library( curl )

dataFile <- "GMdata.zip"
if( ! file.exists( dataFile )){
  theURL <- paste( 'https://s3.amazonaws.com/daviesbjCoursera/Guess_o_Matic_v00/', dataFile, sep = '' )
  download.file( theURL, destfile = dataFile, method = "curl" )
  unzip( dataFile )
}

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

SentenceFinder <- function( inputArray ){
  gsub( "\\s\\s*", " ",
        unlist(
          strsplit(
            tolower( inputArray ),
            "\\s*[.?!]+\\s*", perl = TRUE)
        ),
        perl = TRUE )
}

HandlePunctuation <- function( workArray,
                               specRegex =       c( "(\\w)'s\\b", "^i'm\\b", "(\\s)i'm\\b", "(\\w)'d\\b", "(\\w)n't\\b", "(\\w)'re\\b", "(\\w)'ll\\b", "(\\w)'ve\\b" ),
                               specLit   =       c( "\\1SS",      "IM",      "\\1IM",       "\\1DD",      "\\1NT",       "\\1RE",       "\\1LL",       "\\1VE"),
                               specSeen  =       c( "SS",         "IM",      "IM",          "DD",         "NT",          "RE",          "LL",          "VE"   ),
                               specFinal =       c( "'s",         "i'm",     "i'm",         "'d",         "n't",         "'re",         "'ll",         "'ve")
){

  # Check parms
  if(( length( specRegex ) != length( specLit )) ||
       ( length( specRegex ) != length( specSeen )) ||
       ( length( specRegex ) != length( specFinal ))){
    stop( "Pattern array length mismatch in HandlePunctuation" )
  }

  # Convert funny apostrophes
  workArray <- gsub( "’", "'", workArray, fixed = TRUE )

  # Add apostrophe protection
  for( iPat in 1:length( specRegex )){
    workArray <- gsub( specRegex[iPat], specLit[iPat], workArray, perl = TRUE )
  }

  # All other nonletters
  workArray <- gsub( "[^abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789' ]", " ", workArray, perl = TRUE )

  # Remove isolated possessives
  workArray <- gsub( " SS ", " ", workArray, perl = TRUE )
  workArray <- gsub( "^SS ", "", workArray, perl = TRUE )
  workArray <- gsub( " SS$", " ", workArray, perl = TRUE )

  # Remove all other apostrophes
  workArray <- gsub( "'", "", workArray, fixed = TRUE )

  # And put protected apostrophes back
  for( iPat in 1:length( specRegex )){
    workArray <- gsub( specSeen[iPat], specFinal[iPat], workArray, fixed = TRUE )
  }

  # Any unwanted spaces that have crept back in
  workArray <- gsub( "\\s+", " ", workArray, perl = TRUE )
  workArray <- gsub( "^ | $", "", workArray, perl = TRUE )

  # And do it!
workArray
}

OnlineTokener <- function( rawSentence, verbose = FALSE ){

  sentence <- tolower( rawSentence )

  if( verbose ) message ( " ... punctuation")
  sentence <- HandlePunctuation( sentence )

  if( verbose ) message ( " ... raw tokens")
  tokens <- strsplit( sentence, ' +', perl = TRUE )[[1]]

  if( verbose ) message ( " ... profanities")
  tokens <- ReplacePatterns( tokens, profanityPatterns, 'PROFANITY' )

  if( verbose ) message ( " ... boys & possessives")
  tokens <- ifelse( tokens %in% malenames,   'MALENAME', tokens )
  tokens <- ifelse( tokens %in% maleposs,    'MALEPOSS', tokens )

  if( verbose ) message ( " ... girls & possessives")
  tokens <- ifelse( tokens %in% femalenames, 'FEMALENAME', tokens )
  tokens <- ifelse( tokens %in% femaleposs,  'FEMALEPOSS', tokens )

  if( verbose ) message ( " ... lastnames")
  tokens <- ifelse( tokens %in% lastnames,   'LASTNAME', tokens )

  if( verbose ) message ( " ... US states")
  tokens <- ifelse( tokens %in% states,      'STATE', tokens )

  if( verbose ) message ( " ... countries")
  tokens <- ifelse( tokens %in% countries,   'COUNTRY', tokens )

  if( verbose ) message ( " ... US cities")
  tokens <- ifelse( tokens %in% uscities,    'USCITY', tokens )

  if( verbose ) message ( " ... World cities")
  tokens <- ifelse( tokens %in% worldcities, 'WORLDCITY', tokens )

  if( verbose ) message(" ... some non-words" )
  tokens <- ifelse( nchar(tokens)>20, 'LONGWORD', tokens )
  tokens <- gsub( '^.*(.)\\1\\1.*$', 'NONWORD', tokens, perl = TRUE )
  tokens <- gsub( '^.*([hjkquwxy])\\1.*$', 'NONWORD', tokens, perl = TRUE )
  tokens <- gsub( '^.*tokens[^aeioty].*$', 'NONWORD', tokens, perl = TRUE )
  tokens <- gsub( '^[bcdefghjklmnpqrstvwxyz]$', 'LETTER', tokens, perl = TRUE )
  tokens <- gsub( '^[0123456789]$', 'DIGIT', tokens, perl = TRUE )

  if( verbose ) message(" ... some alphanumerics" )
  tokens <- gsub( '^[0123456789]+$', 'NUMBER', tokens, perl = TRUE )
  tokens <- gsub( '^[abcdefghijklmnopqrstuvwxyz]+[0123456789].*$', 'ALPHANUM', tokens, perl = TRUE )
  tokens <- gsub( '^[0123456789]+[abcdefghijklmnopqrstuvwxyz].*$', 'ALPHANUM', tokens, perl = TRUE )
  tokens <- gsub( "^[0123456789]+\'[sd]$", 'ALPHANUM', tokens, perl = TRUE )

  tokens <- unlist( lapply( list( tokens ), function(x){ ifelse( x %in% knownWords, x, 'HAPAX')} ))

  tokens <- c( "START", tokens )

tokens
}

ReplacePatterns <- function( someWords, targetPatterns, replacementString  ){
  someWords[ grepl( targetPatterns, someWords ) ] <- replacementString
someWords
}

BinMatch <- function( x, charVec ){
  if( x < charVec[1] || x > charVec[length(charVec)]) return( NA )
  ilo <- 1
  ihi <- length( charVec )
  while( ( ihi - ilo ) > 1 ){
    imid <- floor( (ihi + ilo)/2 )
    ymid <- charVec[ imid ]
    if( ymid > x ) ihi <- imid
    if( ymid < x ) ilo <- imid
    if( ymid == x ) return( imid )
  }
  if( charVec[ilo] == x ) return( ilo )
  if( charVec[ihi] == x ) return( ihi )
NA
}

HitFrame <- function( tokens ){

  nHit <- 0
  resultList <- list()
  for( nTok in 1:(min(5,length(tokens)))){
    lastFew <- tokens[(length(tokens)+1-nTok):(length(tokens))]
    possPrev <- paste0( lastFew, collapse = ":" )
    iMatch <- BinMatch( possPrev, keyFrameSorted$previous )
    if( is.na(iMatch)) next
    start <- keyFrameSorted$first[iMatch]
    end <- keyFrameSorted$last[iMatch]
    nHit <- nHit + end - start
    nextWords <- nextTok[start:end]
    nextFreq <- freq[start:end]
    resultList[[nTok]] <- data.frame(
      nPrev = rep( nTok, end - start + 1 ),
      nextWord = factor( nextWords ),
      freq = nextFreq
    )
  }
  if( nHit == 0 ) return(NA)
do.call( rbind, resultList )
}


NextScores <- function( hitFrame, weights, howMany = 10 ){
  hitFrame$score <- weights[ hitFrame$nPrev ] * hitFrame$freq
  wordScore <- data.frame( summarize( group_by( hitFrame, nextWord ), sum( score )))
  colnames( wordScore ) <- c( "nextWord", "score" )
  theOrder <- order( wordScore$score, decreasing = TRUE )
  wordScore <- wordScore[theOrder[1:howMany],]
  wordScore$nextWord <- as.character( wordScore$nextWord )
wordScore
}

PredictNext <- function( tokens,
                         weights = c( 0.0001, 0.001, 0.01, 0.1, 1 ),
                         fallbackWord = "and"
)
{
  hitFrame <- HitFrame( tokens )
  if( ! is.data.frame( hitFrame )) return( fallbackWord )
  topWordFrame <- NextScores( hitFrame, weights, howMany = 1 )
topWordFrame$nextWord
}

NormalizeWeights <- function(x) x / sum(x)
