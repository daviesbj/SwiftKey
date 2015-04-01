TabulateBigrams <- function( sentenceList,
                             firstOne = 1,
                             howMany = length( sentenceList ) - firstOne + 1,
                             verbose = TRUE ){
  biGramList <- hash()
  nTok = 0;
  for ( iSentence in 1:howMany ){
    if( ( verbose && ( iSentence %% floor( howMany / 100 ) ) == 0 )){
      message( "Done ", iSentence, "/", howMany, " sentences -- tokens: ", nTok,
               " biGrams: ", length( biGramList) )
    }
    sentence <- sentenceList[[iSentence + firstOne - 1]]
    nTok <- nTok + length( sentence )
    twoWords <- c( NA, 'START' )
    innerList <- list()
    nBigram <- 0
    for( nextWord in c( sentence, 'END' ) ){
      nBigram <- nBigram + 1
      twoWords <- c( twoWords[2], nextWord )
      biGram <- paste( twoWords, collapse = ':' )
      if( is.null( biGramList[[biGram]])){
        biGramList[[biGram]] <- 1
      } else {
        biGramList[[biGram]] <- biGramList[[biGram]] + 1
      }
    }
  }
biGramList
}

TabulateTrigrams <- function( sentenceList,
                             firstOne = 1,
                             howMany = length( sentenceList ) - firstOne + 1,
                             verbose = TRUE ){
  triGramList <- hash()
  nTok = 0;
  for ( iSentence in 1:howMany ){
    if( ( verbose && ( iSentence %% floor( howMany / 100 ) ) == 0 )){
      message( "Done ", iSentence, "/", howMany, " sentences -- tokens: ", nTok,
               " triGrams: ", length( triGramList) )
    }
    sentence <- sentenceList[[iSentence + firstOne - 1]]
    nTok <- nTok + length( sentence )
    threeWords <- c( NA, 'START', sentence[1] )
    xSentence <- sentence[2:length( sentence)]
    innerList <- list()
    for( nextWord in c( xSentence, 'END' ) ){
      threeWords <- c( threeWords[2:3], nextWord )
      triGram <- paste( threeWords, collapse = ':' )
      if( is.null( triGramList[[triGram]])){
        triGramList[[triGram]] <- 1
      } else {
        triGramList[[triGram]] <- triGramList[[triGram]] + 1
      }
    }
  }
triGramList
}

ReplaceHapax <- function( wordVec, hapaxVec, newToken = 'HAPAX' ){
  wordVec[ wordVec %in% hapaxVec ] <- newToken
wordVec
}

ReplaceHapaxInv <- function( wordVec, nonHapaxVec, newToken = 'HAPAX' ){
  wordVec[ ! ( wordVec %in% nonHapaxVec ) ] <- newToken
  wordVec
}

BigramTable <- function( sentenceList,
                         firstOne = 1,
                         howMany = length( sentenceList ) - firstOne + 1,
                         verbose = TRUE ){

  bigramVecList <- list()
  nTok <- 0

# Loop over sentences
  for ( iSentence in 1:howMany ){

    if( ( verbose && ( iSentence %% floor( howMany / 100 ) ) == 0 )){
      message( "Sentences: ", iSentence, "/", howMany, " Tokens: ", nTok )
    }

    sentence <- sentenceList[[iSentence + firstOne - 1]]
    nTok <- nTok + length( sentence )

    bigramVec <- paste( c( "START", sentence ), c( sentence, "END" ), sep = ":" )
    bigramVecList[[iSentence]] <- bigramVec

  }

  if( verbose ) message( "Unlisting ... " )
  bigBigramVec <- unlist( bigramVecList )

  if( verbose ) message( "Tabulating ... " )
  bigramTable <- table( bigBigramVec )

bigramTable
}

TrigramTable <- function( sentenceList,
                         firstOne = 1,
                         howMany = length( sentenceList ) - firstOne + 1,
                         verbose = TRUE ){

  typicalVec <- paste0( rep( '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ', 10 ), sep = " " )
  trigramVecList <- list( rep( typicalVec, howMany ))
  nTok <- 0

  # Loop over sentences
  for ( iSentence in 1:howMany ){

    if( ( verbose && ( iSentence %% floor( howMany / 100 ) ) == 0 )){
      message( "Sentences: ", iSentence, "/", howMany, " Tokens: ", nTok )
    }

    sentence <- sentenceList[[iSentence + firstOne - 1]]
    nSent <- length( sentence )
    nTok <- nTok + nSent

    trigramVec <- paste( c( "START", sentence[1:nSent-1] ),
                         sentence,
                         c( sentence[2:nSent], "END" ), sep = ":" )
    trigramVecList[[iSentence]] <- trigramVec

  }

  if( verbose ) message( "Unlisting ... " )
  bigTrigramVec <- unlist( trigramVecList )

  if( verbose ) message( "Tabulating ... " )
  trigramTable <- table( bigTrigramVec )

  trigramTable
}



FourgramTable <- function( sentenceList,
                           firstOne = 1,
                           howMany = length( sentenceList ) - firstOne + 1,
                           verbose = TRUE ){

  fourgramVecList <- list()
  nTok <- 0

  # Loop over sentences
  for ( iSentence in 1:howMany ){

    if( ( verbose && ( iSentence %% floor( howMany / 100 ) ) == 0 )){
      message( "Sentences: ", iSentence, "/", howMany, " Tokens: ", nTok )
    }

    sentence <- c( "START", sentenceList[[iSentence + firstOne - 1]], "END" )
    nSent <- length( sentence )
    nTok <- nTok + nSent
    if( nSent < 4 ) next

    fourgramVec <- paste( sentence[1:(nSent-3)],
                          sentence[2:(nSent-2)],
                          sentence[3:(nSent-1)],
                          sentence[4:nSent],
                          sep = ":" )
    fourgramVecList[[iSentence]] <- fourgramVec

  }

  if( verbose ) message( "Unlisting ... " )
  bigfourgramVec <- unlist( fourgramVecList )

  if( verbose ) message( "Tabulating ... " )
  fourgramTable <- table( bigfourgramVec )

  fourgramTable
}


NgramFrame <- function( sentenceList,
                        n = 2,
                        firstOne = 1,
                        howMany = length( sentenceList ) - firstOne + 1,
                        verbose = TRUE ){

  if( n < 2 ) stop( "Can't do less than a bigram" )
  ngramVecList <- list()
  nTok <- 0

  # Loop over sentences
  for ( iSentence in 1:howMany ){

    if( ( verbose && ( iSentence %% floor( howMany / 100 ) ) == 0 )){
      message( "Sentences: ", iSentence, "/", howMany, " Tokens: ", nTok )
    }

    sentence <- c( "START", sentenceList[[iSentence + firstOne - 1]], "END" )
    nSent <- length( sentence )
    nTok <- nTok + nSent
    if( nSent < n ) next
    argList <- list()
    for( iOffset in 1:n ) argList[[iOffset]] <- sentence[iOffset:(nSent-n+iOffset)]

    ngramVec <- do.call( paste, argList )
    ngramVec <- gsub( " ", ":", ngramVec, fixed = TRUE )
    ngramVecList[[iSentence]] <- ngramVec

  }

  if( verbose ) message( "Unlisting ... " )
  bigngramVec <- unlist( ngramVecList )
  rm( ngramVecList )

  if( verbose ) message( "Tabulating ... " )
  ngramTable <- table( bigngramVec )
  rm( bigngramVec )

  if( verbose ) message( "Sorting ... ")
  ngramTable <- ngramTable[order( ngramTable, decreasing = TRUE )]
  ngrams <- names( ngramTable )
  nNgram <- length( ngrams )
  freq <- unname( ngramTable )
  rm( ngramTable )

  if( verbose ) message( "Rearranging ..." )
  ngramTokList <- strsplit( ngrams, ':' )
  rm( ngrams )
  ngramTokVec <- unlist( ngramTokList )
  rm( ngramTokList )
  dim( ngramTokVec ) <- c( n, nNgram )
  ngramTokVec <- t( ngramTokVec )
  ngramFrame <- data.frame( ngramTokVec, stringsAsFactors = TRUE )
  rm( ngramTokVec )
  ngramFrame$freq <- freq
  rm( freq )

ngramFrame
}
