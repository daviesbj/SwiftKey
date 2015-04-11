library( plyr )


ReplaceHapax <- function( wordVec, hapaxVec, newToken = 'HAPAX' ){
  wordVec[ wordVec %in% hapaxVec ] <- newToken
wordVec
}

ReplaceHapaxInv <- function( wordVec, nonHapaxVec, newToken = 'HAPAX' ){
  wordVec[ ! ( wordVec %in% nonHapaxVec ) ] <- newToken
  wordVec
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

  colnames( ngramFrame ) <- gsub( "^X", "token", colnames( ngramFrame ), perl = TRUE )

ngramFrame
}



RearrangeNgramFrame <- function( ngramFrame, verbose = TRUE ){
  if( verbose ) message ( "Rearranging ... " )
  leftColumns <- list()
  nLeft <- dim( ngramFrame )[2] - 2
  for( iCol in 1:nLeft ){
    leftColumns[[iCol]] <- ngramFrame[,iCol]
  }
  previous <- do.call( paste, leftColumns )
  previous <- gsub( " ", ":", previous, fixed = TRUE )
  newFrame <- data.frame(
    previous = factor( previous ),
    nextTok = factor( ngramFrame[,nLeft+1]),
    freq = ngramFrame$freq
  )
  if( verbose ) message ( "Sorting ... ")
  newFrame <- newFrame[order( newFrame$previous ),]
newFrame
}


UniqueNgramFrame <- function( ngramPrevFrame, verbose = TRUE ){
  if( verbose ) message( "Filtering ... " )
  ngramPrevWords <- subset( ngramPrevFrame, (
    ! grepl( "^[ABCDEFGHIJKLMNOPQRSTUVWXYZ]", nextTok, perl = TRUE ))
  )
  if( verbose ) message( "Sorting ... " )
  ngramPrevWordsOrder <- order(
    as.character(ngramPrevWords$previous),
    -ngramPrevWords$freq
  )
  ngramPrevWords <- ngramPrevWords[ngramPrevWordsOrder,]

  if( verbose ) message( "Analyzing ... ")
  breaks <- Breaks( ngramPrevWords$previous )

  if( verbose ) message( "Subsetting ... ")
  biggestNgram <- ngramPrevWords[breaks,]
  biggestNgram <- biggestNgram[order(biggestNgram$freq, decreasing=TRUE),]
  biggestNgram$previous <- as.character( biggestNgram$previous )
  biggestNgram <- subset( biggestNgram, freq > 1 )
  biggestNgram$freq <- NULL

  biggestNgram
}

Breaks <- function( charVec ){
  nTok <- length( charVec )
  offset1 <- charVec[-1]
  offset2 <- charVec[-nTok]
  breaks <- c(1, 1+which(offset1 != offset2))
  breaks
}

