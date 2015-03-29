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

