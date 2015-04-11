library( dplyr )

NextTokFrame <- function( nGram ){
  ngramComponents <- strsplit( nGram, ':' )[[1]]
  nComp <- length( ngramComponents )
  shortList <- list()
  for( i in 1:nComp ){
    shorterGram <- paste0( ngramComponents[(nComp+1-i):nComp], collapse = ":" )
    shortList[[i]] <- shorterGram
  }
  targets <- unlist( shortList )
  matches <- match( targets, uniqueNgrams )
  starts <- allgramBreaks[matches]
  ends <- allgramBreaks[matches+1]-1
  frameList <- list()
  for( i in 1:nComp ){
    if( is.na(starts[i])) next
    nextTokPartial <- as.character( nextTok[starts[i]:ends[i]] )
    freqPartial <- freq[starts[i]:ends[i]]
    ngramSize <- rep( i, ends[i] - starts[i] + 1 )
    frameList[[i]] <- data.frame(
      size = ngramSize,
      nextTok = factor(nextTokPartial),
      freq = freqPartial
    )
  }
  resultFrame <- do.call( rbind, frameList )
resultFrame
}

ScoredNext <- function( nGram, weights ){
  candFrame <- NextTokFrame( nGram )

  candFrame$weight <- weights[ candFrame$size ]
  candFrame$score <- candFrame$freq * candFrame$weight
  scoreFrame <- summarise(
    group_by( candFrame, nextTok ), score = sum( score )
  )
  scoreOrder <- order( scoreFrame$score, decreasing = TRUE )
  scoreFrame <- scoreFrame[ scoreOrder, ]
scoreFrame
}
