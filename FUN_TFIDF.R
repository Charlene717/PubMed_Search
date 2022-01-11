## bind_tf_idf {tidytext}
TF_IDF = function (tbl, term, document, n , mode = "Basic")
{
  term <- quo_name(enquo(term))
  document <- quo_name(enquo(document))
  n_col <- quo_name(enquo(n))
  terms <- as.character(tbl[[term]])
  documents <- as.character(tbl[[document]])
  n <- tbl[[n_col]]
  doc_totals <- tapply(n, documents, sum)
  
  if(mode == "Basic"){
      idf <- log(length(doc_totals)/table(terms))
      tbl$tf <- n/as.numeric(doc_totals[documents])
      tbl$idf <- as.numeric(idf[terms])
      
  }else if(mode == "A"){
      idf <- log(length(doc_totals)/table(terms))
      tbl$tf <- n/as.numeric(doc_totals[documents])
      
      tbl[tbl$tf > 0,  ]$tf <- 1
      if(nrow(tbl[tbl$tf <= 0, ])>0){
      tbl[tbl$tf <= 0, ]$tf <- 0
      }
      
      tbl$idf <- as.numeric(idf[terms])
    
  }else if(mode == "B"){
      idf <- log(length(doc_totals)/table(terms))
      tbl$tf <- n/as.numeric(doc_totals[documents])
      maxtf <- max(tbl$tf)
      tbl$tf <- 0.5 + 0.5*tbl$tf/maxtf
      tbl$idf <- as.numeric(idf[terms])
      
  }else if(mode == "C"){
    idf <- log((length(doc_totals)-table(terms))/table(terms))
    tbl$tf <- n/as.numeric(doc_totals[documents])
    
    tbl[tbl$tf > 0,  ]$tf <- 1
    if(nrow(tbl[tbl$tf <= 0, ])>0){
      tbl[tbl$tf <= 0, ]$tf <- 0
    }
    tbl$idf <- as.numeric(idf[terms])
    if(nrow(tbl[tbl$idf  <= 0, ])>0){
    tbl[tbl$idf <= 0, ]$idf <- 0
    }
  
  }else if(mode == "D"){
    idf <- log((length(doc_totals)-table(terms))/table(terms))
    tbl$tf <- n/as.numeric(doc_totals[documents])
    maxtf <- max(tbl$tf)
    tbl$tf <- 0.5 + 0.5*tbl$tf/maxtf
    tbl$idf <- as.numeric(idf[terms])
    if(nrow(tbl[tbl$idf  <= 0, ])>0){
    tbl[tbl$idf <= 0, ]$idf <- 0
    # ##
    # tbl$idf[tbl$idf<0] <- 0
    # ##
    }
    
  }
  
  tbl$tf_idf <- tbl$tf * tbl$idf
  if (any(tbl$idf < 0, na.rm = TRUE)) {
    rlang::warn(paste("A value for tf_idf is negative:\n", 
                      "Input should have exactly one row per document-term combination."))
  }
  tbl
  
  return(tbl)
}
