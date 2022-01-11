# # https://rdrr.io/cran/superml/src/R/bm25.R
# view(bind_tf_idf)

BM25Score = function(tbl, term, document, n, 
                     b = 0.75, k1 = 1.25, sigmoid=1){
  
  term <- quo_name(enquo(term))
  document <- quo_name(enquo(document))
  n_col <- quo_name(enquo(n))
  terms <- as.character(tbl[[term]])
  documents <- as.character(tbl[[document]])
  
  doc_len <- length(documents)
  mean_doc_len <- mean(vapply(documents, length, FUN.VALUE = integer(1)))
  
  n <- tbl[[n_col]]
  doc_totals <- tapply(n, documents, sum)
  idf <- log(length(doc_totals)/table(terms))
  idf_BM25 <- log((length(doc_totals)-table(terms)+0.5)/(table(terms)+0.5))
  tbl$tf <- n/as.numeric(doc_totals[documents])
  tbl$idf <- as.numeric(idf[terms])
  tbl$idf_BM25 <- as.numeric(idf_BM25[terms])
  tbl$tf_idf <- tbl$tf * tbl$idf
  tbl$bm25 <- (k1 + 1)*(tbl$tf) * tbl$idf_BM25/
    (k1*(1 - b + b *(doc_len / mean_doc_len))+ tbl$tf)
  if (sigmoid==1){
    ## Sigmoid
    # https://cran.r-project.org/web/packages/sigmoid/vignettes/sigmoid.html
    library(sigmoid)
    book_BM25_sig <- tbl
    book_BM25_sig$bm25 <- sigmoid(tbl$bm25,method = c("ReLU"))
    tbl <- book_BM25_sig
  }
  if (any(tbl$idf < 0, na.rm = TRUE)) {
    rlang::warn(paste("A value for tf_idf is negative:\n", 
                      "Input should have exactly one row per document-term combination."))
  }
  return(tbl)    
}


# book_BM25 <- book_words %>%
#   BM25Score(word, book, n)
# 
# plot(book_BM25_sig$tf_idf ,book_BM25_sig$bm25)
