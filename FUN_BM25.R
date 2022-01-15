# View(bind_tf_idf)

##### version info #####
  # platform       x86_64-w64-mingw32          
  # arch           x86_64                      
  # os             mingw32                     
  # system         x86_64, mingw32             
  # status                                     
  # major          4                           
  # minor          1.1                         
  # year           2021                        
  # month          08                          
  # day            10                          
  # svn rev        80725                       
  # language       R                           
  # version.string R version 4.1.1 (2021-08-10)
  # nickname       Kick Things

##### (FUN) BM25Score #####
  BM25Score = function(tbl, term, document, n, Ld,
                       b = 0.75, k1 = 1.25){
    ## Environment
    require(dplyr)
    require(magrittr)
    require(tidyverse)
    
    ##
    term <- quo_name(enquo(term))
    document <- quo_name(enquo(document))
    n_col <- quo_name(enquo(n))
    Ld <- quo_name(enquo(Ld))
    terms <- as.character(tbl[[term]])
    documents <- as.character(tbl[[document]])
    n <- tbl[[n_col]]
    Lds <- tbl[[Ld]]
    
    doc_totals <- tapply(n, documents, sum)
    
    ## Count mean_Lds
    mean_Lds <- sum(doc_totals)/length(doc_totals)
    
    ## idf
    idf <- log(length(doc_totals)/table(terms))
    idf_BM25 <- log10((length(doc_totals)-table(terms)+0.5)/(table(terms)+0.5))
    
    ## write export table
    tbl$tf <- n/as.numeric(doc_totals[documents])
    tbl$idf <- as.numeric(idf[terms])
    tbl$tf_idf <- tbl$tf * tbl$idf
    tbl$idf_BM25 <- as.numeric(idf_BM25[terms])
    tbl$tf_bm25 <- (k1 + 1)*(tbl$tf) /
      (k1*(1 - b + b *(as.numeric(Lds) / mean_Lds)) + tbl$tf)
    tbl$bm25 <- tbl$tf_bm25 * tbl$idf_BM25
  
    if (any(tbl$idf < 0, na.rm = TRUE)) {
      rlang::warn(paste("A value for tf_idf is negative:\n", 
                        "Input should have exactly one row per document-term combination."))
    }
    return(tbl)    
  }

# # ##### Try #####
  #   book_BM25 <- book_words %>%
  #     BM25Score(word, book, n, total)
  # 
  #   plot(book_BM25$tf_idf ,book_BM25$bm25)