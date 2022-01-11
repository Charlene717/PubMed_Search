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
  BM25Score = function(tbl, term, document, n, 
                       b = 0.75, k1 = 1.25, sigmoid=1){
    ## Environment
    require(dplyr)
    require(magrittr)
    require(tidyverse)
    
    ##
    term <- quo_name(enquo(term))
    document <- quo_name(enquo(document))
    n_col <- quo_name(enquo(n))
    terms <- as.character(tbl[[term]])
    documents <- as.character(tbl[[document]])

    n <- tbl[[n_col]]
    doc_totals <- tapply(n, documents, sum)
    
    ## https://rdrr.io/cran/superml/src/R/bm25.R
    ## Not suitable
    # doc_len <- length(documents)
    # mean_doc_len <- mean(vapply(documents, length, FUN.VALUE = integer(1)))
    
    ## Count doc_len & mean_doc_len
    tbl %>% group_by(book) %>% mutate(.,Ld=sum(n)) -> doc_len
    #doc_len <- doc_len[!duplicated(doc_len[,c('book')]),]
    #book_BM25 %>% group_by(book) %>% mutate(.,Ld=sum(n)) -> doc_len
    doc_len <- doc_len$Ld
    names(doc_len) <- terms
    mean_doc_len <- sum(tbl$n)/length(doc_totals)
    
    ## idf
    idf <- log(length(doc_totals)/table(terms))
    idf_BM25 <- log((length(doc_totals)-table(terms)+0.5)/(table(terms)+0.5))
    
    ## write export table
    tbl$tf <- n/as.numeric(doc_totals[documents])
    tbl$idf <- as.numeric(idf[terms])
    tbl$idf_BM25 <- as.numeric(idf_BM25[terms])
    tbl$tf_idf <- tbl$tf * tbl$idf
    tbl$doc_len <- as.numeric(doc_len[terms])
    tbl$bm25 <- (k1 + 1)*(tbl$tf) * tbl$idf_BM25/
      (k1*(1 - b + b *(tbl$doc_len / mean_doc_len))+ tbl$tf)
    
    ## Apply sigmoid
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
  
# # ##### Try #####
#   book_BM25 <- book_words %>%
#     BM25Score(word, book, n)
# 
#   plot(book_BM25$tf_idf ,book_BM25$bm25)
