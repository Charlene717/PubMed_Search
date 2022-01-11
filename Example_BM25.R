## BM25
# https://www.tidytextmining.com/tfidf.html
  rm(list = ls()) # Clean variable
  memory.limit(150000)

##### 3.1 Term frequency in Jane Austen s novels #####
  library(dplyr)
  library(janeaustenr)
  library(tidytext)
  
  book_words <- austen_books() %>%
    unnest_tokens(word, text) %>%
    count(book, word, sort = TRUE)
  
  total_words <- book_words %>% 
    group_by(book) %>% 
    summarise(total = sum(n))  # https://github.com/tidyverse/dplyr/issues/505
  
  book_words <- left_join(book_words, total_words)
  
  book_words
  
  
  library(ggplot2)
  
  ggplot(book_words, aes(n/total, fill = book)) +
    geom_histogram(show.legend = FALSE) +
    xlim(NA, 0.0009) +
    facet_wrap(~book, ncol = 2, scales = "free_y")

##### 3.2 Zipf s law #####
  freq_by_rank <- book_words %>% 
    group_by(book) %>% 
    mutate(rank = row_number(), 
           `term frequency` = n/total) %>%
    ungroup()
  
  freq_by_rank
  
  freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`, color = book)) + 
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()
  
  
  rank_subset <- freq_by_rank %>% 
    filter(rank < 500,
           rank > 10)
  
  lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
  
  
  freq_by_rank %>% 
    ggplot(aes(rank, `term frequency`, color = book)) + 
    geom_abline(intercept = -0.62, slope = -1.1, 
                color = "gray50", linetype = 2) +
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()


##### 3.3 The bind_tf_idf() function #####
  book_tf_idf <- book_words %>%
    bind_tf_idf(word, book, n)
  
  book_tf_idf
  
  book_tf_idf %>%
    select(-total) %>%
    arrange(desc(tf_idf))
  
  
  library(forcats)
  
  book_tf_idf %>%
    group_by(book) %>%
    slice_max(tf_idf, n = 15) %>%
    ungroup() %>%
    ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~book, ncol = 2, scales = "free") +
    labs(x = "tf-idf", y = NULL)

##### 3.3 The BM25() function #####
  # # https://rdrr.io/cran/superml/src/R/bm25.R
  # bmscore = function(q, document_from_corpus, b = 0.75,k1 = 1.25){
  #   
  #   # # constant values
  #   # b <- 0.75
  #   # k1 <- 1.25
  #   
  #   freq_q <- sum(document == q)
  #   doc_len <- length(document)
  #   mean_doc_len <- mean(vapply(document, length, FUN.VALUE = integer(1)))
  #   return(#private$calculate_idf(q, corpus) *
  #            ((freq_q * (k1 + 1)) /
  #               ((freq_q + k1) *
  #                  (1 - b + b *
  #                     (doc_len / mean_doc_len)))))
  # }
  
  view(bind_tf_idf)
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
  
  
  book_BM25 <- book_words %>%
               BM25Score(word, book, n)

  plot(book_BM25_sig$tf_idf ,book_BM25_sig$bm25)
  
  # ## Z-score
  # # https://www.r-bloggers.com/2020/02/how-to-compute-the-z-score-with-r/
  # book_BM25_zscore <- book_BM25
  # 
  # book_BM25_zscore <- book_BM25 %>% 
  #                     mutate(BM25_zscore = (book_BM25$bm25 - 
  #                     mean(book_BM25$bm25))/sd(book_BM25$bm25))
  # # book_BM25_zscore_Pos <- book_BM25_zscore %>% mutate(BM25_zscore_Pos =
  # #                                              (book_BM25_zscore$BM25_zscore-min(book_BM25_zscore$BM25_zscore)))         

##### 3.4 A corpus of physics texts #####
  library(gutenbergr)
  physics <- gutenberg_download(c(37729, 14725, 13476, 30155), 
                                meta_fields = "author")
  
  physics_words <- physics %>%
    unnest_tokens(word, text) %>%
    count(author, word, sort = TRUE)
  
  physics_words
  
  
  plot_physics <- physics_words %>%
    BM25Score(word, author, n) %>%
    mutate(author = factor(author, levels = c("Galilei, Galileo",
                                              "Huygens, Christiaan", 
                                              "Tesla, Nikola",
                                              "Einstein, Albert")))
  
  plot_physics %>% 
    group_by(author) %>% 
    slice_max(bm25, n = 15) %>% 
    ungroup() %>%
    mutate(word = reorder(word, bm25)) %>%
    ggplot(aes(bm25, word, fill = author)) +
    geom_col(show.legend = FALSE) +
    labs(x = "BM25", y = NULL) +
    facet_wrap(~author, ncol = 2, scales = "free")
  
## Remove Stop Word
  
  library(stringr)
  
  physics %>% 
    filter(str_detect(text, "_k_")) %>% 
    select(text)
  
  physics %>% 
    filter(str_detect(text, "RC")) %>% 
    select(text)
  
  
  
  mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                                 "fig", "file", "cg", "cb", "cm",
                                 "ab", "_k", "_k_", "_x"))
  
  physics_words <- anti_join(physics_words, mystopwords, 
                             by = "word")
  
  plot_physics <- physics_words %>%
    BM25Score(word, author, n) %>%
    mutate(word = str_remove_all(word, "_")) %>%
    group_by(author) %>% 
    slice_max(bm25, n = 15) %>%
    ungroup() %>%
    mutate(word = reorder_within(word, bm25, author)) %>%
    mutate(author = factor(author, levels = c("Galilei, Galileo",
                                              "Huygens, Christiaan",
                                              "Tesla, Nikola",
                                              "Einstein, Albert")))
  
  ggplot(plot_physics, aes(word, bm25, fill = author)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "BM25") +
    facet_wrap(~author, ncol = 2, scales = "free") +
    coord_flip() +
    scale_x_reordered()
  