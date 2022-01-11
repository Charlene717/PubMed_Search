# # https://rdrr.io/cran/superml/src/R/bm25.R
# view(bind_tf_idf)

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

# ##### Test seq #####
#   BM25 <- seq(1:50)*0.01
#   set.seed(1)
#   W2V <- sample(1:50)*0.01

##### (FUN) BM25 combine W2V Score #####
  BM25_W2V_Score = function(tbl, BM25, W2V, document, mode=1, b = 0.75){
    
      BM25 <- quo_name(enquo(BM25))
      W2V <- quo_name(enquo(W2V))
      document <- quo_name(enquo(document))
      
      BM25s <-  as.numeric(tbl[[document]])
      W2Vs  <-  as.numeric(tbl[[document]])
      documents <- as.character(tbl[[document]]) 
      
      
      ## https://rdrr.io/cran/superml/src/R/bm25.R
      ## Not suitable
      # doc_len <- length(documents)
      # mean_doc_len <- mean(vapply(documents, length, FUN.VALUE = integer(1)))
      
      ## Count doc_len & mean_doc_len
      tbl %>% group_by(book) %>% mutate(.,Ld=sum(n)) -> doc_len
      #doc_len <- doc_len[!duplicated(doc_len[,c('book')]),]
      #book_BM25 %>% group_by(book) %>% mutate(.,Ld=sum(n)) -> doc_len
      doc_len <- doc_len$Ld
      mean_doc_len <- sum(tbl$n)/length(doc_totals)
      
      
      
      if (mode==1){
      Score <- sum(BM25s*W2Vs)/(1 - b + b *(doc_len / mean_doc_len))
      }else{
      Score <- sum(BM25s+W2Vs)/(1 - b + b *(doc_len / mean_doc_len))  
      }
      tbl$Score <- Score
      return(tbl)    
    }

# ##### Try #####
  BM25_W2V_Score.df <- BM25_W2V_Score(book_BM25, tf_idf, bm25, book)










