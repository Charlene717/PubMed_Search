## BM25
# https://www.tidytextmining.com/tfidf.html
  rm(list = ls()) # Clean variable
  memory.limit(150000)
  
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
  
##### 3.1 Term frequency in Jane Austen s novels #####
  library(dplyr)
  library(janeaustenr)
  library(tidytext)
  
  book_words.df <- austen_books() %>%
    unnest_tokens(word, text) %>%
    count(book, word, sort = TRUE)
  
  total_words.df <- book_words.df %>% 
    group_by(book) %>% 
    summarise(total = sum(n))  # https://github.com/tidyverse/dplyr/issues/505
  
  book_words.df <- left_join(book_words.df, total_words.df)
  
  book_words.df
  
  
  library(ggplot2)
  
  ggplot(book_words.df, aes(n/total, fill = book)) +
    geom_histogram(show.legend = FALSE) +
    xlim(NA, 0.0009) +
    facet_wrap(~book, ncol = 2, scales = "free_y")

##### 3.2 Zipf s law #####
  freq_by_rank.df <- book_words.df %>% 
    group_by(book) %>% 
    mutate(rank = row_number(), 
           `term frequency` = n/total) %>%
    ungroup()
  
  freq_by_rank.df
  
  freq_by_rank.df %>% 
    ggplot(aes(rank, `term frequency`, color = book)) + 
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()
  
  
  rank_subset <- freq_by_rank.df %>% 
    filter(rank < 500,
           rank > 10)
  
  lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
  
  
  freq_by_rank.df %>% 
    ggplot(aes(rank, `term frequency`, color = book)) + 
    geom_abline(intercept = -0.62, slope = -1.1, 
                color = "gray50", linetype = 2) +
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()

##### 3.3 The BM25() function #####
  source("FUN_BM25.R")
  book_BM25.df <- book_words.df %>%
                  BM25Score(word, book, n)
  plot(book_BM25.df$tf_idf ,book_BM25.df$bm25)
  
  ## Check
  doc_totals <- data.frame(Ld = tapply(book_BM25.df$n, book_BM25.df$book, sum))
  book_BM25_Check1 <- book_BM25.df[1,]
  WordCount.df <- as.data.frame(table(book_BM25.df$word))
  WordCount_W.df <- WordCount.df[WordCount.df$Var1 == book_BM25_Check1$word,]
  
  N <- nrow(doc_totals)
  dft <- WordCount_W.df$Freq
  k=1.25
  b=0.75
  
  Ld <- total_words.df[total_words.df$book == book_BM25_Check1$book,2]
  tftd <- book_BM25_Check1$n/Ld
  Lave <- mean(total_words.df$total)
  book_BM25_Check1.R <- log10((N-dft+0.5)/(dft+0.5))*(k+1)*tftd/(k*((1-b)+b*(Ld/Lave))+tftd)
  
  ## Check2
  doc_totals <- data.frame(Ld = tapply(book_BM25.df$n, book_BM25.df$book, sum))
  book_BM25_Check1 <- book_words.df
  WordCount.df <- as.data.frame(table(book_BM25.df$word))
  #WordCount_W.df <- WordCount.df[WordCount.df$Var1 == book_BM25_Check1$word,]
  
  N <- nrow(doc_totals)
  #dft <- WordCount.df$Freq
  colnames(WordCount.df) <- c("word","dft")
  
  book_BM25_Check1 <- left_join(book_BM25_Check1,WordCount.df,by="word")
  dft <- book_BM25_Check1$dft
  k=1.25
  b=0.75
  
  #Ld <- left_join( book_BM25_Check1,total_words.df,by="book")
  tftd <- book_BM25_Check1$n/book_BM25_Check1$total
  Lave <- mean(total_words.df$total)
  book_BM25_Check1 %>% mutate(bm25=log10((N-dft+0.5)/(dft+0.5))*(k+1)*tftd/(k*((1-b)+b*(book_BM25_Check1$total/Lave))+tftd)) ->book_BM25_Check1 
  sum(round(book_BM25.df$bm25,4) == round(book_BM25_Check1$bm25,4))
  
  
##### Current path and new folder setting  ##### 
  W2V.Path = setwd(getwd())
  W2V_Version = "20220111_W2V"
  dir.create(paste0(W2V.Path,"/",W2V_Version))
  
  RawDataPath = ""
  
##### 3.4 W2V #####
  library(udpipe)
  book_text.df <- austen_books()
  x <- tolower(book_text.df$text)
  x <- str_replace_all(x, "[[:punct:]]", "")
  ## Build a model
  library(word2vec)
  set.seed(123456789)
  # model <- word2vec(x = x, type = "cbow", dim = 15, iter = 20)
  model <- word2vec(x = x, type = "cbow", window = 10, dim = 15, iter = 25, lr=0.01)
  
  #https://www.rdocumentation.org/packages/word2vec/versions/0.3.4/topics/predict.word2vec
  embedding <- as.matrix(model)
  embedding2 <- predict(model, c("virtues", "aspect"), type = "embedding")
  lookslike <- predict(model, c("virtues", "aspect"), type = "nearest", top_n = nrow(embedding))
  lookslike$virtues
  lookslike$aspect
  
  KeyWord = "aspect"
  lookslike <- predict(model, KeyWord, type = "nearest", top_n = nrow(embedding))
  lookslike_Key <- lookslike[[1]]
  
   
  ## Save the model and read it back in and do something with it
  write.word2vec(model, paste0(W2V.Path,"/",W2V_Version, "mymodel2.bin"))
  model     <- read.word2vec(paste0(W2V.Path,"/",W2V_Version, "mymodel2.bin"))
  terms     <- summary(model, "vocabulary")
  embedding <- as.matrix(model)
  
  ## Visualise the embeddings
  ## Perform dimension reduction using UMAP + make interactive plot of only the adjectives for example
  library(uwot)
  viz <- umap(embedding, n_neighbors = 15, n_threads = 2)
  
  ## Static plot
  library(ggplot2)
  library(ggrepel)
  df  <- data.frame(word = gsub("//.+", "", rownames(embedding)), 
                    xpos = gsub(".+//", "", rownames(embedding)), 
                    x = viz[, 1], y = viz[, 2], 
                    stringsAsFactors = FALSE)
  #df  <- subset(df, xpos %in% c("JJ"))
  df  <- df[df$word %in% lookslike_Key[1:1000,]$term2,]
  ggplot(df, aes(x = x, y = y, label = word)) + 
    geom_text_repel() + theme_void() + 
    labs(title = "word2vec - adjectives in 2D using UMAP")
  
  ## Interactive plot
  library(plotly)
  plot_ly(df, x = ~x, y = ~y, type = "scatter", mode = 'text', text = ~word)
  
##### 3.5 Combine BM25 and CosinS #####
  colnames(lookslike_Key)[2] <- "word" 
  book_BM25_Cosin.df <- left_join(book_BM25.df,lookslike_Key,by = "word")
  
  source("FUN_BM25_W2V.R")
  book_BM25_Cosin.df <- book_BM25_Cosin.df[!is.na(book_BM25_Cosin.df$similarity), ]
  BM25_W2V_Score.df <- BM25_W2V_Score(book_BM25_Cosin.df,
                                      word, bm25, similarity, book, n, 
                                      mode=1, b = 0.75, MinMax=1)
 
  max(BM25_W2V_Score.df$Score)
  min(BM25_W2V_Score.df$Score)
  sd(BM25_W2V_Score.df$Score)
  
  BM25_W2V_Score.df %>% group_by(word) %>% slice(which.max((Score))) %>% 
                    arrange(.,desc(Score)) -> BM25_W2V_Score_Max.df
  BM25_W2V_Score.df %>% group_by(word) %>% mutate(meanScore=mean(Score))%>%  
                    slice(which.max((meanScore)))%>% 
                    arrange(.,desc(meanScore)) -> BM25_W2V_Score_Fin.df
  BM25_W2V_Score.df %>% group_by(word) %>% mutate(meanScore=mean(Score))%>%  
                     arrange(.,desc(meanScore)) -> BM25_W2V_Score_Fin2.df
  
  length(unique(BM25_W2V_Score.df$word))
  
##### 3.6 Visualization #####
  # po1 <- plot(BM25_W2V_Score.df$bm25,BM25_W2V_Score.df$Score)
  po1 <- ggplot(BM25_W2V_Score.df,aes(x=bm25,y=Score)) + 
    geom_point(shape=19) + xlab("bm25") + ylab("Score(bm25&W2V)")
  
  po2 <- ggplot(BM25_W2V_Score.df,aes(x=tf_idf,y=Score)) + 
    geom_point(shape=19) + xlab("tf_idf") + ylab("Score(bm25&W2V)")
  
  po3 <- ggplot(BM25_W2V_Score.df,aes(x=tf_idf,y=bm25)) + 
    geom_point(shape=19) + xlab("tf_idf") + ylab("bm25")
  po4 <- ggplot(BM25_W2V_Score_Fin2.df,aes(x=Score,y=meanScore)) + 
    geom_point(shape=19) + xlab("Score") + ylab("meanScore")
  po4
  
  p1 <- ggplot(BM25_W2V_Score_Fin.df,aes(x=bm25,y=Score)) + 
    geom_point(shape=19) + xlab("bm25") + ylab("Score(bm25&W2V)")
  p2 <- ggplot(BM25_W2V_Score_Fin.df,aes(x=tf_idf,y=Score)) + 
    geom_point(shape=19) + xlab("tf_idf") + ylab("Score(bm25&W2V)")
  p3 <- ggplot(BM25_W2V_Score_Fin.df,aes(x=tf_idf,y=bm25)) + 
    geom_point(shape=19) + xlab("tf_idf") + ylab("bm25")
  p4 <- ggplot(BM25_W2V_Score_Fin.df,aes(x=Score,y=meanScore)) + 
    geom_point(shape=19) + xlab("Score") + ylab("meanScore")
  
  # https://www.itread01.com/hkpfclq.html
  library(cowplot)
  library(ggplot2)
  plot_grid(p1,p2,p3,p4)
  plot_grid(po1,po2,po3)
  plot_grid(p3,po3)
  
  BM25_W2V_Score_Fin.df %>% 
    group_by(book) %>% 
    slice_max(Score, n = 15) %>% 
    ungroup() %>%
    mutate(word = reorder(word, Score)) %>%
    ggplot(aes(Score, word, fill = book)) +
    geom_col(show.legend = FALSE) +
    labs(x = "BM25", y = NULL) +
    facet_wrap(~book, ncol = 2, scales = "free")
  
