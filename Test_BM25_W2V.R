
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

##### Environment #####
  library(dplyr)
  library(janeaustenr)
  library(tidytext)
  library(udpipe)
  library(word2vec)
  library(cowplot)
  library(ggplot2)
  library(lattice) # for densityplot

##### Parameter Setting #####
  # W2V
  W2V_parameter.lt <- list(
    type = "cbow", window = 10, dim = 15, iter = 25, lr=0.01
  )
  
  # Cosine similarity
  CSP = 1
  k=1.25
  b=0.1
##### Load data #####
  load("D:/Dropbox/##_GitHub/0-R/PubMed_Search/20220112_preprocess.RData")

## BM25 from TF-IDF Example
# https://www.tidytextmining.com/tfidf.html      
##### 1 Term frequency in 10000 PubMed abstract #####
  t_Text.df <- Text.df[!is.na(Text.df$Abstract),]

  PubMed_word.df <- t_Text.df %>%
                    unnest_tokens(word, Abstract) %>%
                    count(PMID, word, sort = TRUE)

  
  ## Ref: https://github.com/tidyverse/dplyr/issues/505
  total_words.df <- PubMed_word.df %>% 
                    group_by(PMID) %>% 
                    summarise(total = sum(n)) 
  
  PubMed_word.df <- left_join(PubMed_word.df, total_words.df)
  
  PubMed_word.df
  

##### 2 The BM25() function #####
  # source("C:/Users/user/Desktop/Pubmed_Search/FUN_BM25.R")
  source(paste0(getwd(),"/FUN_BM25.R"))
  PMID_BM25.df <- PubMed_word.df %>%
    BM25Score(word, PMID, n,total, k=k, b=b)
  plot(PMID_BM25.df$tf_idf ,PMID_BM25.df$bm25)  
  
  ## Check_1
  doc_totals <- data.frame(Ld = tapply(PMID_BM25.df$n, PMID_BM25.df$PMID, sum))
  PMID_BM25_Check1 <- PMID_BM25.df[1,]
  WordCount.df <- as.data.frame(table(PMID_BM25.df$word))
  WordCount_W.df <- WordCount.df[WordCount.df$Var1 == PMID_BM25_Check1$word,]
  
  N <- nrow(doc_totals)
  
  dft <- WordCount_W.df$Freq

  
  Ld <- total_words.df[total_words.df$PMID == PMID_BM25_Check1$PMID,2]
  tftd <- PMID_BM25_Check1$n/Ld
  Lave <- mean(total_words.df$total)
  PMID_BM25_Check1_result <- log10((N-dft+0.5)/(dft+0.5))*(k+1)*tftd/(k*((1-b)+b*(Ld/Lave))+tftd)
  
  rm(doc_totals, PMID_BM25_Check1, WordCount.df, WordCount_W.df, N, dft, Ld, tftd, Lave, PMID_BM25_Check1_result)
  
  ## Check2
  doc_totals <- data.frame(Ld = tapply(PMID_BM25.df$n, PMID_BM25.df$PMID, sum))
  PMID_BM25_Check2 <- PubMed_word.df
  WordCount.df <- as.data.frame(table(PMID_BM25.df$word))
  
  N <- nrow(doc_totals)
  colnames(WordCount.df) <- c("word","dft")
  
  PMID_BM25_Check2 <- left_join(PMID_BM25_Check2,WordCount.df,by="word")
  dft <- PMID_BM25_Check2$dft

  
  tftd <- PMID_BM25_Check2$n/PMID_BM25_Check2$total
  Lave <- mean(total_words.df$total)
  PMID_BM25_Check2 <- PMID_BM25_Check2 %>% 
    mutate(bm25=log10((N-dft+0.5)/(dft+0.5))*(k+1)*tftd/(k*((1-b)+b*(PMID_BM25_Check2$total/Lave))+tftd)) 
  sum(c(round(PMID_BM25.df$bm25,4) == round(PMID_BM25_Check2$bm25,4))) 
  
  # NA check
  PMID_BM25.df$bm25[is.na(round(PMID_BM25.df$bm25,4) == round(PMID_BM25_Check2$bm25,4))]
  
  
  rm(doc_totals, PMID_BM25_Check2, WordCount.df, N, dft, k, b, tftd, Lave)

##### 3. W2V  ##### 
  ## Generate a directory
  W2V.Path = paste0(getwd(),"/20220116_W2V")
  dir.create(W2V.Path)
  
  ## Build a model
  set.seed(123456789)
  
  model <- word2vec(
    tolower(t_Text.df$Abstract),
    type = W2V_parameter.lt$type,
    window = W2V_parameter.lt$window,
    dim = W2V_parameter.lt$dim,
    iter = W2V_parameter.lt$iter,
    lr = W2V_parameter.lt$lr
  )
  
  # Ref: https://www.rdocumentation.org/packages/word2vec/versions/0.3.4/topics/predict.word2vec
  embedding <- as.matrix(model)
  
  # test 1
  selected_embedding <- predict(model, c("virus", "cancer"), type = "embedding")
  rm(selected_embedding)
  
  # test 2
  nearest_term.lt <- predict(model, c("virus", "cancer"), type = "nearest", top_n = 10000)
  nearest_term.lt$virus[1:20,]
  nearest_term.lt$cancer[1:20,]
  rm(nearest_term.lt)
  
  # Formal search
  KeyWord = "cancer"
  nearest_term.lt <- predict(model, KeyWord, type = "nearest", top_n = 10000)
  nearest_term.df <- nearest_term.lt[[1]]
  
  ## Save the model and read it back in and do something with it
  write.word2vec(model, paste0(W2V.Path, "mymodel2.bin"))
  
  t_model     <- read.word2vec(paste0(W2V.Path, "mymodel2.bin"))
  t_terms     <- summary(t_model, "vocabulary")
  t_embedding <- as.matrix(t_model)
  t_model
  t_terms
  t_embedding
  rm(t_model, t_terms, t_embedding)

##### 4. Combine BM25 and Cosine Similarity #####
  colnames(nearest_term.df)[2] <- "word" 
  PMID_BM25_W2V.df <- left_join(PMID_BM25.df,nearest_term.df[2:3],by = "word")
  PMID_BM25_W2V.df$similarity[is.na(PMID_BM25_W2V.df$similarity)] <- 0
  PMID_BM25_W2V.df <- PMID_BM25_W2V.df %>% mutate(Score = bm25*abs(CSP*similarity))
  
  summary(PMID_BM25_W2V.df$Score)

##### 5. Visualization #####
  plot(PMID_BM25_W2V.df$tf_idf ,PMID_BM25_W2V.df$bm25)  
  plot(PMID_BM25_W2V.df$Score ,PMID_BM25_W2V.df$bm25)  
  plot(PMID_BM25_W2V.df$Score ,PMID_BM25_W2V.df$similarity)
  
##### 6. Rank the PMID #####
  PMID_BM25_W2V.df <- PMID_BM25_W2V.df %>% 
                      group_by(PMID) %>% 
                      mutate(PMIDScore=sum(Score)) %>%
                      arrange(desc(PMIDScore))
  summary(PMID_BM25_W2V.df$PMIDScore)
  
  #* Can check the Highest Score term in each paper
  PMID_Rank.df <- PMID_BM25_W2V.df %>% 
                  group_by(PMID) %>% 
                  slice(which.max(Score)) %>%
                  arrange(desc(PMIDScore))
  
  plot(PMID_Rank.df$PMIDScore, PMID_Rank.df$PMID)  
  
  densityplot( ~ PMIDScore ,      
               data=PMID_Rank.df
  ) 
  
  # Ref: https://www.geeksforgeeks.org/how-to-create-kernel-density-plot-in-r/
  plot(density(PMID_Rank.df$PMIDScore))                           
  abline(v = mean(PMID_Rank.df$PMIDScore), col = "red")
  abline(v = quantile(PMID_Rank.df$PMIDScore, c(0.25, 0.5, 0.75)), col = "blue")

  PMIDScore_Q.set <- quantile(PMID_Rank.df$PMIDScore, c(0.25, 0.5, 0.75))
  
##### 7 Rank the Sentence in each PMID #####  
  
  # Split Para to Sent
  source(paste0(getwd(),"/FUN_SplitPara2Sent.R"))
  
  PMID_Sent.df <- data.frame(matrix(nrow = 0,ncol = 4))
  for (i in c(1:nrow(t_Text.df))) {
    PMID_Sent_One <- SplitPara2Sent(t_Text.df[i,4])
    PMID_Sent.df <- rbind(PMID_Sent.df,data.frame(PMID= t_Text.df[i,1],
                                                  Line = seq(1,nrow(PMID_Sent_One),by=1),
                                                  Sent=PMID_Sent_One))
    
  }
  rm(PMID_Sent_One)
  
  # Count words in Sentences
  PMID_Sent_word.df <- PMID_Sent.df %>%
                       unnest_tokens(word, Text) %>%
                       count(PMID,Line, word, sort = TRUE)
  colnames(PMID_Sent_word.df)[4] <- "n.sent" 
  
  # Check
  PMID_Sent_word_Test.df <- mutate(PMID_Sent.df,PMIDLine=paste0(PMID,"_",Line))
  PMID_Sent_word_Test.df <- PMID_Sent_word_Test.df %>%
                            unnest_tokens(word, Text) %>%
                            count(PMIDLine, word, sort = TRUE)
  rm(PMID_Sent_word_Test.df)
  
  # Join the line information to summary df
  PMID_BM25_W2V.df <- left_join(PMID_BM25_W2V.df,PMID_Sent_word.df,by=c("PMID","word"))
  PMID_BM25_W2V.df <- arrange(PMID_BM25_W2V.df,PMID)
  
  # Rank the Sentence in each PMID
  PMID_BM25_W2V.df <- PMID_BM25_W2V.df %>% 
                      group_by(PMID,Line) %>% 
                      mutate(PMIDLineScore=sum(Score)) %>%
                      arrange(desc(PMIDLineScore))
  # https://pubmed.ncbi.nlm.nih.gov/34048775/
  PMID_BM25_W2V.df <- arrange(PMID_BM25_W2V.df,PMID) 
  summary(PMID_BM25_W2V.df$PMIDLineScore)
  PMID_BM25_W2V.df <- arrange(PMID_BM25_W2V.df,desc(PMIDScore))
  
  # Check
  PMID_BM25_W2V_TestPL.df <- PMID_BM25_W2V.df %>% 
                      slice(which.max(PMIDLineScore)) %>% 
                      group_by(PMID) %>% 
                      mutate(PMIDLineScoreCheck=sum(PMIDLineScore)) %>%
                      arrange(desc(PMIDLineScoreCheck))
  
  rm(PMID_BM25_W2V_TestPL.df)
  
  #* Can check the Highest Score term in each line
  PMIDLine_Rank.df <- PMID_BM25_W2V.df %>% 
                      group_by(PMID,Line) %>% 
                      slice(which.max(Score)) %>%
                      arrange(desc(PMIDScore))
  PMIDLine_Rank.df <- arrange(PMIDLine_Rank.df,PMID)
  