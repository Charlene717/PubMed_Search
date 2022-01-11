tSNEPlot = function(data_5year_SRP_word2phrase_DR,KY,BIN,NB,Title,tSNEPerX=50){

  Keyword = KY
  dist_Keyword_word2phrase = distance(file_name = BIN ,search_word = Keyword,num = 1000)
  
  data_DR_word2phrase_K <- data_5year_SRP_word2phrase_DR[rownames(data_5year_SRP_word2phrase_DR) %in% c(as.character(dist_Keyword_word2phrase$word),Keyword),]
  
  set.seed(1) # Fix the seed
  tsne_word2phrase <- Rtsne(data_DR_word2phrase_K, perplexity = tSNEPerX, pca = FALSE)

  tsneP_word2phrase<- as.data.frame(tsne_word2phrase$Y)
  row.names(tsneP_word2phrase) <- row.names(data_DR_word2phrase_K)
  
  #
  #dist_Keyword_word2phrase = distance(file_name = "vec_5year_SRP_word2phrase.bin",search_word = Keyword,num = 1000)
  
  tsneP_word2phrase$SearchWord <- ifelse(row.names(tsneP_word2phrase) %in% c(as.character(dist_Keyword_word2phrase$word[1:NB]),Keyword),
                                         ifelse(row.names(tsneP_word2phrase) %in% Keyword,'SearchWord','SW_CosDist'),'Other')
  
  # tsneP_word2phrase$SearchWord <- ifelse(row.names(tsneP_word2phrase) %in% c(as.character(dist_Keyword_word2phrase$word[1:50]),Keyword) ,'SearchWord','Other')
  color <- c(SearchWord = "red",SW_CosDist = "#d538fc",Other = "gray")
  tsne_word2phrase_plot2 <- tsneP_word2phrase%>%
    as.data.frame() %>%
    mutate(word = row.names(tsneP_word2phrase)) %>%
    ggplot(aes(x = V1, y = V2, label = word, col = SearchWord))+
    scale_color_manual(values = color) +
    geom_text(size = 3)+ ggtitle(Title)
  
  return(tsne_word2phrase_plot2)
}
