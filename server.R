options(shiny.maxRequestSize=30*1024^2)
##### Preload data
#load("D:/Ch_Bioinformatics Dropbox/Chang Charlene/##_GitHub/0-R/IR_Project3_Cha/word2vec_5year.Rdata")

load("word2vec_5year2_TFIDF.RData")

##### Server #####
server = function(input, output, session){

  
##### Main reactive #####
  df_reactive_XML = reactive({
    #XML.df <- XML_to_df(input$file1$datapath,input$word_select)
    Output_Sum <- XML_to_df(input$file1$datapath)
    XML.df <- Output_Sum[["XML.df"]]
  })
  
  Keyword_reactive_Ori_XML = reactive({
    Keyword = tolower(input$word_select)
  })
  
  Keyword_reactive_Stem_XML = reactive({
    Keyword = tolower(input$word_select)
  })
  
  
  Keyword_reactive_DR_XML = eventReactive(c(input$RunDP),{
    Output_Sum <- XML_to_df(input$file1$datapath)
    Abs.All_df.Word.C <- Output_Sum[["Abs.All_df.Word.C"]]
    ##
    # Dynamic Programming (by Biostrings pairwiseAlignment) Loop
    Keyword = tolower(input$word_select)
    
    Abs.All_df.Word.C.Score <- Abs.All_df.Word.C
    for (x in 1:length(Abs.All_df.Word.C$word)) {
      if (abs(nchar(as.character(Abs.All_df.Word.C$word[x]))-nchar(Keyword))<=1) {
        DP <- pairwiseAlignment(Keyword, as.character(Abs.All_df.Word.C$word[x]),scoreOnly=TRUE)
        Abs.All_df.Word.C.Score[x,3] <- DP
      }else
        Abs.All_df.Word.C.Score[x,3] <- -999 
    }
    
    colnames(Abs.All_df.Word.C.Score)[3] <- c("Score")
    Abs.All_df.Word.C.Score <- Abs.All_df.Word.C.Score[order(Abs.All_df.Word.C.Score$Score,decreasing = TRUE),]
    NewKeyword_Ori <- as.character(Abs.All_df.Word.C.Score$word[1])
    
    # ## Dynamic Programming (by Biostrings pairwiseAlignment) sapply Save time slitly
    # # https://www.biostars.org/p/15688/
    # # TTT <- sapply(as.character(Abstract.All_df.Word.C$word), function(x) pairwiseAlignment(toupper(x), Keyword, scoreOnly=TRUE)) 
    # Abstract.All_df.Word.C.Score <- Abstract.All_df.Word.C
    # DPScore <- sapply(as.character(Abstract.All_df.Word.C$word), function(x) pairwiseAlignment(tolower(x), tolower(Keyword), scoreOnly=TRUE)) 
    # Abstract.All_df.Word.C.Score["Score"] <- as.numeric(as.character(DPScore))
    # Abstract.All_df.Word.C.Score <- Abstract.All_df.Word.C.Score[order(Abstract.All_df.Word.C.Score$Score,decreasing = TRUE),]
    # NewKeyword <- as.character(Abstract.All_df.Word.C.Score$word[1])
    
  })


  Keyword_reactive_DR_Stem_XML = eventReactive(c(input$RunDP),{
    Output_Sum <- XML_to_df(input$file1$datapath)
    Abs.All_df.Word.Stem.C <- Output_Sum[["Abs.All_df.Word.Stem.C"]]
    ##
    # Dynamic Programming (by Biostrings pairwiseAlignment) Loop
    Keyword = tolower(input$word_select)
    #Keyword = TryKeyWord()
    Abs.All_df.Word.Stem.C.Score <- Abs.All_df.Word.Stem.C
    for (x in 1:length(Abs.All_df.Word.Stem.C$stem)) {
    if (abs(nchar(as.character(Abs.All_df.Word.Stem.C$stem[x]))-nchar(Keyword))<=1) {
      DP <- pairwiseAlignment(Keyword, as.character(Abs.All_df.Word.Stem.C$stem[x]),scoreOnly=TRUE)
      Abs.All_df.Word.Stem.C.Score[x,3] <- DP
    }else
      Abs.All_df.Word.Stem.C.Score[x,3] <- -999 
    }
  
    colnames(Abs.All_df.Word.Stem.C.Score)[3] <- c("Score")
    Abs.All_df.Word.Stem.C.Score <- Abs.All_df.Word.Stem.C.Score[order(Abs.All_df.Word.Stem.C.Score$Score,decreasing = TRUE),]
    NewKeyword_Stem <- as.character(Abs.All_df.Word.Stem.C.Score$stem[1])
  })

#  TryKeyWord  = eventReactive(c(input$SearchKW,input$file1$datapath), {
  TryKeyWord  = eventReactive(c(input$SearchKW), {
    if (input$word_select==""){ButKeyword =NULL
    }else{
    ButKeyword = tolower(input$word_select)
    }
    })
  OriKeyWord  =  reactive({
    ButKeyword = tolower(input$word_select)
  })
  

  
##### summary graph #####  
  output$HisFig <- renderPlot({
    NewKeyword_Ori <- Keyword_reactive_Ori_XML()
    NewKeyword_Stem <- Keyword_reactive_Stem_XML()
    if (length(input$file1)>0){
      ## Bar plot for XML
      
      Output_Sum <- XML_to_df(input$file1$datapath)
      Abs.All_df.Word.C <- Output_Sum[["Abs.All_df.Word.C"]]
      Abs.All_df.Word.Stem.C <- Output_Sum[["Abs.All_df.Word.Stem.C"]]
      Abs.All_df.Word.Stem.RmSW.C <- Output_Sum[["Abs.All_df.Word.Stem.RmSW.C"]]
      
      Abs.All_df.Sum <- c(Abs.All_df.Word.C$n,Abs.All_df.Word.Stem.C$n,Abs.All_df.Word.Stem.RmSW.C$n)
      Abs.All_df.Sum.max <- max(Abs.All_df.Sum)
      
      TryKey <- TryKeyWord()
      OriKey <- OriKeyWord()
      
      
      p1 <- FUN_BarPlot(Abs.All_df.Word.C,Abs.All_df.Sum.max, AES = aes(x = word, y = n), GB.Color = "#f5e6e8", GGTitle = "Original")
      

      p2 <- FUN_BarPlot(Abs.All_df.Word.Stem.C,Abs.All_df.Sum.max, AES = aes(x = stem, y = n), GB.Color = "#d5c6e0", GGTitle = "Porter")


      p3 <- FUN_BarPlot(Abs.All_df.Word.Stem.RmSW.C,Abs.All_df.Sum.max, AES = aes(x = stem, y = n), GB.Color = "#aaa1c8", GGTitle = "Porter+RmSW")  

      
      p4 <- ggplot(df_reactive_XML(), aes(x=df_reactive_XML()[,2], y=df_reactive_XML()[,9])) + geom_bar(stat="identity")+
        geom_bar(stat="identity", fill="#967aa1", colour="black")+
        xlab("PMID") + ylab("Number of Search Words")+ 
        theme(axis.text.x = element_text(angle=90, hjust=1)) +
        theme(axis.text=element_text(size=10), axis.title=element_text(size=14,face="bold")) 
      if (is.null(TryKey)){
        grid.arrange(p1, p2 ,p3 , nrow = 1)
      }else if (TryKey != OriKey){
        grid.arrange(p1, p2 ,p3 , nrow = 1)
      }else{
        p1_2 <- p1+
          geom_vline(xintercept = NewKeyword_Ori,color="#d90d6c", size=1, alpha = 0.8)+ # http://www.sthda.com/english/wiki/ggplot2-add-straight-lines-to-a-plot-horizontal-vertical-and-regression-lines
          annotate(geom = "text", x = NewKeyword_Ori, y = Abs.All_df.Word.C[Abs.All_df.Word.C$word %in% NewKeyword_Ori,2]+1, 
                   label = paste0(NewKeyword_Ori,",",Abs.All_df.Word.C[Abs.All_df.Word.C$word %in% NewKeyword_Ori,2]), hjust = "left",size=6)
        
        p2_2 <- p2+
          geom_vline(xintercept = NewKeyword_Stem,color="#d90d6c", size=1, alpha = 0.8)+ 
          annotate(geom = "text", x = NewKeyword_Stem, y = Abs.All_df.Word.Stem.C[Abs.All_df.Word.Stem.C$stem %in% NewKeyword_Stem,2]+1, 
                   label = paste0(NewKeyword_Stem,",",Abs.All_df.Word.Stem.C[Abs.All_df.Word.Stem.C$stem %in% NewKeyword_Stem,2]), hjust = "left",size=6)
        p3_2 <- p3+
          geom_vline(xintercept = NewKeyword_Stem,color="#d90d6c", size=1, alpha = 0.8)+ 
          annotate(geom = "text", x = NewKeyword_Stem, y = Abs.All_df.Word.Stem.RmSW.C[Abs.All_df.Word.Stem.RmSW.C$stem %in% NewKeyword_Stem,2]+1, 
                   label = paste0(NewKeyword_Stem,",",Abs.All_df.Word.Stem.RmSW.C[Abs.All_df.Word.Stem.RmSW.C$stem %in% NewKeyword_Stem,2]), hjust = "left",size=6)
        grid.arrange(p1_2, p2_2 ,p3_2 , nrow = 1)
      }
      


    }else{
      # p1 <- ggplot()
      # p2 <- ggplot()
      # p3 <- ggplot()
      # p4 <- ggplot()
      # grid.arrange(p1, p2 ,p3,p4 , nrow = 1)
      ## grid.arrange(p1, p2 ,p3 , nrow = 1)      
      TryKey <- TryKeyWord()
      OriKey <- OriKeyWord()
      if (is.null(TryKey)){
        grid.arrange(p1, p2 ,p3 , nrow = 1)
      }else if (TryKey != OriKey){
        grid.arrange(p1, p2 ,p3 , nrow = 1)
      }else{
        p1_2 <- p1+
          geom_vline(xintercept = NewKeyword_Ori,color="#d90d6c", size=1, alpha = 0.8)+ # http://www.sthda.com/english/wiki/ggplot2-add-straight-lines-to-a-plot-horizontal-vertical-and-regression-lines
          annotate(geom = "text", x = NewKeyword_Ori, y = Abs.All_df.Word.C[Abs.All_df.Word.C$word %in% NewKeyword_Ori,2]+1, 
                   label = paste0(NewKeyword_Ori,",",Abs.All_df.Word.C[Abs.All_df.Word.C$word %in% NewKeyword_Ori,2]), hjust = "left",size=6)
        
        p2_2 <- p2+
          geom_vline(xintercept = NewKeyword_Stem,color="#d90d6c", size=1, alpha = 0.8)+ 
          annotate(geom = "text", x = NewKeyword_Stem, y = Abs.All_df.Word.Stem.C[Abs.All_df.Word.Stem.C$stem %in% NewKeyword_Stem,2]+1, 
                   label = paste0(NewKeyword_Stem,",",Abs.All_df.Word.Stem.C[Abs.All_df.Word.Stem.C$stem %in% NewKeyword_Stem,2]), hjust = "left",size=6)
        p3_2 <- p3+
          geom_vline(xintercept = NewKeyword_Stem,color="#d90d6c", size=1, alpha = 0.8)+ 
          annotate(geom = "text", x = NewKeyword_Stem, y = Abs.All_df.Word.Stem.RmSW.C[Abs.All_df.Word.Stem.RmSW.C$stem %in% NewKeyword_Stem,2]+1, 
                   label = paste0(NewKeyword_Stem,",",Abs.All_df.Word.Stem.RmSW.C[Abs.All_df.Word.Stem.RmSW.C$stem %in% NewKeyword_Stem,2]), hjust = "left",size=6)
        grid.arrange(p1_2, p2_2 ,p3_2 , nrow = 1)
      }
    }

  })
  
##### Summary table #####  
  output$SumTable <- renderTable({
    if (length(input$file1)>0){
      df_reactive_XML()[,c(1:3,6:9)]
    }else{
      # XML.df0 <- data.frame(matrix(nrow = 0,ncol = 10))
      # colnames(XML.df0) <- c("NO.","ID","Time","Text","CHAR","WORD","SENT","Search Word","FileNo","LitNo")
      # XML.df0
      # Summary_table <- Output_Sum[["XML.df"]]
      # Summary_table <- na.omit(Summary_table)
      # Summary_table <- Summary_table[1:100,c(1:3,6:8)]
      # Summary_table <- as.matrix(Summary_table)
      Summary_table
    }
    
  },digits=0)  

  
##### Searching the Keywords #####
  # Reference # https://newbedev.com/highlight-word-in-dt-in-shiny-based-on-regex
  df_reactive_HL = reactive({
    if(length(input$file1)>0){
      # XML.df.Hl <- XML_to_df(input$file1$datapath,NewKeyword)
      Output_Sum <- XML_to_df(input$file1$datapath)
      XML.df.Hl <- Output_Sum[["XML.df"]]
      NewKeyword <- Keyword_reactive_Ori_XML()
      XML.df.Hl[,c(2,4,5)] %>%
        # Filter if input is anywhere, even in other words.
        filter_all(any_vars(grepl(NewKeyword, ., T, T))) %>% 
        # Replace complete words with same in XML.
        mutate_all(~ gsub(
          paste(c("\\b(", NewKeyword, ")\\b"), collapse = ""),
          "<span style='background-color:#d0d1ff;color:#7251b5;font-family: Calibra, Arial Black;'>\\1</span>", # font-family: Lobster, cursive
          ., TRUE, TRUE ))
    }else{
      # XML.df0 <- data.frame(matrix(nrow = 0,ncol = 3))
      # colnames(XML.df0) <- c("PMID","Title","Abstract")
      # XML.df0
      
      # Summary_table2 <- Output_Sum[["XML.df"]]
      # Summary_table2 <- na.omit(Summary_table2)
      # Summary_table2 <- Summary_table2[1:100,]
      # Summary_table2 <- as.matrix(Summary_table2)
      # Summary_table2
      # Keyword_reactive_Ori_XML
      
      
      XML.df.Hl <- Summary_table2
      NewKeyword <- Keyword_reactive_Ori_XML()
      XML.df.Hl[,c(2,4,5)] %>%
        # Filter if input is anywhere, even in other words.
        filter_all(any_vars(grepl(NewKeyword, ., T, T))) %>% 
        # Replace complete words with same in XML.
        mutate_all(~ gsub(
          paste(c("\\b(", NewKeyword, ")\\b"), collapse = ""),
          "<span style='background-color:#d0d1ff;color:#7251b5;font-family: Calibra, Arial Black;'>\\1</span>", # font-family: Lobster, cursive
          ., TRUE, TRUE ))
      
    }
  })

  output$table <- renderDataTable({
    datatable(df_reactive_HL(), escape = F, options = list(searchHighlight = TRUE,dom = "lt"))
  })
  
  ##### Word2Vector graph #####  
  ## Word2Vector df
  df_reactive_W2V_SRP = reactive({
    # ana_cachexia_5year_SRP
    if (length(Keyword_reactive_Ori_XML())==1){
      ## SG
      dist_Gene_5year_SRP = distance(file_name = "vec_5year_SRP.bin",search_word = Keyword_reactive_Ori_XML(),num = 1000) 
      dist_Gene_5year_SRP$word <- enc2utf8(as.character(dist_Gene_5year_SRP$word)) # turn to utf-8
      dist_Gene_5year_SRP <- dist_Gene_5year_SRP[Encoding(dist_Gene_5year_SRP$word)=='unknown',]# delet unknwon
      
      dist_Gene_5year_SRP$word =  as.character(dist_Gene_5year_SRP$word)
      colnames(dist_Gene_5year_SRP) <- c("Word","SG.CosDist")
      #dist_Gene_5year_SRP
      
      dist_Gene_5year_SRP_W2P = distance(file_name = "vec_5year_SRP_word2phrase.bin",search_word = Keyword_reactive_Ori_XML(),num = 1000) 
      dist_Gene_5year_SRP_W2P$word <- enc2utf8(as.character(dist_Gene_5year_SRP_W2P$word)) # turn to utf-8
      dist_Gene_5year_SRP_W2P <- dist_Gene_5year_SRP_W2P[Encoding(dist_Gene_5year_SRP_W2P$word)=='unknown',]# delet unknwon
      
      dist_Gene_5year_SRP_W2P$word =  as.character(dist_Gene_5year_SRP_W2P$word)
      colnames(dist_Gene_5year_SRP_W2P) <- c("Word","SG.W2P.CosDist")
      #df_W2V_SRP <- full_join(dist_Gene_5year_SRP, dist_Gene_5year_SRP_W2P,by="Word")
      
      ## CBOW
      dist_Gene_5year_SRP_CBOW = distance(file_name = "vec_5year_SRP_CBOW.bin",search_word = Keyword_reactive_Ori_XML(),num = 1000) 
      dist_Gene_5year_SRP_CBOW$word <- enc2utf8(as.character(dist_Gene_5year_SRP_CBOW$word)) # turn to utf-8
      dist_Gene_5year_SRP_CBOW <- dist_Gene_5year_SRP_CBOW[Encoding(dist_Gene_5year_SRP_CBOW$word)=='unknown',]# delet unknwon
      
      dist_Gene_5year_SRP_CBOW$word =  as.character(dist_Gene_5year_SRP_CBOW$word)
      colnames(dist_Gene_5year_SRP_CBOW) <- c("Word","CBOW.CosDist")
      
      df_W2V_SRP <- full_join(dist_Gene_5year_SRP, dist_Gene_5year_SRP_W2P,by="Word")
      #df_W2V_SRP <- full_join(df_W2V_SRP, dist_Gene_5year_SRP_CBOW,by="Word")
      
    }else{
      ana_cachexia_5year_SRP = word_analogy(file_name = "vec_5year_SRP.bin",search_words = Keyword_reactive_Ori_XML() ,num = 1000)
      ana_cachexia_5year_SRP$word <- enc2utf8(as.character(ana_cachexia_5year_SRP$word)) # turn to utf-8
      ana_cachexia_5year_SRP <- ana_cachexia_5year_SRP[Encoding(ana_cachexia_5year_SRP$word)=='unknown',]# delet unknwon
      
      ana_cachexia_5year_SRP$word =  as.character(ana_cachexia_5year_SRP$word)
      colnames(ana_cachexia_5year_SRP) <- c("SG.Word","SG.CosDist")
      ana_cachexia_5year_SRP
    }
  })
  
  output$W2VTable_SRP <- renderTable({
    df_reactive_W2V_SRP()
    })

  plot_tsne_word2phrase = reactive({
    if (nchar(Keyword_reactive_Ori_XML())!=0){
      tSNEPerX =Keyword_reactive_Ori_XML()
      Keyword =Keyword_reactive_Ori_XML()
      
      Title_SG_W2P = "Skip-gram & word2phrase"
      KY_W2P = Keyword
      BIN_W2P = "vec_5year_SRP_word2phrase.bin"
      tsne_word2phrase_plot2 <- tSNEPlot(data_5year_SRP_DR,KY_W2P,BIN_W2P,50,Title_SG_W2P)
      tsne_word2phrase_plot2
      
      Title_SG = "Skip-gram"
      KY_SG = Keyword
      BIN_SG = "vec_5year_SRP.bin"
      tsne_SG_plot2 <- tSNEPlot(data_5year_SRP_word2phrase_DR,KY_SG,BIN_SG,50,Title_SG)
      tsne_SG_plot2
      
      # Title_CBOW = "CBOW"
      # KY_SG = Keyword
      # BIN_SG_Cbow = "vec_5year_SRP_Cbow.bin"
      # tsne_SG_plot_Cbow <- tSNEPlot(data_5year_SRP_DR_Cbow,KY_SG,BIN_SG_Cbow,50,Title_CBOW)
      # tsne_SG_plot_Cbow
      par(mfrow=c(1,2))
      


      
      

      tsne_word2phrase_plot2
      tsne_SG_plot2
      grid.arrange(tsne_SG_plot2,tsne_word2phrase_plot2, nrow = 1)
      }else{
      #tsne_word2phrase_plot
      plot(0,type='n',axes=FALSE,ann=FALSE)
    }
    
  })
  
  output$W2V_DR <- renderPlot({
    plot_tsne_word2phrase()
  })
  
  
  plot_UMAP_SentFreq = reactive({
    grid.arrange(UMAP_Score_IF, UMAP_Score_Ori, UMAP_Score_ModeA ,UMAP_Score_ModeB , nrow = 1)
  })
  output$SentFreq <- renderPlot({
    plot_UMAP_SentFreq()
  })
  ##### Sentence Ranking #####
  df_reactive_XML = reactive({
    #XML.df <- XML_to_df(input$file1$datapath,input$word_select)
    Output_Sum <- XML_to_df(input$file1$datapath)
    XML.df <- Output_Sum[["XML.df"]]
  })
  
  ## Sent
  BestSentence = reactive({
    BestS <- as.character(BestText3)
  })
  
  output$BestSent <- renderText({
    BestSentence()
    
  })
  
  
  ## df Ori
  BestSentence_Table = reactive({
    BestS_Tb <- BestTextinPMID_Ori
    NewKeyword <- Keyword_reactive_Ori_XML()
    BestS_Tb[,c(1,2,3,6)] %>%
      # Filter if input is anywhere, even in other words.
      filter_all(any_vars(grepl(NewKeyword, ., T, T))) %>% 
      # Replace complete words with same in XML.
      mutate_all(~ gsub(
        paste(c("\\b(", NewKeyword, ")\\b"), collapse = ""),
        "<span style='background-color:#c9f5e2;color:#29805a;font-family: Calibra, Arial Black;'>\\1</span>", # font-family: Lobster, cursive
        ., TRUE, TRUE ))
    
  })
  # output$BestSent_Table <- renderTable({
  #   #datatable(BestSentence_Table(), escape = F, options = list(searchHighlight = TRUE,dom = "lt"))
  #   BestSentence_Table()
  # })
  output$BestSent_Table <- renderDataTable({
    datatable(BestSentence_Table(), escape = F, options = list(searchHighlight = TRUE,
                                                               pageLength = 100,dom = "lt"))
  })
  
  ## df Mode A
  BestSentence_TableA = reactive({
    BestS_Tb <- BestTextinPMID_ModeA
    NewKeyword <- Keyword_reactive_Ori_XML()
    BestS_Tb[,c(1,2,3,6)] %>%
      # Filter if input is anywhere, even in other words.
      filter_all(any_vars(grepl(NewKeyword, ., T, T))) %>% 
      # Replace complete words with same in XML.
      mutate_all(~ gsub(
        paste(c("\\b(", NewKeyword, ")\\b"), collapse = ""),
        "<span style='background-color:#c9f5e2;color:#29805a;font-family: Calibra, Arial Black;'>\\1</span>", # font-family: Lobster, cursive
        ., TRUE, TRUE ))
    
  })
  # output$BestSent_TableA <- renderTable({
  #   #datatable(BestSentence_Table(), escape = F, options = list(searchHighlight = TRUE,dom = "lt"))
  #   BestSentence_TableA()
  # })
  output$BestSent_TableA <- renderDataTable({
    datatable(BestSentence_TableA(), escape = F, options = list(searchHighlight = TRUE,
                                                                pageLength = 100,dom = "lt"))
  })
  
  
  ## df Mode B
  BestSentence_TableB = reactive({
    BestS_Tb <- BestTextinPMID_ModeB
    NewKeyword <- Keyword_reactive_Ori_XML()
    BestS_Tb[,c(1,2,3,6)] %>%
      # Filter if input is anywhere, even in other words.
      filter_all(any_vars(grepl(NewKeyword, ., T, T))) %>% 
      # Replace complete words with same in XML.
      mutate_all(~ gsub(
        paste(c("\\b(", NewKeyword, ")\\b"), collapse = ""),
        "<span style='background-color:#c9f5e2;color:#29805a;font-family: Calibra, Arial Black;'>\\1</span>", # font-family: Lobster, cursive
        ., TRUE, TRUE ))
    
  })
  # output$BestSent_TableB <- renderTable({
  #   #datatable(BestSentence_Table(), escape = F, options = list(searchHighlight = TRUE,dom = "lt"))
  #   BestSentence_TableB()
  # })
  output$BestSent_TableB <- renderDataTable({
    datatable(BestSentence_TableB(), escape = F, options = list(searchHighlight = TRUE,
                                                                pageLength = 100,dom = "lt"))
  })
  

  
}
