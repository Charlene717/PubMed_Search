XML_to_df_Jl = function(input.datapath){
  
  
    ##### XML to df #####
    XML.df <- data.frame(matrix(nrow = 0,ncol = 12))
    colnames(XML.df) <- c("NO.","PMID","PubYear","Title","Abstract","CHAR","WORD","SENT","Search Word","FileNo","LitNo","Journal")
    
    xml.all <- list()
    
    for (k in 1:length(input.datapath)) {
      xml1 <- xmlParse(input.datapath[k], encoding="UTF-8") %>% 
        xmlToList()
      for (w in 1:length(xml1)) {
        names(xml1)[[w]] <- paste0("PubmedArticle",k,"-",w)
        xml1[[w]][[3]] <- as.numeric(k)
        names(xml1[[w]])[[3]] = c("FileNo") # Serial number of Files
        xml1[[w]][[4]] <- as.numeric(w)
        names(xml1[[w]])[[4]] = c("LitNo") # Serial number of Literatures
        }

      xml.all <- c(xml.all,xml1)
      rm(xml1)
    }   
    
    Abstract.All <- ""
    for (i in 1:length(xml.all)) {
      
      Abstract <- xml.all[[i]][["MedlineCitation"]][["Article"]][["Abstract"]]
      
      if (length(Abstract)==0 ) {
        XML.df[i,1] <- i
        XML.df[i,2] <- paste0("PMID: ",xml.all[[i]][["MedlineCitation"]][["PMID"]][["text"]])
        
        if (length(xml.all[[i]][["MedlineCitation"]][["Article"]][["Journal"]][["JournalIssue"]][["PubDate"]][["Year"]]) == 0){ 
          XML.df[i,3] <- ""
        }else {  
          XML.df[i,3] <- xml.all[[i]][["MedlineCitation"]][["Article"]][["Journal"]][["JournalIssue"]][["PubDate"]][["Year"]]
        }
   
        if (length(xml.all[[i]][["MedlineCitation"]][["Article"]][["ArticleTitle"]]) == 0){
          XML.df[i,4] <- ""
        }else {
          XML.df[i,4] <- xml.all[[i]][["MedlineCitation"]][["Article"]][["ArticleTitle"]]
        }
        

        XML.df[i,5:9] <- 0
        XML.df[i,10] <- xml.all[[i]][["FileNo"]]
        XML.df[i,11] <- xml.all[[i]][["LitNo"]]
        if (length(xml.all[[i]][["MedlineCitation"]][["Article"]][["Journal"]][["ISOAbbreviation"]]) == 0){ 
          XML.df[i,3] <- ""
        }else {  
          
        XML.df[i,12] <-xml.all[[i]][["MedlineCitation"]][["Article"]][["Journal"]][["ISOAbbreviation"]]
        }
      }else {
        try({
          if (length(Abstract)==1) {
            Abstract.1P <- str_c(Abstract[["AbstractText"]], collapse = " ") 
            # Abstract.1P <- Abstract[["AbstractText"]] %>% str_c(.,collapse=" ")
          }else {
            if (length(Abstract[["CopyrightInformation"]])==1) {
              Abstract.1P <- ""
              for (j in 1:(length(Abstract)-1)) {
                if (class(Abstract[[j]])!='character') {
                names(Abstract[[j]])[names(Abstract[[j]]) %in% c("i","b")] <- "text" 
                Abstract.1P <- paste0(Abstract.1P," ", str_c(as.character(Abstract[[j]][["text"]]),collapse=" "))
              
               }else {
              Abstract.1P <- paste0(Abstract.1P," ", Abstract[[j]])}
              Abstract.1P <- gsub("^\\s", "", Abstract.1P)
              
               }
            }else {

              Abstract.1P <- ""
              for (j in 1:(length(Abstract))) {
              names(Abstract[[j]])[names(Abstract[[j]]) %in% c("i","b")] <- "text" 
              Abstract.1P <- paste0(Abstract.1P," ", str_c(as.character(Abstract[[j]][["text"]]),collapse=" "))}
              Abstract.1P <- gsub("^\\s", "", Abstract.1P)
            }
          }
          
          Abstract.1P2 <- gsub('=','',Abstract.1P) 
          
          Abstract.1P_df <- tibble(line = 1:length(Abstract.1P), text = Abstract.1P)

          Abstract.1P_df %>%
            unnest_tokens(word, text) %>% as.data.frame() -> Abstract.1P_df.Word

        ##### Stemming (Porter's algorithm)#####
          ## Original
          Abstract.1P_df.Word.C <- Abstract.1P_df.Word  %>% count(word, sort = TRUE)
          Abstract.1P_df.Word.C <- Abstract.1P_df.Word.C[order(Abstract.1P_df.Word.C$n, decreasing = TRUE),]
          Abstract.1P_df.Word.C$word <-  factor(Abstract.1P_df.Word.C$word, levels = Abstract.1P_df.Word.C$word)

          
          ## Stemming
          Abstract.1P_df.Word.Stem <- Abstract.1P_df.Word %>% mutate(stem = wordStem(word))
          Abstract.1P_df.Word.Stem.C <- Abstract.1P_df.Word.Stem  %>% count(stem, sort = TRUE)
          Abstract.1P_df.Word.Stem.C <- Abstract.1P_df.Word.Stem.C[order(Abstract.1P_df.Word.Stem.C$n, decreasing = TRUE),]
          Abstract.1P_df.Word.Stem.C$stem <-  factor(Abstract.1P_df.Word.Stem.C$stem, levels = Abstract.1P_df.Word.Stem.C$stem)

          
          ## Remove the stop word
          Abstract.1P_df.Word.Stem.RmSW <- Abstract.1P_df.Word.Stem %>% anti_join(get_stopwords())
          Abstract.1P_df.Word.Stem.RmSW.C <- Abstract.1P_df.Word.Stem.RmSW  %>% count(stem, sort = TRUE)
          Abstract.1P_df.Word.Stem.RmSW.C <- Abstract.1P_df.Word.Stem.RmSW.C[order(Abstract.1P_df.Word.Stem.RmSW.C$n, decreasing = TRUE),]
          Abstract.1P_df.Word.Stem.RmSW.C$stem <-  factor(Abstract.1P_df.Word.Stem.RmSW.C$stem, levels = Abstract.1P_df.Word.Stem.RmSW.C$stem)

          
        # Fill the statistic result to df
        XML.df[i,1] <- i
        XML.df[i,2] <- paste0("PMID: ",xml.all[[i]][["MedlineCitation"]][["PMID"]][["text"]])
        XML.df[i,3] <- xml.all[[i]][["MedlineCitation"]][["Article"]][["Journal"]][["JournalIssue"]][["PubDate"]][["Year"]]
        XML.df[i,4] <- xml.all[[i]][["MedlineCitation"]][["Article"]][["ArticleTitle"]]
        
        Abstract.1P.paste0 <- ""
        for (c in 1:length(Abstract.1P)) {
          Abstract.1P.paste0 <-paste0(Abstract.1P.paste0, Abstract.1P[c])
        }
        XML.df[i,5] <- Abstract.1P.paste0
        XML.df[i,6] <- sum(nchar(Abstract.1P, type = "chars", allowNA = T, keepNA = NA))  # https://stat.ethz.ch/R-manual/R-devel/library/base/html/nchar.html
        XML.df[i,7] <- sapply(str_split(Abstract.1P, " "), length) # https://www.tutorialspoint.com/how-to-count-the-number-of-words-in-a-string-in-r
        XML.df[i,8] <- nsentence(Abstract.1P2)  # https://rdrr.io/cran/quanteda/man/nsentence.html
        XML.df[i,9] <- "NA"
        XML.df[i,10] <- xml.all[[i]][["FileNo"]]
        XML.df[i,11] <- xml.all[[i]][["LitNo"]]
        Abstract.All <- paste0(Abstract.All," ", Abstract.1P)
        rm(Abstract.1P,Abstract.1P2, Abstract.1P_df, Abstract.1P_df.Word)
        })

        
      }

      Abstract.All_df <- tibble(line = 1:length(Abstract.All), text = Abstract.All)
      
      Abstract.All_df %>%
        unnest_tokens(word, text) %>% as.data.frame() -> Abstract.All_df.Word
      
      ##### Stemming (Porter's algorithm)#####
      
      
      ## Original
      Abstract.All_df.Word.C <- Abstract.All_df.Word  %>% count(word, sort = TRUE)
      Abstract.All_df.Word.C <- Abstract.All_df.Word.C[order(Abstract.All_df.Word.C$n, decreasing = TRUE),]
      Abstract.All_df.Word.C$word <-  factor(Abstract.All_df.Word.C$word, levels = Abstract.All_df.Word.C$word)

      
      ## Stemming
      Abstract.All_df.Word.Stem <- Abstract.All_df.Word %>% mutate(stem = wordStem(word))
      Abstract.All_df.Word.Stem.C <- Abstract.All_df.Word.Stem  %>% count(stem, sort = TRUE)
      Abstract.All_df.Word.Stem.C <- Abstract.All_df.Word.Stem.C[order(Abstract.All_df.Word.Stem.C$n, decreasing = TRUE),]
      Abstract.All_df.Word.Stem.C$stem <-  factor(Abstract.All_df.Word.Stem.C$stem, levels = Abstract.All_df.Word.Stem.C$stem)

      
      ## Remove the stop word
      Abstract.All_df.Word.Stem.RmSW <- Abstract.All_df.Word.Stem %>% anti_join(get_stopwords())
      Abstract.All_df.Word.Stem.RmSW.C <- Abstract.All_df.Word.Stem.RmSW  %>% count(stem, sort = TRUE)
      Abstract.All_df.Word.Stem.RmSW.C <- Abstract.All_df.Word.Stem.RmSW.C[order(Abstract.All_df.Word.Stem.RmSW.C$n, decreasing = TRUE),]
      Abstract.All_df.Word.Stem.RmSW.C$stem <-  factor(Abstract.All_df.Word.Stem.RmSW.C$stem, levels = Abstract.All_df.Word.Stem.RmSW.C$stem)

      
      
      # Keyword.df <- Abstract.All_df.Word[Abstract.All_df.Word[,2] %in% c(Keyword,tolower(Keyword),toupper(Keyword),capitalize(Keyword)),]
      
    }
  # Put all result to output list  
  Output <- list()
  Output <- list(XML.df,
                 Abstract.All_df.Word, Abstract.All_df.Word.C,
                 Abstract.All_df.Word.Stem, Abstract.All_df.Word.Stem.C,
                 Abstract.All_df.Word.Stem.RmSW,Abstract.All_df.Word.Stem.RmSW.C)
  
  names(Output) <- c("XML.df",
                     "Abs.All_df.Word","Abs.All_df.Word.C",
                     "Abs.All_df.Word.Stem","Abs.All_df.Word.Stem.C",
                     "Abs.All_df.Word.Stem.RmSW","Abs.All_df.Word.Stem.RmSW.C")
  return(Output)
}
