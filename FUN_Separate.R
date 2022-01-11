##### Separate the Paragraph to Sentence #####
SplitPara2Sent = function(XML.df.Abs1){
  
      # https://stackoverflow.com/questions/35304900/split-paragraph-into-sentences-in-r
      tmp <- strsplit(as.character(XML.df.Abs1), "(?<=\\.|\\?)\\s(?=[A-Z])", perl = TRUE) 
      # tmp <- str_split(XML.df.Abs1, " ")
      # tmp <- substr(XML.df.Abs1, " ")
      
      Output_Sent = tmp[[1]] %>% as.data.frame() #%>% as.character() %>% enc2utf8()
      
      # Remove <U+00A0>
      # https://stackoverflow.com/questions/41108617/remove-u00a0-from-values-in-columns-in-r
      Output_Sent <- as.data.frame(lapply(Output_Sent , function(x) {
        gsub("\u00A0", "", x) 
      }))
      colnames(Output_Sent) = "Text"

return(Output_Sent)
}

##### Separate the Sentence to Word #####
SplitSent2Word = function(XML.df.Abs1){
  ## https://cloud.tencent.com/developer/ask/44882
  #XML.df.Abs1 <- str_replace_all(XML.df.Abs1, "[[:punct:]]", " " ) 
  # https://www.codenong.com/21533899/
  XML.df.Abs1 <- gsub("[[:punct:]]","", XML.df.Abs1, ignore.case ="-" )
  tmp <- strsplit(as.character(XML.df.Abs1), " ", perl = TRUE) 
  # tmp <- substr(XML.df.Abs1, " ")
  
  Output_Sent = tmp[[1]] %>% as.data.frame() #%>% as.character() %>% enc2utf8()
  
  # Remove <U+00A0>
  # https://stackoverflow.com/questions/41108617/remove-u00a0-from-values-in-columns-in-r
  Output_Sent <- as.data.frame(lapply(Output_Sent , function(x) {
    gsub("\u00A0", "", x) 
  }))
  colnames(Output_Sent) = "Word"
  
  return(Output_Sent)
}

