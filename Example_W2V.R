## word2vec
# https://cran.r-project.org/web/packages/word2vec/readme/README.html
##### Presetting ######
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


##### Current path and new folder setting  ##### 
  W2V.Path = setwd(getwd())
  W2V_Version = "20220111_W2V"
  dir.create(paste0(W2V.Path,"/",W2V_Version))
  
  RawDataPath = ""

##### Example #####
  ## Take some data and standardise it a bit
  library(udpipe)
  data(brussels_reviews, package = "udpipe")
  x <- subset(brussels_reviews, language == "nl")
  x <- tolower(x$feedback)

  ## Build a model
  library(word2vec)
  set.seed(123456789)
  # model <- word2vec(x = x, type = "cbow", dim = 15, iter = 20)
  model <- word2vec(x = x, type = "cbow", window = 10, dim = 15, iter = 25, lr=0.01)
  
  embedding <- as.matrix(model)
  embedding <- predict(model, c("bus", "toilet"), type = "embedding")
  lookslike <- predict(model, c("bus", "toilet"), type = "nearest", top_n = 5)
  lookslike$bus
  lookslike$toilet

  ## Save the model and read it back in and do something with it
  write.word2vec(model, paste0(W2V.Path,"/",W2V_Version, "mymodel.bin"))
  model     <- read.word2vec(paste0(W2V.Path,"/",W2V_Version, "mymodel.bin"))
  terms     <- summary(model, "vocabulary")
  embedding <- as.matrix(model)
  
##### Visualise the embeddings #####
  ## Using another example, we get the embeddings of words together with parts of speech tag (Look to the help of the udpipe R package to easily get parts of speech tags on text)
  library(udpipe)
  data(brussels_reviews_anno, package = "udpipe")
  x <- subset(brussels_reviews_anno, language == "fr" & !is.na(lemma) & nchar(lemma) > 1)
  x <- subset(x, xpos %in% c("NN", "IN", "RB", "VB", "DT", "JJ", "PRP", "CC",
                             "VBN", "NNP", "NNS", "PRP$", "CD", "WP", "VBG", "UH", "SYM"))
  x$text <- sprintf("%s//%s", x$lemma, x$xpos)
  x <- paste.data.frame(x, term = "text", group = "doc_id", collapse = " ")
  
  model     <- word2vec(x = x$text, dim = 15, iter = 20, split = c(" ", ".\n?!"))
  embedding <- as.matrix(model)
  
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
  df  <- subset(df, xpos %in% c("JJ"))
  ggplot(df, aes(x = x, y = y, label = word)) + 
    geom_text_repel() + theme_void() + 
    labs(title = "word2vec - adjectives in 2D using UMAP")
  
  ## Interactive plot
  library(plotly)
  plot_ly(df, x = ~x, y = ~y, type = "scatter", mode = 'text', text = ~word)
  

# ##### Pretrained models #####
# # https://github.com/maxoodf/word2vec#basic-usage
# # https://cran.r-project.org/web/packages/word2vec/readme/README.html
#   
#   library(word2vec)
#   model <- read.word2vec(file = "cb_ns_500_10.w2v", normalize = TRUE)
  
  
  