
##### Load library ########
library(readr)
library(DT)
library(magrittr) 

#library(NLP)
library(pdp)
library(shiny)
library(XML)
#library(jsonlite)
library(tibble)
library(tidytext)

library(data.table)
library(dplyr) # For `filter_all` and `mutate_all`.
library(Hmisc)
library(quanteda)
library(stringr)

library(hcandersenr)
library(tidyverse)


library(SnowballC) # For wordStem

# Dynamic Programming
library(dynprog) 
library(Biostrings) # Dynamic Programming (pairwiseAlignment)

# word2vec
library(rword2vec)

# Dimensionality reduction
library(Rtsne)

library(enc)
library(ggplot2)



#####  Function setting ##### 
source("FUN_XML_to_df.R") # Load function
source("FUN_tSNE.R") # Load function
source("FUN_BarPlot.R") # Load function
source("FUN_Separate.R") # Load function
source("FUN_TFIDF.R") # Load function
source("FUN_Beautify_ggplot.R") # Load function


# Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
##### UI ########
ui =   fluidPage( 
  # https://stackoverflow.com/questions/57037758/r-shiny-how-to-color-margin-of-title-panel
  titlePanel(h1("IR Project 4",
                style='background-color:#ece4db;  
                     color:#474973;
                     font-weight: 500;
                     font-family: Arial Black;
                     line-height: 1.2;
                     padding-left: 15px')),
  sidebarLayout( 
    sidebarPanel(fileInput("file1", "Choose XML Files", accept = ".xml", multiple = T),
                 textInput("word_select", label = "Word to search","Cachexia"),
                 actionButton("SearchKW", "Search"),
                 actionButton("RunDP", "DP")), 
    mainPanel(textOutput(outputId="BestSent")) ),
  
  ##### Summary Page #####
  tabsetPanel(
    tabPanel("Summary",  
             fluidPage(
               plotOutput("HisFig"),
               tableOutput("SumTable"))
             ),
    
    ##### Text search Page #####  
    navbarMenu("Text search",  
                 tabPanel("Key sentence",
                          fluidPage(fluidRow(dataTableOutput("BestSent_Table")))
                        ),
                 tabPanel("Key sentence ModeA",
                          fluidPage(fluidRow(dataTableOutput("BestSent_TableA")))
                        ),
                 tabPanel("Key sentence ModeB",
                          fluidPage(fluidRow(dataTableOutput("BestSent_TableB")))
                        ),
                 tabPanel("All Sentence",
                          fluidPage(fluidRow(dataTableOutput("table")))
                        )
              ),
    
    ##### Analysis search Page #####  
    navbarMenu("Analysis", 
               tabPanel("Word2Vector",    
                        fluidPage(tableOutput("W2VTable_SRP"))      
                  
               ),
               tabPanel("W2V Dimension Reduction",
                        fluidPage(plotOutput("W2V_DR"))
                       ),
               tabPanel("SentFreq Dimension Reduction",
                        fluidPage(img(src = "Monocle3_UMAP.PNG",
                                      height = "450px", width = "1700px", align = "center"), br())
               )
            )
  )
)
