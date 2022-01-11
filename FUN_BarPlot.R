# GGTitle = "Original"
# GB.Color = "#f5e6e8"
# XW = word
# YW = n

FUN_BarPlot <- function(Abs.All_df.Word.C, Abs.All_df.Sum.max, AES = aes(x = word, y = n), GB.Color = "#f5e6e8", GGTitle = "Title" ) {
  p1 <- ggplot(data = Abs.All_df.Word.C, AES) +
    geom_bar(stat="identity", fill = GB.Color, colour = GB.Color) +
    #geom_bar(stat="identity", fill="#f5e6e8", colour="#f5e6e8") +
    #geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    
    xlab("Rank order of terms") + ylab("Frequency")+
    theme(axis.text.x = element_blank()) + #theme(axis.text.x = element_text(angle=90, hjust=1)) +
    theme(axis.text=element_text(size=10), axis.title=element_text(size=14,face="bold"))+
    ggtitle(GGTitle)+ theme(
      plot.title = element_text(color="black", size=14, face="bold.italic")) +
    ylim(0, Abs.All_df.Sum.max)
  return(p1)
}
