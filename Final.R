library(shiny)
library(magrittr)
library(ggplot2)
library(lattice)
library(tm)
library(RTextTools)
library(e1071)
library(dplyr)
library(caret)
library(NLP)
library(shinythemes)
library(RColorBrewer)
library(wordcloud)
library(dplyr)
library(tidytext)



# Define UI for data upload app ----

ui <- fluidPage(
  
  theme = shinytheme("darkly"),
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",options(shiny.maxRequestSize=200*1024^2),
                multiple = TRUE),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tabsetPanel(
        tabPanel("Data",
                 h4("Table"),
                 tableOutput("first")
        ),
        tabPanel("Cleaned Reviews",tableOutput("contents")),
        tabPanel(" Term Count / Word Cloud",
                 tabsetPanel(
                   tabPanel("Intial Term Count table", tableOutput("Freq")),
                   tabPanel("Inital Word Cloud", plotOutput("plot")),
                   tabPanel("Cleaned Term Count table", tableOutput("Freq2")),
                   tabPanel("Cleaned Word Cloud", plotOutput("plot2")))
        ),
        tabPanel("Sentiment Scores", tabsetPanel(
          tabPanel("Sentiment Scores",tableOutput("sentiment")),
          tabPanel("Avg Sentiment",tableOutput("Avg")),
          tabPanel("Top 6 products",tableOutput("Top"))
        )),
        tabPanel("Scatter Plots", plotOutput("Scatter")))
    )
    
  )
  
)


# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$first <- renderTable({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  output$contents <- renderTable({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    corpus <- Corpus(VectorSource(df$Text))
    
    corpus.clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    temp <- data.frame(text=sapply(corpus.clean, identity), 
                       stringsAsFactors=F)
    df$Text = temp
    names(df)[10]<-paste("Review")
    
    subdf <- df[,c(1:3,10)]
    
    return(subdf)
    
    
  })
  
  
  output$plot <- renderPlot({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    corpus <- Corpus(VectorSource(df$Text))
    review_dtm <- DocumentTermMatrix(corpus)
    review_dtm = removeSparseTerms(review_dtm, 0.99)
    findFreqTerms(review_dtm, 200)
    freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
    wordcloud(rownames(freq), freq[,1], max.words=200, colors=brewer.pal(1, "Dark2"))
  })
  
  output$Freq <- renderTable({req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    corpus <- Corpus(VectorSource(df$Text))
    
    review_dtm <- DocumentTermMatrix(corpus)
    freq <- sort(colSums(as.matrix(review_dtm)), decreasing=TRUE)
    wf <- data.frame(word=names(freq), freq=freq)   
    return(wf)
    
  })
  
  
  output$plot2 <- renderPlot({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    corpus <- Corpus(VectorSource(df$Text))
    
    corpus.clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    review_dtm <- DocumentTermMatrix(corpus.clean)
    review_dtm = removeSparseTerms(review_dtm, 0.99)
    findFreqTerms(review_dtm, 200)
    freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
    wordcloud(rownames(freq), freq[,1], max.words=200, colors=brewer.pal(1, "Dark2"))
  })
  
  output$Freq2 <- renderTable({req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    corpus <- Corpus(VectorSource(df$Text))
    
    corpus.clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    review_dtm <- DocumentTermMatrix(corpus.clean)
    review_dtm = removeSparseTerms(review_dtm, 0.99)
    freq <- sort(colSums(as.matrix(review_dtm)), decreasing=TRUE)
    wf <- data.frame(word=names(freq), freq=freq)   
    return(wf)
    
  })
  
  
  output$sentiment <- renderTable({
    
    req(input$file1)
    
    dataf <- read.csv(input$file1$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
    
    corpus <- Corpus(VectorSource(dataf$Text))
    
    corpus.clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    review_dtm <- DocumentTermMatrix(corpus.clean)
    terms <- Terms(review_dtm)
    Term_count <- tidy(review_dtm)
    ap_sentiments <- Term_count %>%
      inner_join(get_sentiments("afinn"), by = c(term = "word"))
    g <- aggregate(ap_sentiments$score, by=list(Category=ap_sentiments$document), FUN=sum)
    g$Category <- as.numeric(as.character(g$Category))
    b <- g[order(g$Category) , ]
    names(b)[1]<-paste("Id")
    names(b)[2]<-paste("Sentiment_score")
    rownames(b) <- 1:nrow(b)
    dfscores <- merge(x = dataf, y = b, by = "Id", all = TRUE)
    dfscores$sentiment <- 'N/A'
    dfscores$sentiment <- ifelse(dfscores$Sentiment_score> 0, "Positive",ifelse(dfscores$Sentiment_score< 0, "Negative",NA))
    return(dfscores)
    
    
  })
  
  output$Avg <- renderTable({
    
    req(input$file1)
    
    dataf <- read.csv(input$file1$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
    
    corpus <- Corpus(VectorSource(dataf$Text))
    
    corpus.clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    review_dtm <- DocumentTermMatrix(corpus.clean)
    terms <- Terms(review_dtm)
    Term_count <- tidy(review_dtm)
    ap_sentiments <- Term_count %>%
      inner_join(get_sentiments("afinn"), by = c(term = "word"))
    g <- aggregate(ap_sentiments$score, by=list(Category=ap_sentiments$document), FUN=sum)
    g$Category <- as.numeric(as.character(g$Category))
    b <- g[order(g$Category) , ]
    names(b)[1]<-paste("Id")
    names(b)[2]<-paste("Sentiment_score")
    rownames(b) <- 1:nrow(b)
    dfscores <- merge(x = dataf, y = b, by = "Id", all = TRUE)
    dfscores$sentiment <- 'N/A'
    dfscores$sentiment <- ifelse(dfscores$Sentiment_score> 0, "Positive",ifelse(dfscores$Sentiment_score< 0, "Negative",NA))
    productavgscores <- aggregate( dfscores$Sentiment_score~dfscores$ProductId, dfscores, mean )
    UserPrd <-aggregate(cbind(UserReviews = dfscores$UserId) ~ dfscores$ProductId, 
                        data = dfscores, 
                        FUN = function(x){NROW(x)})
    PrdUsrAvgScore <- merge(x = productavgscores, y = UserPrd, by = "dfscores$ProductId", all = TRUE)
    names(PrdUsrAvgScore)[1] <- paste("ProductId")
    names(PrdUsrAvgScore)[2] <- paste("Avg_Scores")
    return(PrdUsrAvgScore)
    
    
  })
  
  output$Top <- renderTable({
    
    req(input$file1)
    
    dataf <- read.csv(input$file1$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
    
    corpus <- Corpus(VectorSource(dataf$Text))
    
    corpus.clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    review_dtm <- DocumentTermMatrix(corpus.clean)
    terms <- Terms(review_dtm)
    Term_count <- tidy(review_dtm)
    ap_sentiments <- Term_count %>%
      inner_join(get_sentiments("afinn"), by = c(term = "word"))
    g <- aggregate(ap_sentiments$score, by=list(Category=ap_sentiments$document), FUN=sum)
    g$Category <- as.numeric(as.character(g$Category))
    b <- g[order(g$Category) , ]
    names(b)[1]<-paste("Id")
    names(b)[2]<-paste("Sentiment_score")
    rownames(b) <- 1:nrow(b)
    dfscores <- merge(x = dataf, y = b, by = "Id", all = TRUE)
    dfscores$sentiment <- 'N/A'
    dfscores$sentiment <- ifelse(dfscores$Sentiment_score> 0, "Positive",ifelse(dfscores$Sentiment_score< 0, "Negative",NA))
    productavgscores <- aggregate( dfscores$Sentiment_score~dfscores$ProductId, dfscores, mean )
    UserPrd <-aggregate(cbind(UserReviews = dfscores$UserId) ~ dfscores$ProductId, 
                        data = dfscores, 
                        FUN = function(x){NROW(x)})
    PrdUsrAvgScore <- merge(x = productavgscores, y = UserPrd, by = "dfscores$ProductId", all = TRUE)
    names(PrdUsrAvgScore)[1] <- paste("ProductId")
    names(PrdUsrAvgScore)[2] <- paste("Avg_Scores")
    Top <- PrdUsrAvgScore[order(PrdUsrAvgScore$UserReviews,decreasing = TRUE),]
    Top6 <- head(Top)
    return(Top6)
    
    
  })
  
  output$Scatter <- renderPlot({
    req(input$file1)
    
    dataf <- read.csv(input$file1$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
    
    corpus <- Corpus(VectorSource(dataf$Text))
    
    corpus.clean <- corpus %>%
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords(kind="en")) %>%
      tm_map(stripWhitespace)
    
    review_dtm <- DocumentTermMatrix(corpus.clean)
    terms <- Terms(review_dtm)
    Term_count <- tidy(review_dtm)
    ap_sentiments <- Term_count %>%
      inner_join(get_sentiments("afinn"), by = c(term = "word"))
    g <- aggregate(ap_sentiments$score, by=list(Category=ap_sentiments$document), FUN=sum)
    g$Category <- as.numeric(as.character(g$Category))
    b <- g[order(g$Category) , ]
    names(b)[1]<-paste("Id")
    names(b)[2]<-paste("Sentiment_score")
    rownames(b) <- 1:nrow(b)
    dfscores <- merge(x = dataf, y = b, by = "Id", all = TRUE)
    dfscores$sentiment <- 'N/A'
    dfscores$sentiment <- ifelse(dfscores$Sentiment_score> 0, "Positive",ifelse(dfscores$Sentiment_score< 0, "Negative",NA))
    productavgscores <- aggregate( dfscores$Sentiment_score~dfscores$ProductId, dfscores, mean )
    UserPrd <-aggregate(cbind(UserReviews = dfscores$UserId) ~ dfscores$ProductId, 
                        data = dfscores, 
                        FUN = function(x){NROW(x)})
    PrdUsrAvgScore <- merge(x = productavgscores, y = UserPrd, by = "dfscores$ProductId", all = TRUE)
    names(PrdUsrAvgScore)[1] <- paste("ProductId")
    names(PrdUsrAvgScore)[2] <- paste("Avg_Scores")
    Top <- PrdUsrAvgScore[order(PrdUsrAvgScore$UserReviews,decreasing = TRUE),]
    Top6 <- head(Top)
    pltdata <- merge(x = dfscores, y = Top6, by="ProductId", all.y =TRUE)
    ggplot(pltdata, aes(x= pltdata$Sentiment_score, y=pltdata$Score, color=pltdata$ProductId)) +
      geom_point() +
      facet_wrap( ~ pltdata$ProductId) +
      ggtitle("         Top 6 Products") +
      labs(x ="Sentiment Scores", y = "User Ratings", fill = "ProductID") +
      theme(
        plot.title = element_text(color="red", size=20, face="bold"),
        axis.title.x = element_text(color="blue", size=14, face="bold"),
        axis.title.y = element_text(color="#993333", size=14, face="bold")
      )
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui,server = server)
