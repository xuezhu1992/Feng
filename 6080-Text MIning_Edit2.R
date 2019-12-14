#Library Packages
library(shiny)
library(scales)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)
library(xml2)
library(rvest)
library(dplyr)
library(reshape2)
library(tidytext)
library(syuzhet)
library(pander)
library(xlsx)
library(ggplot2)
library(RWeka)
library(RWekajars)
library(partykit)
library(DT)
library(shinydashboard)
library(qdap)
library(rJava)

##ui code
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "XN Final-Onclusive"),
                    ##Sidebar##
                    dashboardSidebar(
                      sidebarMenu(
                        ##Tab File Upload##
                        menuItem("File Upload",tabName = "file",icon = icon("file-text-o")),
                        ##Tabe Web Scrape Text##
                        menuItem("Web Scrape Text",tabName = "scrape_text",icon = icon("fab fa-internet-explorer")),
                        ##Tab Text Output##
                        menuItem("Text Output",tabName = "text",icon = icon("file-text-o")),
                        ##Tab WordBreakdown
                        menuItem("Word Breakdown",tabName = "breakdown",icon = icon("table")),
                        ##Tab Word Count
                        menuItem("Word Count Bar Plot",tabName = "barplot",icon = icon("bar-chart-o")),
                        ##Tab Wordcloud##
                        menuItem("Wordcloud",tabName = "wordcloud",icon = icon("cloud")),
                        ##Tab Emotional Sentiment##
                        menuItem("Emotional Sentiment",tabName = "emotionalsentiment",icon = icon("bar-chart-o")),
                        ##Tab Percentage##
                        menuItem("Emotion Percentages Table",tabName = "emotionalpercentages",icon = icon("percent")),
                        ##Tab P&N Sentiment##
                        menuItem(paste("Positive vs. Negative Sentiment"),tabName = "pnsentiment",icon = icon("bar-chart-o")),
                        ##Tab Emotion Trajectory##
                        menuItem("Plot Trajectory",tabName = "plottrajectory",icon = icon("line-chart")),
                        ##Reference##
                        menuItem("References:",tabName = "workscited"),
                        ##Text Analysis Report##
                        menuItem("Text Analysis Report",tabName = 'analysisreport')
                        
                      )),
                    
                    ##Dashboard Body###
                    dashboardBody(
                      tabItems(
                        ###File Upload Tab
                        tabItem(tabName = "file",
                                fileInput("selection", "Upload Text File:",multiple = TRUE),
                                helpText(paste("Please upload a .txt/csv file with the text", 
                                               "you would like to analyze."),
                                         br(),
                                         br(),
                                         tags$b(paste("* Please ensure the file uploaded utilizes UTF-8 encoding")))),
                        ###Text Output Tab
                        tabItem(tabName = "text",
                                helpText(paste("This tab displays the uploaded text.")),
                                actionButton("display","Display Text"),
                                br(),
                                br(),
                                box(title = "Text Ouput",textOutput("text",inline = FALSE),width = 450)),
                        ###Word Frequency Barplot Tab
                        tabItem(tabName = "barplot",
                                helpText(paste("This tab allows you to display the frequency of words in the uploaded text "),
                                         br(),
                                         paste("via a bar chart. The bar chart by default displays the first through tenth"),
                                         br(),
                                         paste("most frequent words in the text.")),
                                actionButton(inputId = "barplot",label = "Create Barplot"),
                                downloadButton(outputId = "downloadsix",label = "Download Barplot"),
                                selectInput(inputId = "download6",label = "Choose Format",choices = list("png","pdf","bmp","jpeg")),
                                numericInput(inputId = "numeric",label =  " From:",min = 1,max = 50000,step = 1,value = 1),
                                numericInput(inputId = "numeric2",label =  "To:",min = 1,max = 50000,step = 1,value = 10), 
                                checkboxInput(inputId = "horz",label = "Horizontal Bars",value = FALSE),
                                selectInput(inputId = 'color',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                            selected = "Blue"),
                                plotOutput("plot2")),
                        ###WordCloud Tab
                        tabItem(tabName = "wordcloud",
                                fluidRow(
                                  box(actionButton(inputId = "update", label = "Create Wordcloud"),
                                      helpText(paste("The minimum frequency refers to the minimum number of times"),
                                               br(),
                                               paste("the word needs to appear in the uploaded text to be included in the wordcloud.")),
                                      sliderInput("freq","Minimum Frequency:",min = 1,  max = 500, value = 10),
                                      helpText(paste("The maximum number of words refers to the maximum number of words"),
                                               br(),
                                               paste("you want to appear in the wordcloud that is created.")),
                                      sliderInput("max","Maximum Number of Words:",min = 1,  max = 1000,  value = 25),
                                      selectInput(inputId = "pal",label = "Cloud Color",choices = c("Dark"="Dark2","Pastel One"="Pastel1","Pastel Two"="Pastel2","Set One"="Set1",
                                                                                                    "Set Two"="Set2","Set Three"="Set3"),selected = "Dark2"),
                                      downloadButton("download1","Download Wordcloud"),
                                      selectInput(inputId = "download3",label = "Choose Wordcloud Format",choices = list("png","pdf","bmp","jpeg"))),
                                  box(plotOutput("plot")))),
                        ###Emotional Sentiment Bar Chart Tab
                        tabItem(tabName = "emotionalsentiment",
                                helpText(paste("This tab allows you to calculate eight types of emotion present within the uploaded text."),
                                         br(),
                                         br(),
                                         paste("The following types of emotion are calculated:"),
                                         br(),
                                         br(),
                                         tags$b(paste("Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, and Trust.")),
                                         br(),
                                         paste("The emotions calculated are the 8 basic universal emotions conveyed by humans in all cultures."),
                                         br(),
                                         paste("Each bar represents the overall percentage of each emotion present within the uploaded text file.")),
                                actionButton("sentiment","Calculate Emotion"),
                                br(),
                                br(),
                                downloadButton("downloadseven","Download Emotional Sentiment Barplot"),
                                selectInput(inputId = 'colornow',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                            selected = "Blue"),
                                plotOutput("nrcplot")),
                        ###Positive & Negative sentiment Tab
                        tabItem(tabName = "pnsentiment",
                                helpText(paste("This tab allows you to calculate the positive and negative sentiment present within the uploaded text."),
                                         br(),
                                         br(),
                                         paste("The following sentiments are calculated:"),
                                         br(),
                                         br(),
                                         tags$b(paste("Positive & Negative")),
                                         br(),
                                         paste("The bar graphs displayed are in relation to the percentage of positive and negative words present in the uploaded text.")),
                                actionButton("negative","Calculate Positive & Negative Sentiment"),
                                br(),
                                br(),
                                downloadButton(outputId = "downloadeight",label = "Download Pos vs. Neg Barplot"),
                                selectInput(inputId = 'colornow2',label =  'Color', choices = list("Blue","Red","Yellow","Green","Black","Orange","Pink","Brown","LightGreen","LightBlue","LightGrey"),
                                            selected = "Blue"),
                                br(),
                                plotOutput("nrcplot2")),
                        ###Emotional Percentages Table Tab
                        tabItem(tabName = "emotionalpercentages",
                                box(helpText(paste("The data table created calculates the percentage of each emotion", 
                                                   "present within the uploaded text file and outputs it to a table."),
                                             br(),
                                             br(),
                                             paste("The following emotions are calculated:"),
                                             br(),
                                             tags$b(paste("Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, and Trust.")),
                                             br(),
                                             br(),
                                             paste("The emotions calculated are the 8 basic universal emotions conveyed by humans in all cultures."),
                                             # paste("The following sentiments are also calculated:"),
                                             # br(),
                                             # tags$b(paste("Positive & Negative")),
                                             br(),
                                             br(),
                                    actionButton("scsentiment","Calculate Emotional %"),
                                    br(),
                                    br(),
                                    downloadButton("downloadfour","Download Emotional %")),
                                box(DT::dataTableOutput("scosentiment")))),
                        ###Text Plot Trajectory Tab
                        tabItem(tabName = "plottrajectory",
                                helpText(paste("This tab allows you to plot the trajectory of the uploaded text."),
                                         br(),
                                         br(),
                                         paste("The plot will display the overall emotion of pieces of the text at different successive linear locations in the text. Large text files will be more condensed than small text files."),
                                         br(),
                                         
                                         paste("The plot displayed can be thought of as the story arc in a movie or book. If text items besides books are used it is highly suggested to order the text correctly. The graph will show"),
                                         br(),
                                         paste("how the emotional content of the uploaded text has changed over time e.g. beginning of a text to the end of the text.The Narrative Timeline axis refers to how the book,text, or comments"),
                                         br(),
                                         paste("have changed from the beginning of the text to the end of the same text being analyzed. The Emotional Valence axis refers to the positive/good-ness and the negative/bad-ness of the text."),
                                         br(),
                                         paste(" Positive valence or upward motion can be seen as the good linear parts of a story, while Negative Valence can be thought of as bad or negative linear parts of the story. Therefore,"),
                                         br(),
                                         paste(" as the plotted line moves up or down it is in turn visualizing the good or bad parts of the text being analyzed.")),
                                actionButton("trajectory","Create Plot Trajectory"),
                                br(),
                                br(),
                                downloadButton("downloadnine","Download Plot Trajectory"),
                                plotOutput("nrcplot3")),
                        ###Word Frequency Tab
                        tabItem(tabName = "breakdown",
                                helpText(paste("This tab allows you to display the frequency of each word present within the uploaded text file."),
                                         br(),
                                         paste("The frequency of each word will be shown and can be searched via the interactive table displayed below.")),
                                box(actionButton("wbdown","Create Word Breakdown"),
                                    br(),
                                    br(),
                                    downloadButton("downloadtwo", label="Download Word Breakdown")),
                                DT::dataTableOutput("wordbreakdown")),
                        ###Text Analysis Report Tab
                        tabItem(tabName = "analysisreport",
                                downloadButton(outputId = "text_report",label = "Download Text Analysis Report")),
                        ###Works Cite
                        tabItem(tabName = "workscited",
                                helpText(strong("                                     References :"),
                                         br(),
                                         br(),
                                         paste("Cashell, D. (2014)."),em("Social media sentiment analysis using data mining techniques"),paste(". National 	College of Ireland."),
                                         br(),
                                         br(),
                                         paste("Hennessey, A. (2014)."),em("Sentiment analysis of twitter: using knowledge based and machine learning techniques"),paste(". National College of Ireland."),
                                         br(),
                                         br(),
                                         paste("Mohammad, S. (2013)."),em("NRC word-emotion association lexicon (aka emolex)"),paste(".Retrieved from:"),a("http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm",href="http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm",target="_blank"),
                                         br(),
                                         br(),
                                         paste("R studio"),em("Create a download button or link"),paste(".Retrieved from:"),a("https://shiny.rstudio.com/reference/shiny/1.0.4/downloadButton.html",href="https://shiny.rstudio.com/reference/shiny/1.0.4/downloadButton.html",target="_blank"),
                                         br(),
                                         br(),
                                         paste("Data Science Tutorials"),em("Shiny web app Tutorial"),paste(".Retrieved from:"),a("https://www.youtube.com/watch?v=Y5arqZ9Bp0A",href="https://www.youtube.com/watch?v=Y5arqZ9Bp0A",target="_blank"),
                                         br(),
                                         br(),
                                         paste("Mullen. (2014)."),em("Introduction to sentiment analysis"),paste(".Retrieved from:"),a("https://lct-master.org/files/MullenSentimentCourseSlides.pdf",href="https://lct-master.org/files/MullenSentimentCourseSlides.pdf",target="_blank"))),
                        ###Web Scrape Tab
                        tabItem(tabName = "scrape_text",
                                textInput(inputId = "text",label = "Enter Website url:",value = "",placeholder = "Enter valid website here"),
                                br(),
                                helpText("Enter the HTML node such as 'p' for paragraph to scrape the relevant data from the website. You can then download the text file to save it and upload it for analysis later."),
                                textInput(inputId = "node",label = "HTML Node",value = "",placeholder = "Enter valid CSS selector here"),
                                actionButton(inputId = "do",label = "Get Data",icon = icon("gears")),
                                br(),
                                br(),
                                textInput(inputId = "name",label = "Save File As:",value = "",placeholder = "Type File Name Here"),
                                br(),
                                downloadButton("download", label="Download"),
                                hr(),
                                verbatimTextOutput("printoutput")
                        )
                        
                        
                        
                      ))
                    ###End of ui Code
)

###server Code

# Define server logic required to run the Text Analysis App
server <- function(input, output, session) {
  options(shiny.maxRequestSize=100*1024^2)
  
 ##Code for uploading Text File from User
  
  ford <- reactive({ 
    req(input$selection)
    
    inFile <- input$selection 
    
    df <- readLines(inFile$datapath)
    
    return(df)
    
  })
  
  
  ##Create DocumentTerm Matrix (DTM) 
  
  getTermMatrix <- function(f) {
    
    
    text <- readLines(f$datapath,encoding = "UTF-8")
    
    docs<-Corpus(VectorSource(text))
    
    docs<-tm_map(docs, content_transformer(tolower))
    docs<-tm_map(docs, removePunctuation)
    docs<-tm_map(docs, removeNumbers)
    docs<-tm_map(docs, removeWords,
                 c(stopwords("SMART"),input$words))
    
    myDTM = TermDocumentMatrix(docs,
                               control = list(minWordLength = 1,wordLengths=c(0,Inf)))
    
    m = as.matrix(myDTM)
    
    sort(rowSums(m), decreasing = TRUE)
  }
  
  terms <- reactive({
    
    getTermMatrix(input$selection)
    
    
  })
  

  ##Create Text Terms Object 
  text_terms <-reactive({
    
    
    doc_terms<- ford()
    
    # Make a vector source: text_source
    doc_source<-VectorSource(doc_terms)

    doc_corpus <- VCorpus(doc_source)

    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),input$words))
      return(corpus)
    }
    
    doc_corp<-clean_corpus(doc_corpus)
    
    doc_dtm<-DocumentTermMatrix(doc_corp)
    
    # Convert text_dtm to a matrix: text_m
    doc_m<-as.matrix(doc_dtm)

    doc_frequencyone<-rowSums(doc_m)

    doc_frequency<-sort(doc_frequencyone,decreasing=TRUE)
    
  })
  
  ###Renders WordCloud Plot
  observeEvent(input$update,{output$plot <- renderPlot({
    inFile <- input$selection
    if (is.null(inFile))
      return("Please Upload File")
    withProgress(message = 'Creating WordCloud',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.75)
                   }
                 },env = parent.frame(n=1))
    ##Wordcloud code
    set.seed(1234)
    v <- terms()
    wordcloud(names(v), v, scale=c(6,0.5),
              min.freq = input$freq, max.words=input$max,
              rot.per=0.35,
              colors=brewer.pal(8, input$pal))
  })})
  
  
  ##Renders Barplot plot
  
  observeEvent(input$barplot,{output$plot2<-renderPlot({
    withProgress(message = 'Creating BarPlot',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    doc_terms<- ford()

    doc_source<-VectorSource(doc_terms)

    doc_corpus <- VCorpus(doc_source)
    
    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),c(input$words)))
      return(corpus)
    }
    
    doc_corp<-clean_corpus(doc_corpus)
    
    doc_dtm<-DocumentTermMatrix(doc_corp)

    doc_m<-as.matrix(doc_dtm)

    doc_frequencyone<-colSums(doc_m)

    doc_frequency<-sort(doc_frequencyone,decreasing=TRUE)
    
    # Plot a barchart of the 10 most common words 
    barplot(doc_frequency[input$numeric:input$numeric2],col=input$color,horiz = input$horz,las=2)
  })})
  
  
  ## Download code for wordcloud picture download
  
  output$download1 <- downloadHandler(
    filename = function() { paste("WordCloud",input$download3,sep = ".") },
    content = function(file) {
      if(input$download3=="png")
        png(file)
      else if (input$download3=="jpeg")
        jpeg(file)
      else if (input$download3=="bmp")
        bmp(file)
      else if (input$download3=="pdf")
        pdf(file)
      set.seed(1234)
      v <- terms()
      wordcloud(names(v),v, scale=c(6,0.5),
                min.freq = input$freq, max.words=input$max,
                rot.per=0.35,
                colors=brewer.pal(8, input$pal))
      dev.off()
    })
  
  
  
  ##Displays Text of Uploaded File
  
  observeEvent(input$display,{output$text<-renderText({
    inFile <- input$selection
    if (is.null(inFile))
      return("Please Upload File")
    ford()})})
  
  ## Creates word breakdown matrix for csv file
  
  texterdf2<- reactive({
    
    withProgress(message = 'Downloading CSV File',
                 value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    doc_terms<- ford()

    doc_source<-VectorSource(doc_terms)

    text <- VCorpus(doc_source)
    
    ##Function to Clean the Corpus
    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus<- tm_map(corpus,removeNumbers)
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),"the","you","httpstco","for","amp","today","--"))
      return(corpus)
    }
    
    # Apply your customized function to the text_corp: clean_corp
    text_corp<-clean_corpus(text)

    text_dtm<-DocumentTermMatrix(text_corp)

    text_m<-as.matrix(text_dtm)

    term_frequency<-colSums(text_m)

    term_frequency<-sort(term_frequency,decreasing=TRUE)

    text_freq<-data.frame(term=names(term_frequency),num=term_frequency)
    text_freq
    return(text_freq)
    
  })
  
  ##Textbreakdown Download
  
  output$downloadtwo <- downloadHandler(
    filename = function() { paste("TextBreakDown",input$name, sep='',".csv") },
    content = function(file) {
      write.csv(texterdf2(), file)
      
    })
  
  ##Emotional Sentiment Analysis
  observeEvent(input$sentiment,{output$nrcplot<-renderPlot({
    withProgress(message = 'Calculating Emotional Sentiment by Word',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    value<- ford()
    
    value <- get_nrc_sentiment(value)
    
    value
    
    #Barplot of Emotional Sentiment
    barplot(
      sort(colSums(prop.table(value[, 1:8]))),
      # horiz = input$horz2,
      cex.names = 0.7,
      las = 1,
      main = "Emotional Sentiment by Word"
      ,col = input$colornow
    )
    
  })})
  
  ##Positive and Negative Sentiment Analysis
  
  ##Sentiment
  observeEvent(input$negative,{output$nrcplot2<-renderPlot({
    withProgress(message = 'Calculating Positive & Negative Sentiment by Word',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    value<- ford()
    
    value <- get_nrc_sentiment(value)
    
    value
    
    ##Barplot of Emotional Sentiment
    barplot(
      sort(colSums(prop.table(value[, 9:10]))),
      # horiz = input$horz2,
      cex.names = 0.7,
      las = 1,
      main = "Positive vs. Negative Sentiment"
      ,col = input$colornow2
    )
    
  })})
  
  
  ## Plot Trajectory 
  observeEvent(input$trajectory,{output$nrcplot3<-renderPlot({
    withProgress(message = 'Creating Plot Trajectory',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    value<- ford()
    
    s_v <- get_sentences(value)
    s_v_sentiment <- get_sentiment(s_v)
    plot(
      s_v_sentiment, 
      type="l", 
      main="Plot Trajectory", 
      xlab = "Narrative Timeline", 
      ylab= "Emotional Valence"
    )
    
  })})
  

  ##Sentiment Analysis Score
  observeEvent(input$scsentiment,{output$scosentiment<-DT::renderDataTable({
    withProgress(message = 'Calculating Emotional Sentiment',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    value<- ford()
    
    value <- get_nrc_sentiment(value)
    
    prop.table(value[,1:8])
    
    sentimentscores <- round(colSums(prop.table((value[,1:8])))*100,digits = 1)
    
    sentimentscores <- as.data.frame(sentimentscores)
    colnames(sentimentscores) <- c("Percentages")
    
    
    Emotions <- c("anger","anticipation","disgust","fear","joy","sadness",
                  "surprise","trust")
    
    
    Percentages<- sentimentscores$Percentages
    emotionality<- cbind(Emotions,Percentages)
    emotionality
    
  })})
  
  
  ##Dataframe for Wordbreakdown
  
  texterdf3<- reactive({
    
    doc_terms<- ford()
    
    doc_source<-VectorSource(doc_terms)
    
    # Make a volatile corpus: rom_corpus
    text <- VCorpus(doc_source)
    
    ##Function to Clean the Corpus
    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus<- tm_map(corpus,removeNumbers)
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),"the","you","httpstco","for","amp","today","--"))
      return(corpus)
    }

    text_corp<-clean_corpus(text)

    text_dtm<-DocumentTermMatrix(text_corp)
    
    text_m<-as.matrix(text_dtm)

    term_frequency<-colSums(text_m)

    term_frequency<-sort(term_frequency,decreasing=TRUE)

    text_freq<-data.frame(term=names(term_frequency),num=term_frequency)
    colnames(text_freq) <- c("Term","Number of Occurences")
    text_freq
    return(text_freq)
    
  })
  
  ##Word Breakdown Table 
  observeEvent(input$wbdown,{output$wordbreakdown<-DT::renderDataTable({
    withProgress(message = 'Creating Word Breakdown',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    worddatabreakdown<- as.matrix.data.frame(texterdf3())  
    
    wordatabreakdown <- worddatabreakdown[,1:2]
    wordatabreakdown
    
  })})
  
  ##Download for Sentiment Percentages
  
  texterdf4<- reactive({
    
    withProgress(message = 'Downloading Emotional % CSV File',
                 value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    
    value<- ford()
    
    value <- get_nrc_sentiment(value)
    
    prop.table(value[,1:8])
    
    sentimentscores <- round(colSums(prop.table((value[,1:8])))*100,digits = 1)
    
    sentimentscores <- as.data.frame(sentimentscores)
    
    colnames(sentimentscores) <- c("Percentages")
    
    Emotions <- c("anger","anticipation","disgust","fear","joy","sadness",
                  "surprise","trust","negative","positive")
    
    Percentages<- sentimentscores$Percentages
    
    emotionality<- cbind(Emotions,Percentages)
    
  })
  
  output$downloadfour <- downloadHandler(
    filename = function() { paste("Emotional Percentage Breakdown",input$name, sep='',".csv") },
    content = function(file) {
      write.csv(texterdf4(), file)
      
    })
  
  output$downloadfive <- downloadHandler(
    filename = function() { paste("Emotion by Sentence Breakdown",input$name, sep='',".csv") },
    content = function(file) {
      write.csv(texterdf5(), file)
      
    })
  
  barplotdw <- reactive({
    doc_terms<- ford()

    doc_source<-VectorSource(doc_terms)

    doc_corpus <- VCorpus(doc_source)

    
    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),c(input$words)))
      return(corpus)
    }
    
    doc_corp<-clean_corpus(doc_corpus)
    
    doc_dtm<-DocumentTermMatrix(doc_corp)

    doc_m<-as.matrix(doc_dtm)

    doc_frequencyone<-colSums(doc_m)

    doc_frequency<-sort(doc_frequencyone,decreasing=TRUE)

    barplot(doc_frequency[input$numeric:input$numeric2],col=input$color,horiz = input$horz,las=2)
    
  })
  
  ##Barplot download code
  output$downloadsix <- downloadHandler(
    filename = function() { paste("Barplot",input$download6,sep = ".") },
    content = function(file) {
      if(input$download6=="png")
        png(file)
      else if (input$download6=="jpeg")
        jpeg(file)
      else if (input$download6=="bmp")
        bmp(file)
      else if (input$download6=="pdf")
        pdf(file)
      withProgress(message = 'Downloading BarPlot',
                   value = 0, {
                     for (i in 1:3) {
                       incProgress(1/3)
                       Sys.sleep(0.25)
                     }
                   },env = parent.frame(n=1))
      
      doc_terms<- ford()
      
      doc_source<-VectorSource(doc_terms)

      doc_corpus <- VCorpus(doc_source)

      clean_corpus <- function(corpus){
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removeWords, c(stopwords("english"),c(input$words)))
        return(corpus)
      }
      
      doc_corp<-clean_corpus(doc_corpus)
      
      doc_dtm<-DocumentTermMatrix(doc_corp)

      doc_m<-as.matrix(doc_dtm)

      doc_frequencyone<-colSums(doc_m)
      
      doc_frequency<-sort(doc_frequencyone,decreasing=TRUE)
  
      barplot(doc_frequency[input$numeric:input$numeric2],col=input$color,horiz = input$horz,las=2)
      dev.off()
    })
  

  ##Emotion ggplot2 reactive download code for barplot
  emotplot1 <- reactive({
    value<- ford()
    
    val_word <- get_tokens(value, pattern = "\\W")
    
    value <- get_nrc_sentiment(value)
    
    
    value <- as.data.frame(sort(colSums((prop.table(value[,1:8])))))
    
    colnames(value) <- "percentages"
    
    ggplot1<- ggplot(value, aes(x=sort(rownames(value),decreasing = FALSE), y=value$percentages)) +

      geom_bar(stat="identity", position="dodge",fill=input$colornow) +

      geom_text(aes(label=percent(value$percentages)), vjust=1, colour="white",
                position=position_dodge(.9), size=4)+labs(title="Emotional Sentiment",y = "Percentage",x="Emotion")+
      theme(panel.background = element_blank())
  })
  
  output$downloadseven <- downloadHandler(
    filename = function() { paste("Emotional Sentiment",'png',sep = ".") },
    content = function(file) {
      withProgress(message = 'Downloading BarPlot',
                   value = 0, {
                     for (i in 1:3) {
                       incProgress(1/3)
                       Sys.sleep(0.25)
                     }
                   },env = parent.frame(n=1))
      ggsave(file,emotplot1())})
  
  ##Positive vs Negative ggplot2 download 
  emotplot2 <- reactive({
    value<- ford()
    
    val_word <- get_tokens(value, pattern = "\\W")
    
    value <- get_nrc_sentiment(value)
    
    
    value <- as.data.frame(sort(colSums((prop.table(value[,9:10])))))
    
    colnames(value) <- "percentages"
    
    ggplot1<- ggplot(value, aes(x=sort(rownames(value),decreasing = FALSE), y=value$percentages))+
      # plot the bars
      geom_bar(stat="identity", position="dodge",fill=input$colornow2) +
      # create the label, "dodged" to fit the bars
      geom_text(aes(label=percent(value$percentages)), vjust=1, colour="white",
                position=position_dodge(.9), size=4)+labs(title="Positive vs. Negative Sentiment",y = "Percentage",x="Sentiment")+
      theme(panel.background = element_blank())
  })
  
  
  ##Plot Trajectory Download Code
  output$downloadnine <- downloadHandler(
    filename = function() { paste("Plot Trajectory",'png',sep = ".") },
    content = function(file) {

      withProgress(message = 'Downloading Plot Trajectory',
                   value = 0, {
                     for (i in 1:3) {
                       incProgress(1/3)
                       Sys.sleep(0.25)
                     }
                   },env = parent.frame(n=1))
      
      ggsave(file,sentimentplot())
    })
  
 
  ##Convert into Text Analysis Report
  output$text_report<- downloadHandler(
    filename = function() {
      paste('Text Analysis Report','pdf', sep = '.')
    },
    
    content = function(file) {
      src <- normalizePath('./text.Rmd')
      
      textReport <- file.path(tempdir(), "./text.Rmd")
      file.copy("./text.Rmd", textReport, overwrite = TRUE)
      
      library(rmarkdown)
      out <- render(input = 'text.Rmd',output_format = pdf_document()

      )
      file.rename(out, file)
      
      ##Set up parameters to pass to Rmd document
      params <- list(s = texterdf3())
      rmarkdown::render(tempReporters, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
  
  ##Web Scrape 
  observeEvent(input$do, {
    cat("Getting", input$text, "Data")
  })
  
  df_scrape <- eventReactive(input$do, {
    withProgress(message = 'Running',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/2)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    seven<-(input$text)
    value<-read_html(seven) %>%
      html_nodes(input$node) %>%
      html_text()
    
  })
  
  output$printoutput <- renderPrint({
    print(df_scrape())
  })
  
  tabledata<- reactive({
    
    seven<-(input$text)
    value<-read_html(seven) %>%
      html_nodes(input$node) %>%
      html_text()
    print(value)
  })
  
  output$download <- downloadHandler(
    filename = function() { paste("Text",input$name, sep='',".txt") },
    content = function(file) {
      write.table(tabledata(), file)
      
    })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
