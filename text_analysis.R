# top ####

library(shiny)
library(scales)
library(tm)
library(wordcloud)
library(SnowballC)
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
library(data.table)
library(googleVis)
library(shinyFiles)
library(slickR)
library(remotes)
library(knitr)
#library(shinyjs)
#library(shinyBS)

#qa <- readLines("./test_qa2.csv")
#QA <- readLines("./test_QA22.csv")
#review <- readLines("./test_review2.csv")

#qa <- fread("./test_qa2.csv")
#QA <- fread("./test_QA22.csv")
review <- fread("./test_review2.csv")
review <- setDF(review)

reviewDescription="./review_description.txt"
reviewText="./review_reviewText.txt"
reviewSummary="./review_summary.txt"
reviewTitle="./final/review_title.txt"
             

b64 <- base64enc::dataURI(file="./accuracy_review.png", mime="image/png")
b65 <- base64enc::dataURI(file="./stat_pics/1_reviewTime_category.png", mime="image/png")
b66 <- base64enc::dataURI(file="./stat_pics/2_reviewTime_Weekday.png", mime="image/png")
b67 <- base64enc::dataURI(file="./stat_pics/3_reviewTime_MMonth.png", mime="image/png")
b68 <- base64enc::dataURI(file="./stat_pics/4_reviewTime_Year.png", mime="image/png")
#b69 <- base64enc::dataURI(file="C:/Users/tuncay/OneDrive/Coding_Projects/amazon/final/wordcloud_pics/review_gram_1.png", mime="image/png")
#b70 <- base64enc::dataURI(file="C:/Users/tuncay/OneDrive/Coding_Projects/amazon/final/wordcloud_pics/review_gram_2.png", mime="image/png")


# ui ####
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Amazon Analysis"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Files",tabName = "file",icon = icon("file-text-o")),
                        menuItem("Dataset",tabName = "dataset",icon = icon("fas fa-database")),
                        menuItem("Visuals",tabName = "visual",icon = icon("far fa-images")),
                        menuItem("Word Breakdown",tabName = "breakdown",icon = icon("fas fa-sort-alpha-down")),
                        menuItem("Wordcloud",tabName = "wordcloud",icon = icon("cloud")),
                        menuItem("Word Count Bar Plot",tabName = "barplot",icon = icon("bar-chart-o")),
                        menuItem("Emotional Sentiment",tabName = "emotionalsentiment",icon = icon("far fa-smile-wink")),
                        menuItem(paste("Positive vs. Negative Sentiment"),tabName = "pnsentiment",icon = icon("far fa-compass")),
                        menuItem("Emotion Percentages Table",tabName = "emotionalpercentages",icon = icon("percent")),
                        menuItem("Plot Trajectory",tabName = "plottrajectory",icon = icon("line-chart")),
                        menuItem("Lexical Dispersion Plot",tabName = "lexical_plot",icon = icon("fas fa-paint-roller")),
                        menuItem("Sentiment",tabName = "sentiment",icon = icon("fas fa-chess-board")),
                        menuItem("Prediction",tabName = "ols",icon = icon("fas fa-equals")),
                        menuItem("References",tabName = "workscited", icon=icon("fas fa-asterisk")),
                        menuItem("Contact",tabName = "contact",icon=icon("fas fa-at"))
                      )),
                    
                    dashboardBody(
                      tabItems(
                        
                        # visual ####
                        tabItem(tabName = "visual",
                                helpText(paste("max-min range is 2se, mid_point is set to be mean......."),
                                  br(),
                                  br(),
                                  paste("1st visual: average overall review rate by category."),
                                         br(),
                                         paste("2nd visual: average overall review rate by weekday."),
                                         br(),
                                         paste("3rd visual: average overall review rate by month."),
                                         br(),
                                         paste("4th visual: average overall review rate by year."),
                                         br(),
                                         paste("5th visual: select categories overall rating density by weekday"),
                                         br(),
                                         paste("6th visual: health personal care category overall density by month")
                                  
                                ),
                                fluidPage(
                                    mainPanel(
                                      slickROutput("slickr2", width="100%",height = "500px")
                                    
                                  )
                                )),
                        
                        
                        
                        # ols model ####
                        tabItem(tabName = "ols",
                                br(),
                                br(),
                                helpText(paste("in process........."),
                                         br(),
                                         br(),
                                         paste("predicting 'overall' review rate of all categories,  "),
                                         br(),
                                         paste("and sub-categories...."))
                        ),
                        
                        # image ####
                        tabItem(tabName = "sentiment",
                                helpText(paste("Overall Accuracy of Bing Lexicon vs Sentiment"),
                                         br(),
                                         paste("This model can be applied on sub-categories."),
                                         br(),
                                         paste("For now, it is skipped. ")
                                         ),
                                fluidPage(
                                  img(src=b64)
                                )),
                        
                        
                        # upload ####
                        tabItem(tabName = "file",
                                helpText((paste("Please select a text data...")),
                                         br(),
                                         br(),
                                         paste("First download a file and then upload it to begin with analysis...")),
                                br(),
                                # upload
                                fileInput("selection", "Upload Text File:",multiple = TRUE),
                                
                                # file select
                                # shinyUI(bootstrapPage(
                                #   shinyFilesButton("texts", 'File select', 'Please select a file', TRUE)
                                # )),                   
                                br(),
                                br(),
                                helpText( paste(" Download a text file"),
                                          br(),
                                          br(),
                                          a("review dataset - desciptions",href= "https://drive.google.com/file/d/15xrpuxZFu9TObGFUkVdKgbR9kQiLW6N-/view?usp=sharing",target="_blank"),
                                          br(),
                                          br(),
                                          a("review dataset - reviews",href= "https://drive.google.com/file/d/1o-0cxaaXD_GKZI-AQAlT0aZsZf894-EB/view?usp=sharing",target="_blank"),
                                          br(),
                                          br(),
                                          a("review dataset - summary",href= "https://drive.google.com/file/d/1YV2N1TsUKSNbSnRkrrsrMwHKpAKPOEu2/view?usp=sharing",target="_blank"),
                                          br(),
                                          br(),
                                          a("review dataset - titles",href= "https://drive.google.com/file/d/1-ATWyO-O3zcj8lSOuJrLCJ2_WMHzTJTZ/view?usp=sharing",target="_blank"),
                                          
                                 #       selectInput("dataset", "Choose a Dataset Text:", 
                                  #                  choices = c("reviewDescription","reviewText","reviewSummary","reviewTitle"),selected = "reviewDescription"),
                                         br(),
                                     #    downloadButton("download","Downaload Text"),
                                         br()
                                         )),
                        
                        # datasets ####
                        tabItem(tabName = "dataset",
                                helpText(paste("There are three datasets in Amazon Recommender Systems analysis. "),
                                         br(),
                                         paste("qa is for single question and answer, QA is for multiple questions and answers, and review is for product reviews. They are all cleaned and merged with Amazon metada. "),
                                         br(),
                                         paste("For app purposes, only review dataset is used here. "),
                                         br(),
                                         paste("If you would like a snapshot of other two datasets, please email me. "),
                                         br(),
                                         paste("Data is received from Professor Julian McAuley with UCSD. Special thanks to Professor Julian for sharing the data."),
                                         br(),
                                         paste("For this analysis, only 1/10th of the data, randomly splitted across categories, is used.")
                                         ),
                                              fluidPage( titlePanel("Amazon Reviews DataTable"),
                                                
                                                # fluidRow(
                                                #   column(2,
                                                #          selectInput("cat",
                                                #                      "Category:",
                                                #                      c("All",
                                                #                        unique(as.character(review$category))))
                                                #   ),
                                                #   column(2,
                                                #          selectInput("overall",
                                                #                      "Overall:",
                                                #                      c("All",
                                                #                        unique(as.character(review$overall))))
                                                #   ),
                                                #   column(2,
                                                #          selectInput("reviewTime_Weekday",
                                                #                      "Weekday:",
                                                #                      c("All",
                                                #                        unique(as.character(review$reviewTime_Weekday))))
                                                #   ),
                                                #   column(2,
                                                #          selectInput("reviewTime_MMonth",
                                                #                      "Month:",
                                                #                      c("All",
                                                #                        unique(as.character(review$reviewTime_MMonth))))
                                                #   ),
                                                #   column(2,
                                                #          selectInput("reviewTime_Year",
                                                #                      "Year:",
                                                #                      c("All",
                                                #                        unique(as.character(review$reviewTime_Year))))
                                                #   )
                                                #   
                                                #   
                                                #   
                                                #   
                                                # ),
                                                # Create a new row for the table.
                                          #      DT::dataTableOutput("table",width = "960")

                                                # sidebarLayout(
                                                #   sidebarPanel(
                                                #     conditionalPanel(
                                                #       'input.dataset === "review"',
                                                #       checkboxGroupInput("show_vars", "Columns in review to show:",
                                                #                          names(review), selected = names(review))
                                                #       )),
                                                #   mainPanel(
                                                #     tabsetPanel(
                                                #       id = 'dataset',
                                                #       tabPanel("review", DT::dataTableOutput("table",width = "960"))
                                                #     )))
                                               DT::dataTableOutput("table", width = "960")

                                              )),

                        # Word Frequency Barplot Tab ####
                        tabItem(tabName = "barplot",
                                helpText(paste("This tab allows you to display the frequency of words "),
                                         paste("via a bar chart. The bar chart by default displays the first through tenth"),
                                         paste("most frequent words in the text.")),
                                actionButton(inputId = "barplot",label = "Create Barplot"),
                                numericInput(inputId = "numeric",label =  " From:",min = 1,max = 50000,step = 1,value = 1),
                                numericInput(inputId = "numeric2",label =  "To:",min = 1,max = 50000,step = 1,value = 10), 
                                checkboxInput(inputId = "horz",label = "Horizontal Bars",value = FALSE),
                                plotOutput("plot2")),
                        # WordCloud Tab ####
                        tabItem(tabName = "wordcloud",
                                fluidRow(
                                  box(actionButton(inputId = "update", label = "Create Wordcloud"),
                                      helpText(paste("The minimum frequency refers to the minimum number of times"),
                                               paste("the word needs to appear in the text to be included in the wordcloud.")),
                                      sliderInput("freq","Minimum Frequency:",min = 1,  max = 20, value = 2),
                                      helpText(paste("The maximum number of words refers to the maximum number of words"),
                                               paste("you want to appear in the wordcloud that is created.")),
                                      sliderInput("max","Maximum Number of Words:",min = 1,  max = 300,  value = 100),
                                      selectInput(inputId = "pal",label = "Cloud Color",choices = c("Set One"="Set1"),selected = "Set1")),
                                  box(plotOutput("plot"))),
                                fluidPage(
                                    mainPanel(
                                      slickROutput("slickr", width="100%",height = "500px")
                                    
                                  )
                                )),
                        # Emotional Sentiment Bar Chart Tab ####
                        tabItem(tabName = "emotionalsentiment",
                                helpText(paste("This tab allows you to calculate eight types of emotion present within the text."),
                                         paste("The following types of emotion are calculated:"),
                                         tags$b(paste("Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, and Trust.")),
                                         paste("The emotions calculated are the 8 basic universal emotions conveyed by humans in all cultures."),
                                         paste("Each bar represents the overall percentage of each emotion present within the text file.")),
                                actionButton("sentiment","Calculate Emotion"),
                                plotOutput("nrcplot")),
                        # Positive & Negative sentiment Tab ####
                        tabItem(tabName = "pnsentiment",
                                helpText(paste("This tab allows you to calculate the positive and negative sentiment present within the text."),
                                         paste("The following sentiments are calculated:"),
                                         tags$b(paste("Positive & Negative")),
                                         paste("The bar graphs displayed are in relation to the percentage of positive and negative words present in the text.")),
                                actionButton("negative","Calculate Positive & Negative Sentiment"),
                                plotOutput("nrcplot2")),
                        # Emotional Percentages Table Tab ####
                        tabItem(tabName = "emotionalpercentages",
                                box(helpText(paste("The data table created calculates the percentage of each emotion", 
                                                   "present within the text file and outputs it to a table."),
                                             paste("The following emotions are calculated:"),
                                             tags$b(paste("Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, and Trust.")),
                                             paste("The emotions calculated are the 8 basic universal emotions conveyed by humans in all cultures."),
                                             a("Reference: NRC Package",href="https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html",target = "_blank")),
                                    actionButton("scsentiment","Calculate Emotional %")
                                    ),
                                box(DT::dataTableOutput("scosentiment"))),
                        # Text Plot Trajectory Tab #####
                        tabItem(tabName = "plottrajectory",
                                helpText(paste("This tab allows you to plot the trajectory of the text."),
                                         paste("The plot will display the overall emotion of pieces of the text at different successive linear locations in the text. Large text files will be more condensed than small text files."),
                                         paste("The plot displayed can be thought of as the story arc in a movie or book. If text items besides books are used it is highly suggested to order the text correctly. The graph will show"),
                                         paste("how the emotional content of the text has changed over time e.g. beginning of a text to the end of the text.The Narrative Timeline axis refers to how the book,text, or comments"),
                                         paste("have changed from the beginning of the text to the end of the same text being analyzed. The Emotional Valence axis refers to the positive/good-ness and the negative/bad-ness of the text."),
                                         paste(" Positive valence or upward motion can be seen as the good linear parts of a story, while Negative Valence can be thought of as bad or negative linear parts of the story. Therefore,"),
                                         paste(" as the plotted line moves up or down it is in turn visualizing the good or bad parts of the text being analyzed.")),
                                actionButton("trajectory","Create Plot Trajectory"),
                                plotOutput("nrcplot3")),
                        # Text Bar Chart Tab #####
                        tabItem(tabName = "plotg",
                                helpText(paste("This tab allows you to create a bar chart that displays both the type of emotion and  type of sentiment"),
                                         paste("present within the text file. The percentage of each emotion and sentiment  is displayed at "),
                                         paste("the top of each bar.")),
                                actionButton("gplottwo","Create Barplot"),
                                plotOutput("gplot")),
                        # Word Tokenizer Tab ####
                        tabItem(tabName = "wordtokenizer",
                                helpText(paste("This tab allows you to utilize a  word tokenizer to see which words in a text are displayed together."),
                                         paste("You can choose to display words from 1 to 5 tokens. Therefore, words that appear next to each other"),
                                         paste("in the text will be displayed. If you choose 2, then two words that appear next to each"),
                                         paste("other will be displayed. You can choose up to 5 words that display next to each other, thus allowing"),
                                         paste("you ,the end user, to look for patterns in any text.")),
                                actionButton("bigram","Create Tokenizer Table"),
                                numericInput(inputId ="numeric3",label="Tokenizer Min.",min=1,max=5,value=2),
                                numericInput(inputId="numeric4",label="Tokenizer Max",min=1,max=5,value=2),
                                DT::dataTableOutput("nrcplot4")
                        ),
                        # Sentence Sentiment Finder Tab####
                        tabItem(tabName = "sentencefinder",
                                helpText(paste("This tab allows you to display sentences by emotion. A sentence may appear more"),
                                         paste("than once if an one emotion is closely related to another: e.g. anger and disgust.")),
                                actionButton("emotion","Get Sentence Sentiment"),
                                helpText(paste("Select the following number below that corresponds with the emotion you want to display:"),
                                         tags$b(paste("1 = Anger   2 = Anticipation   3 = disgust   4 = Fear   5 = Joy")),
                                         tags$b(paste("6 = Sadness   7 = Surprise   8 = Trust    9 = Negative   10 =  Positive"))),
                                numericInput(inputId = 'emselect',label =  'Emotion Selector',
                                             value = 1,min = 1,max = 10,step = 1),
                                DT::dataTableOutput("nrcplot5")),
                        # Word Frequency Tab ####
                        tabItem(tabName = "breakdown",
                                helpText(paste("This tab allows you to display the frequency of each word present within the text file."),
                                         br(),
                                         paste("The frequency of each word will be shown and can be searched via the interactive table displayed below.")),
                                box(actionButton("wbdown","Create Word Breakdown")),
                                DT::dataTableOutput("wordbreakdown")),
                       
                        # Works Cited ####
                        tabItem(tabName = "workscited",
                                helpText(strong("References :"),
                                         br(),
                                         br(),
                                         paste("Cashell, D. (2014)."),em("Social media sentiment analysis using data mining techniques"),paste(". National 	College of Ireland."),
                                         br(),
                                         paste("Di Noia, T. (2016). Mirizzi, R, Ostuni, V. Romito, D. Zanker, M"),em("Linked Open Data to Support Content-Based Recommender Systems"),paste(".Retrieved from:"),a("http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.912.7248&rep=rep1&type=pdf",href="http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.912.7248&rep=rep1&type=pdf",target="_blank"),
                                         br(),
                                         paste("Font Awesome."),em("Icons of Tab Items"),paste(". Retrieved from:"),a("https://fontawesome.com/icons?from=io",href="https://fontawesome.com/icons?from=io",target="_blank"),
                                         br(),
                                         paste("Gonzalez, B. (2019)."),em("Text analysis of a file"),paste(".Retrieved from:"),a("https://www.showmeshiny.com/text-analysis/",href= "https://www.showmeshiny.com/text-analysis/",target="_blank"),
                                         br(),
                                         paste("Gupta, S. (2018)."),em("Sentiment Analysis: Concept, Analysis and Applications"),paste(". Retrieved from:"),a("https://towardsdatascience.com/sentiment-analysis-concept-analysis-and-applications-6c94d6f58c17",href="https://towardsdatascience.com/sentiment-analysis-concept-analysis-and-applications-6c94d6f58c17",target="_blank"),
                                         br(),
                                         paste("Helm, B. (2016)."),em("How This Company Makes $70 Million Selling Random Stuff on Amazon"),paste(".Retrieved from:"),a("https://www.inc.com/magazine/201603/burt-helm/pharmapacks-amazon-warehouse.html",href="https://www.inc.com/magazine/201603/burt-helm/pharmapacks-amazon-warehouse.html",target="_blank"),
                                         br(),
                                         paste("Hennessey, A. (2014)."),em("Sentiment analysis of twitter: using knowledge based and machine learning techniques"),paste(". National College of Ireland."),
                                         br(),
                                         paste("Jockers, M. (2016)."),em("Introduction to the syuzhet package"),paste(".Retrieved from:"),a("https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html",href="https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html",target="_blank"),
                                         br(),
                                         paste("Madasamy, M. (2019)."),em("Introduction to recommendation systems and How to design Recommendation system,that resembling the Amazon"),paste(". Retrieved from:"),a("https://medium.com/@madasamy/introduction-to-recommendation-systems-and-how-to-design-recommendation-system-that-resembling-the-9ac167e30e95",href="https://medium.com/@madasamy/introduction-to-recommendation-systems-and-how-to-design-recommendation-system-that-resembling-the-9ac167e30e95",target="_blank"),
                                         br(),
                                         paste("McAuley, J. (2019)."),em("Recommender Systems Datasets"),paste(".Retrieved from:"),a("http://cseweb.ucsd.edu/~jmcauley/datasets.html",href="http://cseweb.ucsd.edu/~jmcauley/datasets.html",target="_blank"),
                                         br(),
                                         paste("Mohammad, S. (2013)."),em("NRC word-emotion association lexicon (aka emolex)"),paste(".Retrieved from:"),a("http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm",href="http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm",target="_blank"),
                                         br(),
                                         paste("Mullen. (2014)."),em("Introduction to sentiment analysis"),paste(".Retrieved from:"),a("https://lct-master.org/files/MullenSentimentCourseSlides.pdf",href="https://lct-master.org/files/MullenSentimentCourseSlides.pdf",target="_blank"),
                                         br(),
                                         paste("Robinson, D. (2016)."),em("Text analysis of trump's tweets confirms he writes only the angrier android half"),paste(".Retrieved from:"),a("http://varianceexplained.org/r/trump-tweets/",href= "http://varianceexplained.org/r/trump-tweets/",target="_blank"),
                                         br(),
                                         paste("Schafer, B. (1999). Konstan, J. Riedl, J"),em("Recommender Systems in E-Commerce"),paste(".Retrieved from:"),a("https://emunix.emich.edu/~sverdlik/COSC562/RecSysInECommerce.pdf",href="https://emunix.emich.edu/~sverdlik/COSC562/RecSysInECommerce.pdf",target="_blank"),
                                         br(),
                                         paste("Smith, D. (2015)."),em("Comparing subreddits, with latent semantic analysis in r"),paste(". Retrieved from:"),a("http://blog.revolutionanalytics.com/2017/03/comparing-subreddits.html",href="http://blog.revolutionanalytics.com/2017/03/comparing-subreddits.html",target="_blank"),
                                         br(),
                                          br(),
                                          br(),
                                         paste(" !!!! more literature review is required for further analysis...."),
                                          br(),
                                         br(),
                                         paste("Special thanks to Professor Julian McAuley with UCSD for sharing the Amazon data."),
                                         br(),
                                         br(),
                                          paste("Special thanks to Charlie Cohen (Teaching Assistant with NYCDSA) for helping with nested dictionaries for multiple questions(QA) part of Amazon dataset. "))),
                                          
                                          
                        # contact ####
                         tabItem(tabName = "contact",
                                helpText(paste("Ted Dogan"),
                                         br(),
                                         br(),
                                         paste("Github Profile"),em(" "),paste(""),a("https://github.com/ted2020",href= "https://github.com/ted2020",target="_blank"),
                                          br(),   
                                         br(),
                                         paste("Email Me"),em(" "),paste(""),a("tuncay2020@gmail.com",href= "mailto:tuncay2020@gmail.com",target="_blank")
                                )),
                        tabItem(tabName = "lexical_plot",
                                actionButton("lexical_run","Create Lexical Dispersion Plot"),
                                hr(),
                                textInput(inputId = "words",label = "Word to search for in text:"),
                                hr(),
                                plotOutput("distPlot")
                        ))))

# server ####
server <- function(input, output, session) {
  options(shiny.maxRequestSize=10000*1024^2)
  memory.limit(size = 18095)
  
  
  # pics ####
  output$slickr <- renderSlickR({
    imgs <- list.files("C:/Users/tuncay/OneDrive/Coding_Projects/amazon/final/wordcloud_pics/", pattern=".png", full.names = TRUE)
    slickR(imgs)
    
  })
  output$slickr2 <- renderSlickR({
    imgs2 <- list.files("C:/Users/tuncay/OneDrive/Coding_Projects/amazon/final/stat_pics/", pattern=".png", full.names = TRUE)
  slickR(imgs2)
  })
  
  # file select
  # shinyServer(function(input, output) {
  #   output$files <- shinyFileChoose(input, 'texts', root=c(root='.'), filetypes=c('','txt'))
  # })
  
  
  
 
  
  # Filter data based on selections ( doesnt work with scroller )
#  output$table <- DT::renderDataTable(
#    DT::datatable({   
#    data <- review
#    if (input$cat != "All") {
#      data <- data[data$category == input$cat,]
#    }
#    if (input$cat != "All") {
#      data <- data[data$overall == input$overall,]
#    }
#    if (input$reviewTime_Weekday != "All") {
#      data <- data[data$reviewTime_Weekday == input$reviewTime_Weekday,]
#    }
#    if (input$reviewTime_MMonth != "All") {
#      data <- data[data$reviewTime_MMonth == input$reviewTime_MMonth,]
#    }
#    if (input$reviewTime_Year != "All") {
#      data <- data[data$reviewTime_Year == input$reviewTime_Year,]
#    }
#    data
#  }))
  
  
  # scroll
  output$table <- DT::renderDataTable(
    review,
    options = list(scrollX = TRUE))
  
  

 # upload a file ####
  ford <- reactive({ 
    req(input$selection)
    inFile <- input$selection 
    df <- readLines(inFile$datapath)
    return(df)
  })
  
 
#   click box sidebar
  # review2 = review[sample(nrow(review), 10000), ]
  # output$table <- DT::renderDataTable({
  #   DT::datatable(review2[, input$show_vars, drop = FALSE])
  # })
  
  
  
  datasetInput <- reactive({
    switch(input$dataset,
           "qa" = qa,
           "QA" = QA,
           "review" = review)
  })
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  
  ford <- reactive({ 
    req(input$selection)
    inFile <- input$selection 
    df <- readLines(inFile$datapath)
    return(df)})
  
  
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
  

  # Create Text Terms Object ####
  text_terms <-reactive({
    doc_terms<- ford()
    doc_source<-VectorSource(doc_terms)
    doc_corpus <- VCorpus(doc_source)

    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),"div","label","span","input","text","script","class","type","shiny","panel",
                                              input$words))
      return(corpus)
    }
    doc_corp<-clean_corpus(doc_corpus)
    doc_dtm<-DocumentTermMatrix(doc_corp)
    doc_m<-as.matrix(doc_dtm)
    doc_frequencyone<-rowSums(doc_m)
    doc_frequency<-sort(doc_frequencyone,decreasing=TRUE)
  })
  
  # Renders WordCloud Plot ####
  observeEvent(input$update,{output$plot <- renderPlot({
    inFile <- input$selection
    if (is.null(inFile))
      return("Please File")
    withProgress(message = 'Creating WordCloud',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.75)
                   }
                 },env = parent.frame(n=1))
    # Wordcloud code ####
    set.seed(1)
    v <- terms()
    wordcloud(names(v), v, scale=c(6,0.5),
              min.freq = input$freq, max.words=input$max,
              rot.per=0.35,
              colors=brewer.pal(8, input$pal))
  })})
  
  # Renders Barplot plot code ####
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
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),"div","label","span","input","text","script","class","type","shiny","panel",
                                              c(input$words)))
      return(corpus)
    }
    
    doc_corp<-clean_corpus(doc_corpus)
    doc_dtm<-DocumentTermMatrix(doc_corp)
    doc_m<-as.matrix(doc_dtm)
    doc_frequencyone<-colSums(doc_m)
    doc_frequency<-sort(doc_frequencyone,decreasing=TRUE)
    barplot(doc_frequency[input$numeric:input$numeric2],col=input$color,horiz = input$horz,las=2)
  })})

  # Displays Text of File ####
  observeEvent(input$display,{output$text<-renderText({
    inFile <- input$selection
    if (is.null(inFile))
      return("Please File")
    ford()})})
  
  # Creates word breakdown matrix for csv file #####
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
    
    clean_corpus <- function(corpus){
      corpus <- tm_map(corpus, stripWhitespace)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus<- tm_map(corpus,removeNumbers)
      corpus <- tm_map(corpus, removeWords, c(stopwords("english"),"the","div","label","span","input","text","script","class","type","shiny","panel",
                                              "you","httpstco","for","amp","today","--"))
      return(corpus)
    }
    
    text_corp<-clean_corpus(text)
    text_dtm<-DocumentTermMatrix(text_corp)
    text_m<-as.matrix(text_dtm)
    
    # Calculate the rowSums: term_frequency ####
    term_frequency<-colSums(text_m)
    term_frequency<-sort(term_frequency,decreasing=TRUE)
    text_freq<-data.frame(term=names(term_frequency),num=term_frequency)
    text_freq
    return(text_freq)
  })
  
  # Textbreakdown Download ####
  output$downloadtwo <- downloadHandler(
    filename = function() { paste("TextBreakDown",input$name, sep='',".csv") },
    content = function(file) {
      write.csv(texterdf2(), file)
    })
  
  # Emotional Sentiment Analysis ####
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
    
    barplot(
      sort(colSums(prop.table(value[, 1:8]))),
      cex.names = 0.7,
      las = 1,
      main = "Emotional Sentiment by Word"
      ,col = input$colornow
    )
  })})
  
  # Positive and Negative Sentiment Analysis ####
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
    
    barplot(
      sort(colSums(prop.table(value[, 9:10]))),
      cex.names = 0.7,
      las = 1,
      main = "Positive vs. Negative Sentiment"
      ,col = input$colornow2
    )
  })})
  
  # Plot Trajectory ####
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
  
  #Tokenizer Table ####
  wordbreak2d<-reactive({
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
    tokenizer<-function(x)
      NGramTokenizer(x,Weka_control(min=input$numeric3,max=input$numeric4))
    doc_dtm<-DocumentTermMatrix(doc_corp,control = list(tokenize = tokenizer))
    doc_m<-as.matrix(doc_dtm)
    doc_frequencyone<-colSums(doc_m)
    doc_frequency<-names(doc_frequencyone)
    doc_frequency <- as.data.frame(doc_frequency)
    colnames(doc_frequency) <- c("Tokenized Words")
    doc_frequency
  })
  
  observeEvent(input$bigram,{output$nrcplot4<-DT::renderDataTable({
    withProgress(message = 'Creating Bigram Table',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    DT::datatable(
      wordbreak2d(),extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ))
  })})
  
  # Sentence Finder ####
  texterdf5<- reactive({
    value<- ford()
    s_v <- get_sentences(value)
    nrc_data <- get_nrc_sentiment(s_v)
    emotion_conveyed <- which(nrc_data[,input$emselect] > 0)
    final <- as.matrix(s_v[emotion_conveyed])
    final
  })
  
  observeEvent(input$emotion,{output$nrcplot5<-DT::renderDataTable({
    withProgress(message = 'Getting Sentences',
                 value = 0, {
                   for (i in 1:3) {
                     incProgress(1/3)
                     Sys.sleep(0.25)
                   }
                 },env = parent.frame(n=1))
    DT::datatable(
      texterdf5(),extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ))
  })})
  
  # Sentiment Analysis Score ####
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
  
  # Dataframe for Wordbreakdown ####
  texterdf3<- reactive({
    doc_terms<- ford()
    doc_source<-VectorSource(doc_terms)
    text <- VCorpus(doc_source)
    
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
    
    # Calculate the rowSums: term_frequency ####
    term_frequency<-colSums(text_m)
    term_frequency<-sort(term_frequency,decreasing=TRUE)
    
    # Creates data frame of words ####
    text_freq<-data.frame(term=names(term_frequency),num=term_frequency)
    colnames(text_freq) <- c("Term","Number of Occurences")
    text_freq
    return(text_freq)
  })
  
  # Word Breakdown Table ####  
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

  # Web Scrape Text ####
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

  # Lexical Dispersion Plot ####
  observeEvent(input$lexical_run,{output$distPlot <- renderPlot({
    lexical_terms<- ford()
    lexical.text <- scan( what = "characters",text = lexical_terms)
    dispersion_plot(lexical.text, input$words,
                    color = "black", bg.color = "grey90", horiz.color = "grey85",
                    total.color = "black", symbol = "|", title = "Lexical Dispersion Plot",
                    rev.factor = TRUE, wrap = "'", xlab = NULL, ylab = "Word Frequencies",
                    size = 3, plot = TRUE)
  })
  })
}

# Run ####
shinyApp(ui = ui, server = server)

