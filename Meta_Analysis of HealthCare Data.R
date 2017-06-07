#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Search Analysis Tool"),
   h5("Created By - Shishir Shirbhate"),
   
   pageWithSidebar(
     headerPanel(""),
     sidebarPanel(
       fileInput('file1', 'Choose file to upload',
                 accept = c(
                   'text/csv',
                   'text/comma-separated-values',
                   'text/tab-separated-values',
                   'text/plain',
                   '.csv',
                   '.xml'
                 )
       ),
       selectInput(inputId = "n_topics",
                   label = "Number of Topics",
                   choices = c(2:50),
                   selected = 10),
       br(),
       actionButton("goButton", "Submit"),
       p("Click the button to display topics displayed in the main panel.")
     ),
     mainPanel(
       verbatimTextOutput("nText")
     )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
  # builds a reactive expression that only invalidates 
  # when the value of input$goButton becomes out of date 
  # (i.e., when the button is pressed)
  ntext <- eventReactive(input$goButton, {
    # getwd()
    #input$file1[["datapath"]]
    
    if (is.null(input$file1))
    {
      return("No file selected. Please select the input file")
    }
    
    if (is.null(input$file1) || input$file1[["type"]] != "application/xml")
    {
      return("Only '*.XML' format is accepted. Choose appropriate format")
    }
    
    # install.packages("XML")
    # Load the package required to read XML files.
    library("XML")
    
    # Also load the other required package.
    library("methods")
    
    # Give the input file name to the function.
    result <- xmlTreeParse(file = input$file1[["datapath"]],useInternalNodes = TRUE)
    nodes <- getNodeSet(result, "//F")
    
    dir.create("temp")

    j <- 1
    Titles <- ""
    for (i in 1:length(nodes))
    {
      file_name <-  paste("temp/",j,".txt",sep = "")
      if (xmlAttrs(nodes[[i]])[["L"]] == "Title")
      {
        Title <- xmlValue(nodes[[i]])
      }
      if (xmlAttrs(nodes[[i]])[["L"]] == "Abstract")
      {
        Titles <- paste(Titles,toString(j),Title,sep="\n")
        j <- j+1
        #print(xmlValue(nodes[[i]]))
        writeLines(xmlValue(nodes[[i]]), file_name)
      }
    }
    
    library(magrittr)
    library(dplyr)
    
    # Here are the documentation for packages used in this code:
    #https://cran.r-project.org/web/packages/tm/tm.pdf
    library(tm)
    #https://cran.r-project.org/web/packages/topicmodels/topicmodels.pdf
    library(topicmodels)
    
    # Use the SnowballC package to do stemming.
    library(SnowballC) 
    
    dirname <- file.path(getwd(),"temp")
    docs <- Corpus(DirSource(dirname, encoding = "UTF-8"))
    
    # The following steps pre-process the raw text documents. 
    # Remove punctuations and numbers because they are generally uninformative. 
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, removeNumbers)
    
    # Convert all words to lowercase. 
    docs <- tm_map(docs, content_transformer(tolower))
    
    # Remove stopwords such as "a", "the", etc. 
    docs <- tm_map(docs, removeWords, stopwords("english"))
    
    # Use the SnowballC package to do stemming. 
    docs <- tm_map(docs, stemDocument)
    
    # Remove excess white spaces between words. 
    docs <- tm_map(docs, stripWhitespace)
    
    # You can inspect the first document to see what it looks like with 
    #docs[[1]]$content
    
    # Convert all documents to a term frequency matrix. 
    tfm <- DocumentTermMatrix(docs)
    
    # We can check the dimension of this matrix by calling dim() 
    # print(dim(tfm))
    
    # we run LDA with 10 topics, and use Gibbs sampling as our method for identifying the optimal parameters 
    # Note: this make take some time to run (~10 mins)
    results <- LDA(tfm, k = as.integer(input$n_topics), method = "Gibbs")
    
    # Obtain the top w words (i.e., the w most probable words) for each topic, with the optional requirement that their probability is greater than thresh
    
    #feel free to explore with different values of w and thresh
    w=10
    thresh = 0.015
    Terms <- terms(results, w,thresh)
    
    # Obtain the most likely t topic assignments for each document. 
    t=1
    Topic <- topics(results,t)
    
    # Get the posterior probability for each document over each topic 
    posterior <- posterior(results)[[2]]
    
    unlink(dirname, recursive = TRUE, force = FALSE)
    
    folder_name <- toString(format(Sys.time(), "%F %H-%M"))
    dir.create(folder_name)
    
    opname <- file.path(getwd(),folder_name)
    
    write.csv(posterior, file = file.path(opname,"Doc.csv"))
    writeLines(Titles, file.path(opname,"Titles.txt"))
    capture.output(Terms, file = file.path(opname,"Topics.txt"))
    
    Terms
    
  })
  
  output$nText <- renderPrint({
    ntext()
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

