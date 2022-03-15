library(shiny)
library(tidyverse)
library(ggplot2)
library(GGally)

# Define UI ----
ui <- pageWithSidebar(
  
  titlePanel("Dataset Explorer"),
 
  sidebarPanel(
    conditionalPanel(condition="input.tabselected==1",
                     
                     fileInput("file1", "Choose CSV File",
                               multiple = FALSE,
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                     
                     # Input: Numeric entry for number of obs to view ----
                     numericInput(inputId = "obs",
                                  label = "Number of observations to view:",
                                  value = 10), uiOutput("number"),
                     
                     radioButtons("choice","Choose an option", choices=c("Dataset" = 1, "Structure" = 2,
                                                                         "Summary" = 3 ))
                     
    ),conditionalPanel(condition="input.tabselected==2",uiOutput("variable")),
    
    conditionalPanel(condition="input.tabselected==5",
                     numericInput(inputId = "sampleSize1",
                                  label = "Number of observations to view:",
                                  value = 10),uiOutput("dx1"),uiOutput("dy1"),uiOutput("dcolor1"),
                    
    radioButtons("sampleType1", "Plot sample type",
                 choices = list("Random n" = "random1", "First n" = "first1")),
    numericInput("sampleSeed1", "Sample seed", value = 1),
    p("Jitter and smoothing are only available when two numeric variables 
      are selected."),
    checkboxInput("jitter", "Jitter"),
    checkboxInput("smooth", "Smooth")
   
 ),conditionalPanel(condition="input.tabselected==4",
                    numericInput(inputId = "sampleSize2",
                                 label = "Number of observations to view:",
                                 value = 10),uiOutput("dx2"),uiOutput("dy2"),uiOutput("dcolor2"),
                   
                    radioButtons("sampleType2", "Plot sample type",
                                 choices = list("Random n" = "random2", "First n" = "first2")),
                    numericInput("sampleSeed2", "Sample seed", value = 1),
                   
                    
 ),conditionalPanel(condition="input.tabselected==6",
                    numericInput(inputId = "sampleSize3",
                                 label = "Number of observations to view:",
                                 value = 10),uiOutput("dx3"),uiOutput("dy3"),
                    
                    radioButtons("sampleType3", "Plot sample type",
                                 choices = list("Random n" = "random3", "First n" = "first3")),
                    numericInput("sampleSeed3", "Sample seed", value = 1),
                    
                    
 ),conditionalPanel(condition="input.tabselected==3",
                    numericInput(inputId = "sampleSize4",
                                 label = "Number of observations to view:",
                                 value = 10),uiOutput("dx4"),
                    
                    radioButtons("sampleType4", "Plot sample type",
                                 choices = list("Random n" = "random4", "First n" = "first4")),
                    numericInput("sampleSeed4", "Sample seed", value = 1),
                    
                    
 )),
  
  
  mainPanel(
    
    # Output: Tabset
    tabsetPanel(
                tabPanel("Data Overview", value=1, conditionalPanel(condition="input.choice==1", DT::dataTableOutput("dat")),
                         conditionalPanel(condition="input.choice==2", verbatimTextOutput("struct")),
                         conditionalPanel(condition="input.choice==3", verbatimTextOutput("summary"))),
                tabPanel("Descriptive statistics", value=2, verbatimTextOutput("descriptive")),
                tabPanel("One numeric or categorical variable", value=3, plotOutput("plot4")),
                tabPanel("One numeric and One categorical variable", value=4, plotOutput("plot2")),
                tabPanel("Two numerical variables", value=5, plotOutput("plot1")),
                tabPanel("Two categorical variables", value=6, plotOutput("plot3")),
                id = "tabselected"
                
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  data <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    
  })
  
  ## to output the dataset
  output$dat <- DT::renderDataTable({
    
    if (length(input$num) == 0) return(head(data(),n=input$obs))
    head(data(),n=input$obs) %>% dplyr::select(!!!input$num)
  })
  
  # to output the structure of the dataset
  output$struct <- renderPrint({
    dataset <- data()
    str(dataset)
  })
  
  # for summary
  
  output$summary <- renderPrint({
    dataset <- data()
    summary(dataset)
  })
  
  output$number<- renderUI({
    varSelectInput("num", "Variables to select", data(), multiple = TRUE)
  })
  
  
  output$variable<- renderUI({
    selectInput("var", "Variables", names(data()))
  })
  
  output$dx1<- renderUI({
    selectInput("x1", "X", names(data()))
  })
  
  output$dy1<- renderUI({
  selectInput("y1", "Y", c("None", names(data())), names(data())[[2]])
  })
  # only allow non-numeric variables for color
  output$dcolor1<- renderUI({
  selectInput("color1", "Color", c("None", names(data())))
  })
  output$dx2<- renderUI({
  selectInput("x2", "X", names(data()))})
  
  output$dy2<- renderUI({
  selectInput("y2", "Y", c("None", names(data())), names(data())[[2]])})
  
  # only allow non-numeric variables for color
  output$dcolor2<- renderUI({
  selectInput("color2", "Color", c("None", names(data())))})
  
  output$dx3<- renderUI({
  selectInput("x3", "X", names(data()))})
  
  output$dy3<- renderUI({
  selectInput("y3", "Y", c("None", names(data())), names(data())[[2]])})
  
  output$dx4<- renderUI({
  selectInput("x4", "X", names(data()))})
  
  output$descriptive <- renderPrint({
    dataset <- data()
    if (is.numeric(dataset[[input$var]])){
    pastecs::stat.desc(dataset[,input$var,drop=FALSE])}
  })

  # get new dataset sample for plotting
  idx1 <- reactive({
    if (input$sampleType1 == "first1") {
      1:input$sampleSize1
    } else {
      set.seed(input$sampleSeed1)
      sample(nrow(data()), input$sampleSize1)
    }
  })
  df1 <- reactive(data()[idx1(), , drop = FALSE])
  
  # get new dataset sample for plotting
  idx2 <- reactive({
    if (input$sampleType2 == "first2") {
      1:input$sampleSize2
    } else {
      set.seed(input$sampleSeed2)
      sample(nrow(data()), input$sampleSize2)
    }
  })
  df2 <- reactive(data()[idx2(), , drop = FALSE])
  
  # get new dataset sample for plotting
  idx3 <- reactive({
    if (input$sampleType3 == "first3") {
      1:input$sampleSize3
    } else {
      set.seed(input$sampleSeed3)
      sample(nrow(data()), input$sampleSize3)
    }
  })
  df3 <- reactive(data()[idx3(), , drop = FALSE])
  
  # get new dataset sample for plotting
  idx4 <- reactive({
    if (input$sampleType4 == "first4") {
      1:input$sampleSize4
    } else {
      set.seed(input$sampleSeed4)
      sample(nrow(data()), input$sampleSize4)
    }
  })
  df4 <- reactive(data()[idx4(), , drop = FALSE])
  
  plot_type1 <- reactive({
    if (input$y1 != "None")
      is.numeric(data()[[input$x1]]) + is.numeric(data()[[input$y1]])
    else
      -1
  })
  
 
  
    
      output$plot1 <- renderPlot({
      # both numeric variables: scatterplot
      # also allow for color, jitter & smoothing
        if (plot_type1() == 2) {
      p1 <- ggplot(df1(), aes_string(x = input$x1, y = input$y1))
      
      if (input$jitter)
        p1 <- p1 + geom_jitter(alpha = 0.5)
      else
        p1 <- p1 + geom_point(alpha = 0.5)
      
      if (input$smooth)
        p1 <- p1 + geom_smooth()
      
      # color change
      if (input$color1 != "None")
        p1 <- p1 + aes_string(color = input$color1)
      p1 <- p1 + labs(title = paste(input$y1, "vs.", input$x1))
      p1 <- p1 + 
        theme_bw() +
        theme(plot.title = element_text(size = rel(1.8), face = "bold", hjust = 0.5),
              axis.title = element_text(size = rel(1.5)))
      
      print(p1)
      
     } }, height=500
      )
      
      plot_type2 <- reactive({
        if (input$y2 != "None")
          is.numeric(data()[[input$x2]]) + is.numeric(data()[[input$y2]])
        else
          -1
      })

    
      output$plot2 <- renderPlot({
      # one numeric var, one character var: boxplot
      # allow color, don't allow jitter or smoothing
        if(plot_type2() == 1){
      p2<- p2 <- ggplot(df2(), aes_string(x = input$x2, y = input$y2)) + 
        geom_boxplot()
      
      # fill change
      if (input$color2 != "None")
        p2 <- p2 + aes_string(fill = input$color2)
      p2 <- p2 + labs(title = paste(input$y2, "vs.", input$x2))
      p2 <- p2 + 
        theme_bw() +
        theme(plot.title = element_text(size = rel(1.8), face = "bold", hjust = 0.5),
              axis.title = element_text(size = rel(1.5)))
      
      print(p2)
      
     } }, height=500)
     
      output$plot3 <- renderPlot({
      # two character variables: heatmap
      # don't allow color, jitter or smoothing
      temp_df <- reactive(df3()[, c(input$x3, input$y3), drop = FALSE] %>%
                            group_by(across()) %>%
                            summarize(count = n())
      )
      p <- ggplot(temp_df(), 
                  mapping = aes_string(x = input$x3, y = input$y3, fill = "count")) +
        geom_tile() +
        scale_fill_gradient(low = "#e7e7fd", high = "#1111dd")
      
      p <- p + labs(title = paste(input$y3, "vs.", input$x3))
      p <- p + 
        theme_bw() +
        theme(plot.title = element_text(size = rel(1.8), face = "bold", hjust = 0.5),
              axis.title = element_text(size = rel(1.5)))
      
      print(p)
      
      }, height=500)
      
      
      
    output$plot4 <- renderPlot({
     
      # only one variable: univariate plot
      #  don't allow jitter or smoothing or color
        dataset<-data()
        p <- ggplot(df4(), aes_string(x = input$x4))
      
      if (is.numeric(dataset[[input$x4]])){
        p <- p + geom_histogram(bins = 30)}
      else
        {p <- p + geom_bar()}
        
      p <- p + labs(title = paste("Distribution of", input$x4))
      p <- p + 
        theme_bw() +
        theme(plot.title = element_text(size = rel(1.8), face = "bold", hjust = 0.5),
              axis.title = element_text(size = rel(1.5)))
      
      print(p)
      
   } , height=500)
  
}
# Run the app ----
shinyApp(ui = ui, server = server)