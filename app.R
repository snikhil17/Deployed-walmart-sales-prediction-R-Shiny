library(shiny)
library(httr)
library(data.table)
library(dplyr)
library(data.table)
library(lmtest)
library(ggplot2)
library(shinythemes)



URL <- "https://github.com/snikhil17/Walmart-R/blob/main/Walmart_Store_sales.csv"
apiResult <- GET(URL)




Walmart <- apiResult
Walmart1 <- Walmart
Walmart1$Date <- as.Date(Walmart1$Date, format = "%d-%m-%Y")
Walmart1$Week<- format(as.Date(Walmart1$Date), "%V")
Walmart1$Month <- month(Walmart1$Date)
Walmart1$Year <- year(Walmart1$Date)

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

# reference 
train_user <- TrainSet %>% 
  mutate(Temperature = exp(Temperature)) %>% 
  mutate(CPI = exp(CPI))

# Read in the RF model
model <- readRDS("model.rds")

# css <- HTML(" body {
#     background-color: #000000;
# }")

ui <- fluidPage(
  theme = shinytheme("slate"),
  # tags$head(tags$style(css)),
  # theme = shinytheme("darkly"),
  # "How to change this black?",
  pageWithSidebar(
    headerPanel('Walmart Weekly Sales Prediction'),
    sidebarPanel(
      tags$head(
        tags$style(type='text/css', 
                   ".nav-tabs {font-size: 20px} ")),
      HTML("<h3>Select input parameters for Prediction</h4>"),
      sliderInput("Temperature", label = "Temperature", value = min(train_user$Temperature),
                  min = round(min(train_user$Temperature),1),
                  max = round(max(train_user$Temperature),1),step = 5),
      sliderInput("Fuel_Price" , label = "Fuel Price", value = 3.717,
                  min = round(min(train_user$Fuel_Price),1),
                  max = round(max(train_user$Fuel_Price),1), step = 0.2),
      radioButtons("Store_Type", label = "Store Type (1: low Sales, 2: Medium Sales, 3: High Sales)", choices  = c(1,2,3),selected = 2),
      radioButtons("Week_type", label = "Week Type (1: low Sales, 2: Medium Sales, 3: High Sales)", choices  = c(1,2,3),selected = 3),
      sliderInput("CPI", label = "CPI", value = max(train_user$CPI),
                  min = round(min(train_user$CPI),1) + 1,
                  max = round(max(train_user$CPI),1) - 1, step = 1),
      radioButtons("Christmas", label = "Christmas Week: Yes(1), No(0)", choices  = c(0,1),selected = 0),
      
      actionButton("submitbutton", "Predict", class = "btn-info")
    ),
    mainPanel(
      tags$head(
        tags$style(type='text/css', 
                   ".nav-tabs {font-size: 20px} ")),
      tabsetPanel(
        tabPanel(
          h4("View & Summarizing"), 
          navbarPage(
            title = h5('Walmart'),
            tabPanel(h5('Dataset'),numericInput("obs", h4("Number of observations to view:"), 15),
                     actionButton("update", "Update View",class = "btn-info"),
                     h5("Walmart Original Data set"),
                     tableOutput("ex1")),
            tabPanel(h5('Summary'),fluidPage(verbatimTextOutput("ex3"))),
            tabPanel(h5('Structure'),fluidPage(verbatimTextOutput("ex4"))))),
        
        
        
        
        tabPanel(
          h4("Visualization"), 
          navbarPage(
            title = h5('Visualizing Walmart'),
            tabPanel(h5('Individual Variables Histogram'),radioButtons("ind", label = "Choose a Variable",
                                                                              choices = c("Weekly Sales","Temperature","Fuel Price", "CPI",
                                                                                          "Unemployment"), selected = "CPI"),fluidRow(plotOutput("hist"))),
            tabPanel(h5('Check the Outliers'),radioButtons("out", label = "Choose the Variable for checking Outliers",
                                                                  choices =c("Weekly Sales","Temperature","Fuel Price", "CPI","Unemployment"), selected = "Weekly Sales"),fluidRow(plotOutput("boxplot"))),
            tabPanel(h5('Weekly Sales by Month'),radioButtons("filling", label = "Choose what to fill",
                                                                     choices = c("Holiday Week: Yes(1), No(0)","Month"), selected = "Month"),fluidRow(plotOutput("histogram"))))),
        
        tabPanel(h4('Processed Data'),numericInput("obs1", h4("Number of observations to view:"), 10),
                 actionButton("update1", "Update View",class = "btn-info"),
                 h3("Walmart Data set Used For Prediction"),
                 tableOutput("ex2"),
                 helpText(h3("- Divided varibales stores and Week into 1 2 3, where we made a new Variable Store_Type and Week_type to factorize stores and Weeks.")),
                 helpText(h3("- The Stores and Weeks which had sales more than upper limit we call them 3, which implies High weekly sales.")),
                 helpText(h3("- Similarly, Stores and Weeks with monthly sales less than Lower threshold are called 1. Rest every store as 2")),
                 helpText(h3("->> Threshold was defined using Weekly sales, Month, Stores etc."))),
        
        
        
        tabPanel(h4("Prediction"),
                 verbatimTextOutput('contents'),
                 h2(tableOutput('tabledata')))
        
      )
      
    )
  )
)


server<- function(input, output, session){
  
  
  
  #individual Variables Histogram
  output$hist <-renderPlot(
    if(input$ind ==  "Weekly Sales"){
      hist(Walmart$Weekly_Sales, col = '#CD0000', main = "Weekly Sales")
    }else if(input$ind == "Fuel Price"){
      hist(Walmart$Fuel_Price, col = '#CD0000', main = "Fuel Price")
    }else if(input$ind == "Temperature"){
      hist(Walmart$Temperature, col = '#CD0000', main = "Temperature")
    }else if(input$ind == "CPI"){
      hist(Walmart$CPI, col = '#B22222', main = "CPI")
    }else if(input$ind == "Unemployment"){
      hist(Walmart$Unemployment, col = '#B22222', main = "Unemployment")
    }
  )
  
  
  #Checking Outliers
  output$boxplot <-renderPlot(
    if(input$out ==  "Weekly Sales"){
      boxplot(Walmart$Weekly_Sales,col="#CD6600",main = "Weekly Sales", horizontal = TRUE)
    }else if(input$out == "Fuel Price"){
      boxplot(Walmart$Fuel_Price,col = "#CD6600",main = "Fuel Price", horizontal = TRUE)
    }else if(input$out == "Temperature"){
      boxplot(Walmart$Temperature,col="#CD6600",main = "Temperature", horizontal = TRUE)
    }else if(input$out == "CPI"){
      boxplot(Walmart$CPI,col = "#CD6600",main = "CPI", horizontal = TRUE)
    }else if(input$out == "Unemployment"){
      boxplot(Walmart$Unemployment,col="#CD6600",main = "Unemployment", horizontal = TRUE)
    }
  )
  
  
  #Weekly sales by Month
  output$histogram<- renderPlot(
    if (input$filling == "Month"){
      ggplot(Walmart1, aes(Date, Weekly_Sales, fill = Month)) +  
        geom_bar(size = 0.8, stat = "summary",
                 fun.y = "sum")+ 
        theme_classic()+
        scale_x_date(date_labels = "%m/%y",date_breaks = "1 month")+
        theme(axis.text.x = element_text(
          colour = 'black', angle = 50, size = 10,
          hjust = 0.5, vjust = 0.5, face = "bold"),
          axis.title.x=element_blank(),axis.text.y = element_text(
            colour = 'black', angle = 50, size = 10,
            hjust = 0.5, vjust = 0.5, face = "bold")) +
        theme(axis.title.x = element_text(colour = "blue", face = "bold"),
              axis.title.y = element_text(colour = "blue", face = "bold"))+
        ggtitle("Monthly Sales")+ 
        ggeasy::easy_center_title()
    }else{
      ggplot(Walmart1, aes(Date, Weekly_Sales, fill = Holiday_Flag)) +  
        geom_bar(size = 0.8, stat = "summary",
                 fun.y = "sum")+ 
        theme_classic()+
        scale_x_date(date_labels = "%m/%y",date_breaks = "1 month")+
        theme(axis.text.x = element_text(
          colour = 'black', angle = 50, size = 10,
          hjust = 0.5, vjust = 0.5, face = "bold"),
          axis.title.x=element_blank(),axis.text.y = element_text(
            colour = 'black', angle = 50, size = 10,
            hjust = 0.5, vjust = 0.5, face = "bold")) +
        theme(axis.title.x = element_text(colour = "blue", face = "bold"),
              axis.title.y = element_text(colour = "blue", face = "bold"))+
        ggtitle("Monthly Sales")+ 
        ggeasy::easy_center_title()
    })
  
  
  
  # Global for Original Dataset
  datasetInput2 <- eventReactive(input$update, {
    Walmart
    
  }, ignoreNULL = FALSE)
  
  # Original Dataset
  output$ex1 <- renderTable({
    head(datasetInput2(), n = isolate(input$obs))
  })
  
  
  
  # Preprocessed Data
  datasetInput3 <- eventReactive(input$update1, {
    TrainSet
    
  }, ignoreNULL = FALSE)
  
  output$ex2 <- renderTable({
    head(datasetInput3(), n = isolate(input$obs1))
  })
  
  #summary
  
  output$ex3 <- renderPrint({
    summary(Walmart)
  })
  
  
  
  #structure
  output$ex4 <- renderPrint({
    str(Walmart)
  })
  
  
  
  # Input Data
  datasetInput <- reactive({
    
    new_val <-  data.frame(Fuel_Price = input$Fuel_Price,
                           CPI = log(input$CPI),
                           Temperature = log(as.numeric(input$Temperature)),
                           Store_Type = as.factor(input$Store_Type),
                           Week_type = as.factor(input$Week_type), 
                           Christmas = as.numeric(input$Christmas)
    )
    Output <- predict(model,new_val)
    print(paste0("Predicted Weekly Sales with 83% accuracy: ",round(exp(Output),2)))
    
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
  
  
}


shinyApp(ui = ui, server = server)