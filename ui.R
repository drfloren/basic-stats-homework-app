#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("MTH 115 Homework"),
  
  # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Problem numbers
       h3(textOutput("probnum")),
       h5("Below, you may generate a random new problem, or go to a specific problem."),
       actionButton("newprob", "Random New Problem"),
       numericInput("manual_pnum", "Go to this problem:", value = 1),
       
       # Chapter Selection
       selectInput("ch",
                   "Which chapter?",
                   choices = list(#"Chapter 1" = "c1",
                                  "Chapter 2" = "c2", 
                                  "Chapter 3" = "c3"#,
                                  #"Chapter 4" = "c4", 
                                  #"Chapter 5" = "c5",
                                  #"Chapter 6" = "c6",
                                  #"Chapter 7" = "c7",
                                  #"Chapter 8" = "c8"
                                  ),
                  selected="c2"),
       
       # Chapter Options
       conditionalPanel(
         condition = "input.ch == 'c2'",
         h3("Chapter 2 Options"),
         selectInput("c2catcont", "Categorical or Continuous?",
                     list("Categorical" = "cat", 
                          "Continuous" = "cont"), selected = "cont"),
         sliderInput("c2n", "Sample Size", min=9, max=30, value = 15),
         sliderInput("c2numclass", "Number of Classes (for continuous)", min=5, max=10, value=7)
       ),
       
       conditionalPanel(
         condition = "input.ch == 'c3'",
         h3("Chapter 3 Options"),
         sliderInput("c3n", "Sample Size", min=7, max=20, value = 15),
         checkboxInput("c3bpo", "Plot Outliers in Boxplot?", value = FALSE)
       )
       
       #conditionalPanel().... #for the rest of the chapters' options
    ),
    

    
    # Show a plot of the generated distribution
    mainPanel(
       tabsetPanel(type="tabs",
                   tabPanel("Problem", 
                            htmlOutput("problem")),
                   tabPanel("Solutions", 
                            verbatimTextOutput("solutiontext"),
                            uiOutput("plotui")))
    )
  )
))
