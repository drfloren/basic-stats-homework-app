#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(xtable) #to display table for c4 empirical
R.utils::sourceDirectory("Problems", onError="warning", modifiedOnly=FALSE)
R.utils::sourceDirectory("Solutions", onError="warning", modifiedOnly=FALSE)
R.utils::sourceDirectory("General Functions", onError="warning", modifiedOnly=FALSE)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Basic Stats Homework App"),
  
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
                                  "Chapter 3" = "c3",
                                  "Chapter 4" = "c4", 
                                  #"Chapter 5" = "c5",
                                  "Chapter 6" = "c6",
                                  "Chapter 7" = "c7",
                                  "Chapter 8" = "c8"
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
       ),
       
       conditionalPanel(
         condition = "input.ch == 'c4'",
         h3("Chapter 4 Options"),
         # sliderInput("c4n", "Sample Size (when appropriate)", min=2, max=4, value = 3), #maybe make the value null by default? Meh, this option just doesn't really work well with these problems......
        selectInput("c4_prob_type", "Type of Question", list("Random"="random",
                                                             "Basic Probability" = "basic_prob",
                                                             "Basic Complement" = "basic_comp",
                                                             "Advanced Complement" = "adv_comp",
                                                             "Or (with Mutually Exclusive)" = "or_wme",
                                                             "Or (without Mutually Exclusive)" = "or_wome",
                                                             "Conditional Probability" = "cond",
                                                             "And (with Replacement)" = "and_wr",
                                                             "And (without Replacement)" = "and_wor",
                                                             "Fundamental Counting Rule" = "fcr",
                                                             "Ways to Arrange" = "arrange",
                                                             "Permutations" = "perm",
                                                             "Combinations" = "combn"))
        # selectInput("c4card_opts", "Playing Card Options", list("Random" = "random",
        #                                                         "Card Values" = "value",
        #                                                         "Card Suits" = "suit"))
        # selectInput("c4context_opts", "Problem Contexts", list("Random" = "random",
        #                                                         "Card Values" = "value",
        #                                                         "Card Suits" = "suit"))# lists are different for every problem context/medium, AND (at least within the other group) are different than EACH OTHER, and I don't want to go through each... I guess I could make a second conditional panel under this to display when the particular thing is selected in select input, then make a different options variable for each one (just in case). But, I'm not going to right now...
        
       ),
       
       conditionalPanel(
         condition = "input.ch == 'c6'",
         h3("Chapter 6 Options"),
         sliderInput("c6n", "Sample Size (when appropriate)", min=7, max=20, value = 15),
         selectInput("c6dir", "Direction of Question", list("Random" = "random",
                                                               "Greater Than" = "greater than",
                                                               "Less Than" = "less than")), 
         selectInput("c6type", "Type of Question", list("Random"="random",
                                                        "Z to Probability (basic)" = "z2p",
                                                        "Z to Probability (word)" = "z2p_word",
                                                        "Probability to Value" = "p2v",
                                                        "Z to Probability (sample/clt)" = "z2p_samp"))
       ),
       
       conditionalPanel(
         condition = "input.ch == 'c7'",
         h3("Chapter 7 Options"),
         sliderInput("c7n", "Sample Size", min=7, max=20, value = 15),
         selectInput("c7alpha", "Alpha Level", list("Random" = "random",
                                                    ".01"=.01,
                                                    ".05"=.05,
                                                    ".10"=.10,
                                                    ".20"=.20)), 
         selectInput("c7type", "Type of Question", list("Random" = "random",
                                                        "Z CI" = "z",
                                                        "T CI" = "t",
                                                        "P CI" = "p"))
       ),
       
       conditionalPanel(
         condition = "input.ch == 'c8'",
         h3("Chapter 8 Options"),
         sliderInput("c8n", "Sample Size", min=7, max=20, value = 15),
         selectInput("c8dir", "Direction of Alternative", list("Random" = "random",
                                                        "Greater Than" = "greater than",
                                                        "Less Than" = "less than",
                                                        "Not Equal To" = "not equal to")),
         selectInput("c8alpha", "Alpha Level", list("Random" = "random",
                                                    ".01"=.01,
                                                    ".05"=.05,
                                                    ".10"=.10))
       )
       
       #conditionalPanel().... #for the rest of the chapters' options
    ),
    

    
    # Show a plot of the generated distribution
    mainPanel(
       tabsetPanel(type="tabs",
                   tabPanel("Problem", 
                            uiOutput("problem")),
                   tabPanel("Solutions",
                            verbatimTextOutput("solutiontext"),
                            tags$head(tags$style("#solutiontext{min-width: 800px}")), #lots of HTML options available. The hashtag tells it what to apply these options to. This min width makes sure that the size of the output is at least what is needed to display the table nicely (at least for now). The width option in the server file, OUTSIDE of the render function, is what sets where the R code wraps in the first place (this was a pain and wasn't working when inside of the solutiontext render function).
                            uiOutput("plotui")))
    )
  )
))
