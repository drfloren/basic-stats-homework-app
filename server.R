#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Ok: what is the logic here? Have a problem and solution output. Within each, decide which chapter we are on, which section within the chapter, then output the correct problem/solution. To get the correct solution, the data needs to be independent of problem or solution...

library(shiny)
library(DT)
R.utils::sourceDirectory("Problems", onError="warning", modifiedOnly=FALSE) # I needed the modifiedOnly to be false to get this to run properly...
R.utils::sourceDirectory("Solutions", onError="warning", modifiedOnly=FALSE)
R.utils::sourceDirectory("General Functions", onError="warning", modifiedOnly=FALSE)
options(width=120) #YES: this does what I want it to

# Define server logic
shinyServer(function(input, output) {
  # Setting the problem number
  probnum <- reactiveVal()
  
  observeEvent(input$manual_pnum, {
    probnum(input$manual_pnum)
  })
  
  observeEvent(input$newprob, {
    rm(.Random.seed, envir=globalenv())#reset the seed... I'm not 100% sure what was going on, but due to using this value to set the seed, there was a loop occuring among the random values (in C7, alpha=0.01, p CIs). I think certain values looped back to their beginning. This creates a truly random starting place when the random button is hit next (literally erases the previous seed so that it isn't dependent on it and can't get wrapped in a loop with it).
    npn <- sample(1:1000, size=1) #new problem number
    probnum(npn)
  })
  
  output$probnum <- renderText({
    paste("Problem Number:", probnum())
  })
  
  
  
  
  
  # Making the problem data (and situation)
  probdat <- reactive({
    # setting seed to the problem number
    set.seed(probnum())
    
    # setting default output
    stem <- "This is filler stem text and data. This implies that information for this chapter has not been loaded/generated. The data below should be the integers 1 through 20 in ascending order."
    data <- 1:20
    
    # doing specific generation for 
    if(input$ch == "c1"){
      stem <- "This is the stem for C1"
      data <- "This is the data for C1"
      
    } else if(input$ch == "c2"){
      if(input$c2catcont=="cont"){
        prob <- c2contp(n=input$c2n)
        stem <- prob$stem
        data <- hidden_data <- prob$data
      } else { #if not continuous
        prob <- c2catp(n=input$c2n)
        stem <- prob$stem
        data <- hidden_data <- prob$data
      }
      
    } else if (input$ch == "c3"){
      prob <- c3p(n=input$c3n)
      stem <- prob$stem
      data <- hidden_data <- prob$data
      
    } else if (input$ch == "c4"){
      prob <- c4p(prob_type = input$c4_prob_type) #many more problem options that could be used, but I'm not going to worry about that right now... specifically options for different contexts/mediums, as well as sample size (Which fucks up some things...)
      stem <- prob$stem
      data <- prob$data
      hidden_data <- prob$hidden_data
    } else if (input$ch == "c5"){
      
    } else if (input$ch == "c6"){
      prob <- c6p(direction = input$c6dir, prob_type=input$c6type, n=input$c6n)
      stem <- prob$stem
      data <- prob$data
      hidden_data <- prob$hidden_data
      
    } else if (input$ch == "c7"){ #need to figure out what to do for z, t, and p
      prob <- c7p(prob_type = input$c7type, n=input$c7n, alpha=input$c7alpha)
      stem <- prob$stem
      data <- prob$data
      hidden_data <- prob$hidden_data
      
    } else if (input$ch == "c8"){ #need to figure out what to do for z, t, and p
      prob <- c8p(n=input$c8n, direction=input$c8dir, alpha=input$c8alpha)
      stem <- prob$stem
      data <- prob$data
      hidden_data <- prob$hidden_data
      
    }
    
    list(stem=stem, data=data, hidden_data=hidden_data)
  })
  
  output$problem <- renderUI({
    HTML(c(probdat()$stem, #either c or paste?
               "<br/>","<br/>",
               paste(probdat()$data, collapse=", ")))
  })
  
  
  
  
  # need to split answers by chapter, so output will need to be different (and rendering will need to be different).
  # if(input$ch=="c2"){
  #   if(input$c2catcont=='cont'){
  #     output$c2fd <- renderDataTable({
  #       
  #     })
  #   } else {
  #     
  #   }
  # }
  
  output$solutiontext <- renderPrint({
    dat  <- probdat()$data
    hdat <- probdat()$hidden_data
    out <- "" #default output
    if(input$ch == "c1"){
      
    } else if(input$ch == "c2"){
      if(input$c2catcont=="cont"){
        out <- c2conts_all_text(dat, numclass = input$c2numclass)
      } else { #if not continuous
        out <- c2cats_all_text(dat)
      }
      
    } else if (input$ch == "c3"){
      out <- c3s_text(dat)
      
    } else if (input$ch == "c4"){
      out <- c4s(probdat())
      
    } else if (input$ch == "c5"){
      
    } else if (input$ch == "c6"){
      out <- c6s_text(prob_type = hdat$prob_type, hdat=hdat)
      
    } else if (input$ch == "c7"){ #need to figure out what to do for z, t, and p (currently t)
      out <- c7s_text(prob_type = hdat$prob_type, hdat=hdat)
      
    } else if (input$ch == "c8"){ #need to figure out what to do for z, t, and p (currently t)
      out <- t_test_steps(data = hdat$dat, direction = hdat$direction, alpha = hdat$alpha, nh = hdat$h0, dig=3)
    }
    out
  })
  
  output$solutionplot <- renderPlot({
    dat  <- probdat()$data
    hdat <- probdat()$hidden_data
    if(input$ch == "c1"){
      
    } else if(input$ch == "c2"){
      if(input$c2catcont=="cont"){
        out <- c2conts_all_plots(dat, numclass = input$c2numclass)
      } else { #if not continuous
        out <- c2cats_all_plots(dat)
      }
      
    } else if (input$ch == "c3"){
      out <- c3s_plot(dat, outliers = input$c3bpo)
      
    } else if (input$ch == "c4"){
      
    } else if (input$ch == "c5"){
      
    } else if (input$ch == "c6"){
      out <- c6s_plot(prob_type = hdat$prob_type, hdat=hdat)
      
    } else if (input$ch == "c7"){
      out <- c7s_plot(prob_type = hdat$prob_type, hdat=hdat)
      
    } else if (input$ch == "c8"){
      out <- cv_t_plot(direction = hdat$direction, alpha = hdat$alpha, df=length(hdat$dat) -1, tail_exp=1.1)
    }
  })
  
  
  
  
  
  
  
  # as I only want to have 1 plot output device, I'm just using this 
  height <- reactive({
    height <- 400
    if(input$ch == "c1"){
      
    } else if(input$ch == "c2"){
      if(input$c2catcont=="cont"){
        height <- 400*6 # muliply by the number of plots being output to get each one to be at 400 height. CEX is currently (and should be) set to 1 (or larger) to make sure they render alright (numbers/labels not too small, etc)...
      } else { #if not continuous
        height <- 400*3
      }
      
    } else if (input$ch == "c3"){ #don't change, as just one plot...
      
    } else if (input$ch == "c4"){
      
    } else if (input$ch == "c5"){
      
    } else if (input$ch == "c6"){
      
    } else if (input$ch == "c7"){
      
    } else if (input$ch == "c8"){
      
    }
    height
  })
  
  output$plotui <- renderUI({
    plotOutput("solutionplot", height = height())
  })
})
