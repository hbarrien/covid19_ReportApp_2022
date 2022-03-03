# server.R
#
# AUTHOR
# Herbert Barrientos
# hpbarr@gmail.com
#
# CREATION DATE
# 2020-04-12
#
# VERSION
# 1
#
# PROJECT NAME
# COVID-19 Open Research Dataset Challenge (CORD-19)
# https://www.kaggle.com/allen-institute-for-ai/CORD-19-research-challenge/tasks
#
# DESCRIPTION
# Back end functionality for a ShinyApp application. Uses ReportArticles.R.


# ################################
# ########## LIBRARIES ###########
# ################################
library(data.table)
library(shiny)
library(DT)

source(file="ReportArticles.R")


# ################################
# ########## CONSTANTS ###########
# ################################
NO_INPUT        <- "Please enter a question..."
BAD_INPUT_TXT   <- "Bad input: please enter a single keyword or a sequence of keywords separated by blank spaces."
BAD_INPUT_REGEX <- "Bad input: please check your regular expression."

PROGRESS_MSG_GENERAL <- "Please wait..."
PROGRESS_MSG_DETAIL  <- ""

MSG_OUT_OF_MEMORY <- "Your question produces a large result dataset that causes memory problems. Please try another question."


# ################################
# ########## VARIABLES ###########
# ################################
lastQuestion     <- EMPTY_STRING
lastQuestionType <- -1


# ################################
# ########## FUNCTIONS ###########
# ################################
updateQuestionControlValues <- function(input) {
  
  lastQuestion     <<- input$question
  lastQuestionType <<- as.integer(input$radio)
  
}  # END updateQuestionControlValues

getErrorMessage <- function(error) {
  
  if (ERROR_OUT_OF_MEMORY == ERROR_OUT_OF_MEMORY)
    return(MSG_OUT_OF_MEMORY)
  
}  # END getErrorMessage

reportErrorMessage <- function(input, output, msg) {
  
  output$yourQuestion <- renderPrint({msg})
  updateQuestionControlValues(input)
  
  output$outDataTable = DT::renderDataTable({NULL}, escape = FALSE)
  
}  # END reportErrorMessage

shinyServer(
  
  function(input, output) {
    
    observeEvent(input$goButton, {
      
      # Check if nothing was entered
      if (input$question == EMPTY_STRING) {
        
        reportErrorMessage(input, output, NO_INPUT)
        return()
        
      }  # END if
      
      # Validate question based on its type. Report errors if necessary
      inputRadio <- as.integer(input$radio)
      question <- validateInput(input$question, inputRadio)
      
      if (question == EMPTY_STRING) {
        
        badInput <- EMPTY_STRING
        
        if (inputRadio == INPUT_TEXT)
          badInput <- BAD_INPUT_TXT
        
        if (inputRadio == INPUT_REGEX)
          badInput <- BAD_INPUT_REGEX
        
        reportErrorMessage(input, output, badInput)
        return()
        
      }  # END if
      
      # If the question, or the question type, remain unchanged, do nothing
      if ((lastQuestion == input$question) && (lastQuestionType == inputRadio))
        return()
      
      # Report the new question and update lastQuestion and lastQuestionType
      # Workaround: set a character variable with input$question, in order to 
      # keep the current text static until the action button is clicked again
      yourQuestion <- as.character(input$question)
      output$yourQuestion  <- renderPrint({yourQuestion})
      
      updateQuestionControlValues(input)
      
      # Clear the table display before processing the new question
      output$outDataTable = DT::renderDataTable({NULL}, escape = FALSE)
      
      outDataTable <- NULL; 
      gc();

      withProgress(message = PROGRESS_MSG_GENERAL, detail = PROGRESS_MSG_DETAIL, min = 0, max = 10, value = 0, 
                   {
                     incProgress(2)
                     
                     # Process the inquiry
                     outDataTable <- findArticles(question)
                     
                     if (!is.data.frame(outDataTable)) {
                       
                       reportErrorMessage(input, output, getErrorMessage(outDataTable))
                       outDataTable <- NULL
                       
                     } else {
                       
                       incProgress(5)
                       
                       if (nrow(outDataTable) > 0)
                         outDataTable <- formatResults(outDataTable)
                       
                       incProgress(3)
                       
                     }  # END if
                     
                   })
      
      # Display results
      if (!is.null(outDataTable))
        output$outDataTable = DT::renderDataTable({outDataTable}, escape = FALSE)
      
    })  # END observeEvent
  
  }  # END function
  
)  # END shinyServer
