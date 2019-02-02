

library(shiny)
library(shinythemes)
library(plotly)
source("helper.R")


shinyUI(fluidPage(theme=shinytheme("readable"),
                  titlePanel(title="Basic combat dice roll simulator", windowTitle="Dice roll simulator"),
                  fluidRow(
                    
                    
                    column(3, 
                           h3("Attacking model statistics"),
                           helpText("Please input the attacking models' characteristics."))
                    
                    
                  ),
                  
                  fluidRow(
                    
                    column(3, 
                           textInput("A_models", h5("Attacking Models"), 
                                     value = "20",width='150px',placeholder="e.g. 20"))   ,
                    
                    column(3, 
                           textInput("A_attacks", h5("Attacks per model"), 
                                     value = "d3",width='150px',placeholder="number/d3/d6/2d6"))   ,
                    
                    column(3, 
                           textInput("A_hit", h5("To hit"), 
                                     value = "4",width='150px',placeholder="e.g. 4+ is 4"))   
                    
                  ),
                  
                  fluidRow(
                    column(3, 
                           textInput("A_wound", h5("To Wound"), 
                                     value = "3",width='150px',placeholder="e.g. 4+ is 4"))   ,
                    
                    column(3, 
                           textInput("A_rend", h5("Rend"), 
                                     value = "0",width='150px',placeholder="e.g. -2 is 2"))   ,
                    
                    column(3, 
                           textInput("A_damage", h5("Damage"), 
                                     value = "1",width='150px',placeholder="number/d3/d6/2d6"))   
                  ),
                  
                  fluidRow(
                    
                    column(3, 
                           h3("Defending model statistics"),
                           helpText("Please input the defending models' characteristics."))
                  ),
                  
                  fluidRow(
                    
                    column(3, 
                           textInput("D_models", h5("Defending Models"), 
                                     value = "25",width='150px',placeholder="e.g. 20"))   ,
                    column(3, 
                           textInput("D_wounds", h5("Wounds per model"), 
                                     value = "1",width='150px',placeholder="e.g. 1"))   ,
                    column(3, 
                           textInput("D_save", h5("Save characteristic"), 
                                     value = "5",width='150px',placeholder="e.g. 4+ is 4"))   
                    
                  ),                  
                  fluidRow(
                    
                    column(1,
                           submitButton("Submit Values"))
                    
                  ),
                  
                  fluidRow(
                    
                    column(3, 
                           h5("Submitting your combat"),
                           helpText("Once you have finished editing your options, please press submit.",
                                    "The app will then run 10,000 simulated dice roll events and display the results below."))
                  ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Damage Summary", verbatimTextOutput("summary")),
                  tabPanel("Damage Plot", plotlyOutput("plot"))
                  )
    )
  )
)
