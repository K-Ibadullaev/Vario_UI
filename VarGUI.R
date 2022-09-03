library(shiny)
library(gmGeostats)
source("spSupport.R")
library(magrittr) 
library(dplyr)
library(gstat)
library(scico)
library(ggplot2)
library(plotly)
library(rlang)


VarGUI = shinyUI({
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        #> Display the upload widget
        fileInput(inputId = "filedata",
                  label = "Upload data. Choose CSV file",
                  accept = c(".csv")),
        #> Display input for nested structures
        numericInput("num", "Number of structures", value = 1, min = 1, max = 3),
        
        #> Display dynamic ui of spatial coordinates
        uiOutput("Xcoords"),
        uiOutput("Ycoords"),
        
        #> Define widgets or variable of interest, e.g numeric/categorical
        fluidRow(
          column(4,uiOutput("var_chem")),
          column(4,uiOutput("indicator_box")), 
          column(4,uiOutput("indicator_list"))),
        
        #> Display cutoff of the variogram
        uiOutput("cutoff"),
        
        #> Display width/lag
        sliderInput("width","Width", min = 1, max = 200,value = 11.25),
        
        #> Display nugget 
        sliderInput("nugget","Nugget", min = 0,step = 0.01, max = 1000,value = 225),
        
        
      ),
      mainPanel(
        
        tabsetPanel(id="Mainpanel",
                    #> Display Data
                    tabPanel("Data set",
                             DT::DTOutput('datatabshow')
                             
                    ),
                    #> Display Variogram
                    tabPanel("Variogram plot",
                             
                             fluidRow( 
                               column(2,uiOutput("dynamic_models")),
                               column(3,uiOutput("dynamic_ranges")),
                               column(3,uiOutput("dynamic_sills")),
                               column(2,uiOutput("dynamic_alpha")),
                               column(2,uiOutput("dynamic_ratio"))
                             ),
                             plotly::plotlyOutput("varioplot"),
                             fluidRow(
                               column(2,actionButton("save_model", "Save model")),)
                             
                             
                             
                    ),
                    #> Tabpanel of swath plots
                    tabPanel("Swath plots",
                             plotly::plotlyOutput("swathN"),
                             plotly::plotlyOutput("swathE")),
                    #> Tabpanel of kriging
                    tabPanel(
                      "Kriging", 
                      # uiOutput("kriging_box")
                      actionButton("kriging_btn", "Kriging", value = F),
                      plotly::plotlyOutput("krig_res")
                      
                    )
                    
                    
                    
        )
        
        
      )
      
    )
  )
  
  
})