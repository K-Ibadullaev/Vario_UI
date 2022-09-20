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
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        #> Display the upload widget
        fileInput(inputId = "filedata",
                  label = "Upload data. Choose CSV or TSV file",
                  accept = c(".csv",".tsv")),
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
                               column(2,actionButton("save_model", "Save model"))
                             )
                             
                             
                             
                    ),
                    #> Tabpanel of swath plots
                    tabPanel("Swath plots",
                             plotly::plotlyOutput("swathN"),
                             plotly::plotlyOutput("swathE")),
                    #> Tabpanel of kriging
                    tabPanel(
                      "Kriging",
                      #loading
                      waiter::use_waiter(),
                      fluidRow(
                        column(4,
                               fluidRow( sliderInput("nmax","Number of nearest observations", min = 10, max = 300,value = 50)),
                               
                               fluidRow( uiOutput("nmin")),
                               # fluidRow( sliderInput("nmin","Minimal number of nearest observations", min = 10, max = 100,value = 50)),
                               
                               
                               fluidRow( sliderInput("omax","Maximum number of observations to select per octant (3D) or quadrant (2D);", min = 1, max = 100,value = 6)),
                               
                               fluidRow( uiOutput("maxdist")),
                               # fluidRow( sliderInput("maxdist","Maximal distance between 2 observations", min = 1, max = 200,value = 20)),
                               fluidRow(actionButton("kriging_btn", "Kriging", value = F))
                               
                        ),
                        column(8,plotly::plotlyOutput("krig_res"))
                        
                      )
                      
                      
                      
                      
                      
                      
                      
                      
                      
                    ),
                    #> Tabpanel of simulations
                    tabPanel(
                      "Simulations",
                      
                      #loading
                      waiter::use_waiter(),
                      fluidRow(
                        column(5,sliderInput("nsim","Select number of simulations", min = 1, 
                                             step = 1,   max = 100,value = 1)),
                        column(5,uiOutput("dispnsim")),
                        column(2,actionButton("sim_btn", "Simulate", value = F))
                      ),
                      
                      plotly::plotlyOutput("sim_res")
                      
                    )
                    
                    
                    
        )
        
        
      )
      
    )
  )
  
  
  
})