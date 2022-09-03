library(miniUI)
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





varparams = function(vgtpm){
## UI of the APP----------

 ui = miniPage(
   
   gadgetTitleBar("Variogram builder gadget"),
   
   miniContentPanel(
     
    
       
     fillRow(flex = c(1,5),
       fillCol(
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
         
         fillRow(flex = c(2,1,2),
           uiOutput("var_chem"),
           uiOutput("indicator_box"), 
           uiOutput("indicator_list")
         ),
         
         #> Display cutoff of the variogram
         uiOutput("cutoff"),
         
         #> Display width/lag
         sliderInput("width","Width", min = 1, max = 200,value = 11.25),
         
         #> Display nugget 
         sliderInput("nugget","Nugget", min = 0,step = 0.01, max = 1000,value = 225)
         
       ),
       
       
       miniContentPanel(
       miniTabstripPanel(id="Mainpanel",
                         #> Display Data
                         miniTabPanel("Data set",
                                      DT::DTOutput('datatabshow')
                                      
                         ),
                         #> Display Variogram
                         miniTabPanel("Variogram plot",
                                      
                                      plotly::plotlyOutput("varioplot"),
                                          
                                          fillRow( 
                                          uiOutput("dynamic_models"),
                                          uiOutput("dynamic_ranges"),
                                          uiOutput("dynamic_sills"),
                                          uiOutput("dynamic_alpha"),
                                          uiOutput("dynamic_ratio")
                                        )
                                       
                                       
                                      
                                      ) 
                                      
                                      
                                      
                                      
                         ,
                         #> Tabpanel of swath plots
                         miniTabPanel("Swath plots",
                                      fillCol(flex=c(1,1),
                                              plotly::plotlyOutput("swathN"),
                                              plotly::plotlyOutput("swathE"))
                         ),                        
                         #> Tabpanel of kriging
                         miniTabPanel(
                           "Kriging", 
                           # uiOutput("kriging_box")
                           # actionButton("kriging_btn", "Kriging", value = F),
                           plotly::plotlyOutput("krig_res")
                           
                         )
                         
                         
       )
       
     )
     )
     
    
   )
   )
 
   
  
  
  
  
  
  
  
# #> Define widgets
# ui = miniPage(
#   
#   gadgetTitleBar("Shiny gadget example"),
#   miniContentPanel(
#   
#   fillRow(
#       fillCol(
#         #> Display the upload widget
#         fileInput(inputId = "filedata",
#                   label = "Upload data. Choose CSV file",
#                   accept = c(".csv")),
#         #> Display input for nested structures
#         numericInput("num", "Number of structures", value = 1, min = 1, max = 3),
#         
#         #> Display dynamic ui of spatial coordinates
#         uiOutput("Xcoords"),
#         uiOutput("Ycoords"),
#         
#         #> Define widgets or variable of interest, e.g numeric/categorical
#         
#         uiOutput("var_chem"),
#         uiOutput("indicator_box"), 
#         uiOutput("indicator_list"),
#         
#         #> Display cutoff of the variogram
#         uiOutput("cutoff"),
#         
#         #> Display width/lag
#         sliderInput("width","Width", min = 1, max = 200,value = 11.25),
#         
#         #> Display nugget 
#         sliderInput("nugget","Nugget", min = 0,step = 0.01, max = 1000,value = 225),
#         
#         
#         
#         
#       )
#       
#     ,
#       
#       
#       miniTabstripPanel(id="Mainpanel",
#                         #> Display Data
#                         miniTabPanel("Data set",
#                                      DT::DTOutput('datatabshow')
#                                      
#                         ),
#                         #> Display Variogram
#                         miniTabPanel("Variogram plot",
#                                      fillRow(
#                                        fillCol(
#                                        
#                                        uiOutput("dynamic_models"),
#                                        uiOutput("dynamic_ranges"),
#                                        uiOutput("dynamic_sills"),
#                                        uiOutput("dynamic_alpha"),
#                                        uiOutput("dynamic_ratio")
#                                      ),
#                                      plotly::plotlyOutput("varioplot")
#                                      ) 
#                                      
#                                      
#                                      
#                                      
#                         ),
#                         #> Tabpanel of swath plots
#                         miniTabPanel("Swath plots",
#                                      fillCol(plotly::plotlyOutput("swathN"),
#                                              plotly::plotlyOutput("swathE"))
#                         ),                        
#                         #> Tabpanel of kriging
#                         miniTabPanel(
#                           "Kriging", 
#                           # uiOutput("kriging_box")
#                           # actionButton("kriging_btn", "Kriging", value = F),
#                           plotly::plotlyOutput("krig_res")
#                           
#                         )
#                         
#                         
#                         
#       )
#     )
#     
#     
#       
#       
#      ) 
#     
#   
# )
# 


## Server of the APP ------------
server <- function(input, output,session)
{
  ### dynamic data frame--------------
  datas <- reactive({
    req(input$filedata)
    read.csv(input$filedata$datapath)
  })
  
  ### indicator box-----
  output$indicator_box <- renderUI({
    checkboxInput("indicator_box", "Indicator", value = F)
  })
  
  ### indicator list-----
  output$indicator_list <- renderUI({
    req(input$indicator_box,input$var_chem)
    selectInput("indicator_list", "Indicator variable", choices = selected_data() %>% select(input$var_chem)%>% unique) 
  })
  
  ### kriging -----
  # output$kriging_box <- renderUI({
  #   checkboxInput("kriging_box", "Kriging", value = F)
  # })
  # 
  
  ### coordinates------
  output$Xcoords <- renderUI({
    selectInput("Xcoords", "Easting",  choices = colnames(datas()))
  }) 
  output$Ycoords <- renderUI({
    selectInput("Ycoords", "Northing",  choices = colnames(datas()))
  }) 
  
  ### variable of interest--------
  output$var_chem <- renderUI({
    selectInput("var_chem", "Variable of interest",  choices = colnames(datas()))
  }) 
  
  ### subset the data w.r.t variable and coordinates------  
  selected_data = reactive({
    datas() %>% select(input$Xcoords,input$Ycoords,input$var_chem)
  })   
  
  
  ### variogram cutoff---------
  output$cutoff<-renderUI({
    req(selected_data())
    sliderInput("cutoff","Cutoff", min = 1, 
                max = max(as.matrix(dist(selected_data()%>%select(input$Xcoords,input$Ycoords))))/2,value = 225)
  })
  
  
  ### dynamic models-----------
  output$dynamic_models <- renderUI({
    #> creates input widgets depending on given number of nested structures
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      selectInput(inputId = paste0("model",i),label=paste("Model",i),
                  choices = c("Exponential"="Exp","Wave"="Wav","Gaussian"="Gau","Spherical"= "Sph"))
      
    })
  })
  
  
  ### dynamic ranges-----------
  output$dynamic_ranges <- renderUI({
    #> creates input widgets depending on given number of nested structures 
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      sliderInput(inputId = paste0("range",i),label=paste("Range",i),
                  min = 0, step = 0.01,
                  max = max(as.matrix(dist(selected_data()%>%select(input$Xcoords,input$Ycoords))))/2,value = 225) 
      
      
    })
  })
  ### dynamic sills-----
  output$dynamic_sills <- renderUI({
    #> creates input widgets depending on given number of nested structures
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      sliderInput(inputId = paste0("sill",i),label=paste("Sill",i),
                  min = 0,
                  step = ifelse(!input$indicator_box,
                                var(selected_data()%>%select(input$var_chem))*1.5*0.001,
                                0.001),
                  max = ifelse(!input$indicator_box,
                               var(selected_data()%>%select(input$var_chem))*3,
                               0.25) ,
                  value = ifelse(!input$indicator_box,
                                 var(selected_data()%>%select(input$var_chem))*1.5*0.01,
                                 0.25)
                  
      )
      
    })
  })
  
  ### dynamic anisotropy-----------
  output$dynamic_alpha <- renderUI({
    #> creates input widgets depending on given number of nested structures
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      sliderInput(inputId = paste0("alpha",i),label=paste("Anisotropy angle",i), min = 0, max = 270,value=0)
      
    })
  })
  
  output$dynamic_ratio <- renderUI({
    #> creates input widgets depending on given number of nested structures
    num <- as.integer(input$num)
    purrr::map(1:num,function(i) {
      
      sliderInput(inputId = paste0("ratio",i),label=paste("Ratio",i), min = 0, max = 1,value = 1)
    })
  })
  
  
  
  
  
  ###  empirical variogram ------------
  #> creates empricial variogram for the vaiable of interest
  gs = reactive({
    #> for categorical variable
    if(input$indicator_box){
      ddt = selected_data() 
      frm = as.formula(paste( as.character(input$var_chem), "=='", as.character(input$indicator_list), "'~1", sep=""))
      location_frm = as.formula(paste( "~X+Y", sep=""))
      
      gstat(id=input$indicator_list, formula=frm, locations = location_frm , data=ddt)
    }else{
      #> for numeric variable
      frm = as.formula(paste("log(", input$var_chem, ")~1", sep=""))
      d =selected_data() 
      location_frm = as.formula(paste( "~X+Y", sep=""))
      gstat(id=input$var_chem, formula=frm, locations = location_frm , data=d)
    }
    
    
    
  }) 
  
  
  
  
  ### configured empirical variogram ---------
  vg = reactive(variogram(gs(), cutoff=input$cutoff, width=input$width) )
  
  
  ### theoretical variogram--------
  
  
  vgt= reactive( {
    
    
    res = vgm(model=input$model1, nugget=input$nugget, range=input$range1, psill=input$sill1,
              anis = c(input$alpha1, input$ratio1))
    if(input$num>1){
      #> evaluates variogram depending on given number of nested structures
      for (i in 2:as.integer(input$num)) {
        res = vgm(model=input[[paste0("model",i)]], range=input[[paste0("range",i)]], psill=input[[paste0("sill",i)]],
                  anis = c(input[[paste0("alpha",i)]], input[[paste0("ratio",i)]]),add.to=res)
        
      }
      
      
    }
    res
    
    
  })
  
  ### kriging----------
  # mn= eventReactive(input$kriging_box,
  #     #> for categorical variable
  #     if(input$indicator_box){
  #      
  #       frm = as.formula(paste( as.character(input$var_chem), "=='", as.character(input$indicator_list), "'~1", sep=""))
  #       location_frm = as.formula(paste("~X+Y", sep=""))
  #       
  #       gs_krig  =  gstat(id=input$var_chem, formula=frm, locations = location_frm ,
  #             data=selected_data(), model=vgt )
  #     }else{
  #       #> for numeric variable
  #       frm = as.formula(paste("log(", input$var_chem, ")~1", sep=""))
  #     
  #       location_frm = as.formula(paste( "~X+Y", sep=""))
  #       gs_krig  = gstat(id=input$var_chem, formula=frm, locations = location_frm ,
  #             data=selected_data(), model=vgt )
  #     }
  #     
  #     
  #     
  #   ) 
  
  
  
  
  # rangesXY = reactive({selected_data() %>% select(input$Xcoords,input$Ycoords) %>% sapply(range)}) 
  # x = reactive({seq(from=rangesXY[1,input$Xcoords]-20, to=rangesXY[2,input$Xcoords]+20, by=10)})
  # y = reactive({seq(from=rangesXY[1,input$Ycoords]-20, to=rangesXY[2,input$Ycoords]+20, by=10)})
  # 
  # xy_grid = reactive({expand.grid(X=x, Y=y)})
  # 
  # kriged = reactive(gs_krig, newdata=xy_grid, debug.level=-1)
  # 
  # 
  
  
  # res_krg = eventReactive(input$kriging_btn,{
  #                 #> set the grid
  #                 
  #                   rangesXY = selected_data() %>% select(input$Xcoords,input$Ycoords) %>% sapply(range)
  #                   x = seq(from=rangesXY[1,input$Xcoords]-20, to=rangesXY[2,input$Xcoords]+20, by=10)
  #                   y = seq(from=rangesXY[1,input$Ycoords]-20, to=rangesXY[2,input$Ycoords]+20, by=10)
  #                   
  #                 xy_grid = expand.grid(X=x, Y=y)
  #                 
  #                 #cross-validation
  #                 xv = gstat.cv(vg(), nfold=nrow(selected_data()))
  #               
  #                
  #                   
  #                     
  #                     gs_krig = gstat(id=input$var_chem, formula=frm, locations = location_frm , data=selected_data(), model=vgt ,
  #                                    # neighbourhood parameters: see ?gstat help
  #                                    nmax = 20,    # maximum number of datapoints => increase if variogram has oscillations
  #                                    omax = 6,    # maximum nr of datapoints per octant/quadrants
  #                                    nmin = 10,    # minimum nr of datapoints (otherwise return NA)
  #                                    maxdist = 20, # maximum distance to seek for datapoints
  #                                    force = TRUE  # ignore maxdist if nmin is not satisfied?
  #                     )
  #                     
  #                     
  #                   kriged = predict(gs_krig, newdata=xy_grid, debug.level=-1) 
  #                 
  #                 
  #               })
  
  
  # kplot=ggplot(kriged, aes(X, Y))+
  #   geom_raster(aes(fill=Co.pred))+
  #   coord_fixed()+
  #   theme_bw()+
  #   ggtitle("Results of local kriging for Log(Co)")+
  #   scico::scale_fill_scico(palette = "roma",direction = -1)
  # 
  # 
  
  
  
  
  
  ### render data frame ------------
  output$datatabshow <- DT::renderDT({    
    selected_data()
    
    
  })
  
  
  
  
  ### plotting ---------
  # plot empirical variogram and  model
  output$varioplot <-  plotly::renderPlotly({
    
    ggplot() +
      ggtitle(paste0("Variogram of ", input$var_chem))+
      
      # theme_classic()+
      theme_light()+ 
      geom_point(data=vg(),aes(dist, gamma, colour = 2)) +
      geom_line(data=variogramLine(object=vgt(),maxdist = input$cutoff),aes(dist, gamma, colour = 2))+
      labs(y = "semivariogram",x="distance")+
      theme(legend.position = "none")
  })
  
  
  #> Swath plots
  #> Plot spatial dependency of the variable w.r.t direction North/East
  
  output$swathN = plotly::renderPlotly(
    {
      if (!input$indicator_box) {
        vrbl = paste0("log(",input$var_chem,")")
        ggplot(selected_data()) +
          geom_point(aes_string(x="Y", y=vrbl )) +
          theme_bw() +
          ggtitle(paste0(" Log(",input$var_chem,")", " vs Y"))+
          geom_smooth(aes_string(x="Y", y=vrbl ), method=loess, col=2)
      }
      
      
    }
    
    
    
  )
  
  output$swathE = plotly::renderPlotly(
    {
      if (!input$indicator_box) {
        vrbl = paste0("log(",input$var_chem,")")
        ggplot(selected_data()) +
          geom_point(aes_string(x="X", y=vrbl )) +
          theme_bw() +
          ggtitle(paste0(" Log(",input$var_chem,")", " vs X") )+
          geom_smooth(aes_string(x="X", y=vrbl ), method=loess, col=2)}
    }
  )
  
  #> Kriging visualization
  output$krig_res = plotly::renderPlotly({
    # ggplot(kriged, aes(x="X",y="Y"))+
    #     geom_raster(aes(fill=paste0(input$varchem,".pred")))+
    #     coord_fixed()+
    #     theme_bw()+
    #     ggtitle(paste0("Results of kriging for Log(", input$varchem,")" ))+
    #     scico::scale_fill_scico(palette = "roma",direction = -1)
    
    
  })
  
  
  
  
  # Handle the Done button being pressed.
  observeEvent(input$done, {
    res = structure(vgt()) 
    stopApp(res)
  })
  
  
} 


  
  runGadget(ui , server , viewer = browserViewer())
}

varparams()



  
  
  
