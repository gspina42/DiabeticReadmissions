#REFERENCE: https://github.com/nardusstricta/soilprofile_app/blob/master/ui.R#L166

library(corrplot)
library(dplyr)
library(ggplot2)
library(markdown)
library(shiny)
library(shinydashboard)
library(png)
library(treemapify)

#Content for sidebar of the Applet
function(request) {
    sidebar <- dashboardSidebar(
        hr(),
        sidebarMenu(id="tabs",
                    menuItem("About", tabName = "about", 
                             icon = icon("info-circle"), selected=TRUE),
                    menuItem("Explore Readmission Rates", 
                             tabName="Explore_Rates", 
                             icon=icon("laptop-medical")),
                    menuItem("EDA and Visualization", 
                             tabName="EDA_and_Visualization", 
                             icon=icon("chart-pie")),
                    menuItem("Treemaps", tabName="Treemaps", 
                             icon=icon("layer-group")),
                    menuItem("Analysis", tabName = "Analysis", 
                             icon=icon("table"),
                             menuSubItem("Feature Selection", 
                                         tabName = "Feature_Selection", 
                                         icon = icon("angle-right")),
                             menuSubItem("Correlation Matrix", 
                                         tabName = "Correlation_Matrix", 
                                         icon = icon("angle-right"))
                             ),
                    menuItem("Models", tabName="Models", icon=icon("chart-bar"),
                             menuSubItem("Logistic Model", 
                                         tabName = "Regression_Model", 
                                         icon = icon("angle-right")),
                             menuSubItem("Random Forest Model", 
                                         tabName = "Random_Forest_Model", 
                                         icon = icon("angle-right")),
                             menuSubItem("Neural Network", 
                                         tabName = "Neural_Network", 
                                         icon = icon("angle-right"))
                             ),
                    menuItem("Code",  icon = icon("file-text-o"),
                             menuSubItem("ui.R", tabName = "ui", 
                                         icon = icon("angle-right")),
                             menuSubItem("server.R", tabName = "server", 
                                         icon = icon("angle-right"))
                    ),
                    menuItem("ReadMe", tabName = "readme", icon=icon("book"))
        ),
        hr(),
        #Conditional Panel for filtering on Explore Rates Tab
        conditionalPanel("input.tabs == 'Explore_Rates'",
                         fluidRow(
                             column(1),
                             column(10,
                                    h4("Filters"),
                                    hr(),
                                    checkboxGroupInput("Age","Age",
                                                       choiceNames = c("0-10","10-20","20-30","30-40","40-50","60-70","70-80","80-90","90-100"),
                                                       choiceValues = c("[0-10)","[10-20)","[20-30)","[30-40)","[40-50)","[60-70)","[70-80)","[80-90)","[90-100)"),
                                                       selected = c("[0-10)","[10-20)","[20-30)","[30-40)","[40-50)","[60-70)","[70-80)","[80-90)","[90-100)")
                                    ),
                                    checkboxGroupInput("Gender","Gender",
                                                       choiceNames = c("Male","Female"),
                                                       choiceValues = c("Male","Female"),
                                                       selected = c("Male","Female")
                                    ),
                                    checkboxGroupInput("Race","Race",
                                        choiceNames = c("African American","Asian", "Caucasian","Hispanic","Other"),
                                        choiceValues = c("AfricanAmerican","Asian", "Caucasian","Hispanic","Other"),
                                        selected = c("AfricanAmerican","Asian", "Caucasian","Hispanic","Other")
                                    ),
                                    hr()

                             )
                         )
                    )
    )
    
    #Content for body of the Applet
    body <- dashboardBody(
        tabItems(
        tabItem(tabName = "Explore_Rates",
                tags$head(
                        tags$style(
                            HTML("
                                #exploreTxt {
                                    font-size: 15px;
                                }
                                "
                            )
                        )
                    ),
                    fluidRow(
                        style = "margin-left:15px; margin-right:15px",
                        titlePanel("Explore Readmission Rates"),
                        uiOutput("exploreTxt"),
                        plotOutput("explore")
                    )),
                tabItem(tabName = "EDA_and_Visualization",
                  tags$head(
                    tags$style(
                        HTML("
                            #edaTxt1 {
                                font-size: 15px;
                            }
                            #edaTxt2 {
                                font-size: 15px;
                            }
                            #edaTxt3 {
                                font-size: 15px;
                            }
                            #edaTxt4 {
                                font-size: 15px;
                            }
                            #edaTxt5 {
                                font-size: 15px;
                            }
                            #edaTxt6 {
                                font-size: 15px;
                            }
                            
                            #edaOut1 {
                            max-height: 300px;
                            }"
                            )
                        )
                    ),
                    titlePanel("Exploratory Data Analysis"),
                    h4("(Loading may take a moment)"),
                    fluidRow(
                      style = "margin-left:15px; margin-right:15px",
                      h3("Introduction"),
                      uiOutput("edaTxt1"),
                      uiOutput("edaTitle1"),
                      verbatimTextOutput("edaOut1"),
                      br(),
                      uiOutput("edaTitle2"),
                      plotOutput("edaPlot1"),
                      br(),
                      uiOutput("edaTitle3"),
                      plotOutput("edaPlot2"),
                      br(),
                      uiOutput("edaTitle4"),
                      verbatimTextOutput("edaOut2"),
                      h3("Finding 1"),
                      uiOutput("edaTxt2"),
                      br(),
                      h3("Examining Patients Perscribed Diabetic Medications"),
                      plotOutput("edaPlot5"),
                      br(),
                      plotOutput("edaPlot6"),
                      h3("Finding 2"),
                      uiOutput("edaTxt3"),
                      br(),
                      h3("Examining Readmission Rates by Admission Type"),
                      plotOutput("edaPlot7"),
                      br(),
                      plotOutput("edaPlot8"),
                      br(),
                      h3("Finding 3"),
                      uiOutput("edaTxt4"),
                      br(),
                      h3("Examining Readmission Rates by Age"),
                      plotOutput("edaPlot9"),
                      br(),
                      plotOutput("edaPlot10"),
                      br(),
                      h3("Finding 4"),
                      uiOutput("edaTxt5"),
                      br(),
                      h3("Examining Readmission Rates by Race"),
                      plotOutput("edaPlot11"),
                      br(),
                      plotOutput("edaPlot12"),
                      h3("Finding 5"),
                      uiOutput("edaTxt6"),
                      br(),
                  )),
        
          tabItem(tabName = "Treemaps",
                  tags$head(
                      tags$style(
                          HTML("
                                #treemapTxt {
                                    font-size: 15px;
                                }
                                "
                          )
                      )
                  ),
                  fluidRow(
                    style = "margin-left:15px; margin-right:15px",
                    titlePanel("Tree Maps"),  
                    uiOutput("treemapTxt"),
                    br(),
                    selectInput(inputId = "tree_plot", 
                                label = "Select Tree Map", 
                                choices = c("Age","Gender","Medicated","Race")),
                    box(plotOutput("tree_plot"), width = NULL)
                  )),
          
        
          tabItem(tabName = "Feature_Selection",
                  fluidPage(
                      tags$head(
                          tags$style(
                              HTML("
                                  #fsTxt1 {
                                      font-size: 15px;
                                  }
                                  #fsTxt2 {
                                      font-size: 15px;
                                      text-align: center;
                                  }
                                  #fsTxt3 {
                                      font-size: 15px;
                                      text-align: center;
                                  }
                                  
                                  "
                              )
                          )
                      ),
                      titlePanel("Feature Selection"),
                      column(
                          width = 4,
                          br(),
                          box(uiOutput("fsTxt1"), width = NULL, height = '100%')
                      ),
                      column(
                          width = 4,
                          br(),
                          box(uiOutput("fsTxt2"), width = NULL, height = '100%')
                      ),
                      column(
                          width = 4,
                          br(),
                          box(uiOutput("fsTxt3"), width = NULL, height = '100%')
                      ),
                      column(
                      width = 12,
                      uiOutput("fsTxt4"),
                      imageOutput("fsImg1"),
                      uiOutput("fsTxt5"),
                      imageOutput("fsImg2")
                      )
                  )),
        
          tabItem(tabName = "Correlation_Matrix",
                  fluidRow(
                      tags$head(
                          tags$style(
                              HTML("
                                #corrTxt {
                                    font-size: 15px;
                                }
                                "
                              )
                          )
                      ),  
                    style = "margin-left:15px; margin-right:15px",  
                    titlePanel("Correlation Matrix"),
                    uiOutput("corrTxt"),
                    plotOutput("corr_plot")
                  )),
        
          tabItem(tabName = "Regression_Model",
                  fluidPage(
                      tags$head(
                          tags$style(
                              HTML("
                                #logitTxt {
                                    font-size: 15px;
                                }
                                "
                              )
                          )
                      ),
                      column(
                      width = 6,
                      br(),
                      valueBoxOutput("lo_vbox", width = 14),
                      plotOutput("lo_plot")
                      ),
                      column(
                          width = 6,
                          h3("Logistic Model"),
                          uiOutput("logitTxt")
                      )
                  )),    
          tabItem(tabName = "Random_Forest_Model",
                  fluidPage(
                      tags$head(
                          tags$style(
                              HTML("
                                #rfTxt {
                                    font-size: 15px;
                                }
                                "
                              )
                          )
                      ),
                      column(
                          width = 6,
                          br(),
                          valueBoxOutput("rf_vbox", width = 14),
                          plotOutput("rf_plot")
                      ),
                      column(
                          width = 6,
                          h3("Random Forest Model"),
                          uiOutput("rfTxt")
                      )  
                  )),
          tabItem(tabName = "Neural_Network",
                  fluidPage(
                      tags$head(
                          tags$style(
                              HTML("
                                #nnTxt {
                                    font-size: 15px;
                                }
                                "
                              )
                          )
                      ),
                      column(
                          width = 6,
                          br(),
                          valueBoxOutput("nn_vbox", width = 14),
                          plotOutput("nn_plot")
                      ),
                      column(
                          width = 6,
                          h3("Neural Network"),
                          uiOutput("nnTxt")
                      )  
                  )),
          tabItem(tabName = "about",
                  tags$head(
                      tags$style(
                          HTML("
                                #about {
                                    font-size: 15px;
                                }
                                "
                          )
                      )
                  ),  
                fluidRow(
                    style = "margin-left:15px; margin-right:15px",
                    titlePanel("Diabetic Readmission Rates"),
                    uiOutput("about")
                )),
        tabItem(tabName = "ui",
            box( width = NULL, status = "primary", solidHeader = TRUE, 
                 title="ui.R",
                 pre(includeText("ui.R"))
            )
            
            ),
        
        tabItem(tabName = "server",
            box( width = NULL, status = "primary", solidHeader = TRUE, 
                 title="server.R",
                 pre(includeText("server.R"))
            )
                
        ),
        
        tabItem(tabName = "readme",
                box( width = NULL, status = "primary", solidHeader = TRUE, 
                     title="ReadMe",
                     includeMarkdown("README.md")
                )
                
        )

        ))
    #Running the Dashboard
    dashboardPage(
        dashboardHeader(title = "Diabetic Readmissions"),
        sidebar,
        body
    )
    
    
}
