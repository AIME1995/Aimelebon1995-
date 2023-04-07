
library(ggplot2)
library(wooldridge)
data("wage1")
library(rAmCharts)
library(shiny)
library(shinydashboard)
library(shinythemes)

ui <- dashboardPage( 
    dashboardHeader(title =  "Visualisation des données" , titleWidth = 350 ), 
    dashboardSidebar( width = 350, 
         sidebarMenu(
           menuItem("Base de données", tabName = "data", icon = icon("dashboard")),
           
           
           
           menuItem("Résumé statistique", tabName = "StatistiqueDescriptive", icon = icon("bar-chart-o")),
          
           
            menuItem("Choix des variables ", tabName = "variables", icon = icon("poll")),
            menuItem("Graphe", tabName = "Histbarplot", icon = icon("poll"),
              menuSubItem(
                "Barplot", tabName = "barplot", icon = icon("poll") 
              ),
              menuSubItem(
                "Histogramme", tabName = "Histogramme", icon = icon("poll")
              )
              
            ),
           
            menuItem("regression", tabName = "regression", icon = icon("readme")),
            menuItem("Boxplot", tabName = "boxplot", icon = icon("home"))
           )
         
       ),
     dashboardBody(
       
          tabItems(
            tabItem(tabName = "data", h3("La base de données wages1"),
                    fluidRow(
                      box(background = "maroon" 
                          
                      )
                      
                    ),
                    textOutput(outputId = "resume"),
                    dataTableOutput(outputId = "tableau"
                                    
                    )),
                    
            tabItem( tabName = "StatistiqueDescriptive", h3("Analyse des données"),
                     background = "maroon",
                     
                     
                       
                         navlistPanel(
                           tabPanel(h3("summary"), verbatimTextOutput("summary")),
                           br(),
                           tabPanel(h3("Str"), 
                                    verbatimTextOutput("str")
                       )
                     ))
                     ,
            
            
            
            
            tabItem(tabName = "variables", h1("Nuaage de point"),
                    fluidRow(
                      box(  background = "red",
                            h3("Choix des variables"), 
                            selectInput(
                              inputId = "y",
                              label = "Y-axis:",
                              choices = names(wage1), 
                              selected = "wage"
                            ),
                            br(),
                            br(),
                            
                            selectInput(
                              inputId = "x",
                              label = "X-axis:",
                              choices = names(wage1),
                              selected = "educ"
                              
                            ),
                        selectInput(inputId = "color", label =  "Sélectionnez la variable couleur :", choices = names(wage1)[c(1:4)])
                      )),
                    br(),
                    br(),
                    
                    
                    plotOutput(outputId = "scatterplot"),
                    plotOutput(outputId = "densityplot", height = 200)
                    
            ),
            tabItem(tabName = "barplot", h3("Barplot"),
                    fluidRow(
                      box(background = "blue",
                          selectInput(
                            inputId = "h",
                            label = "Nom des variables",
                            choices = c( "nonwhite", "female",   "married",  "numdep",   "smsa",     "northcen",  "south" ,   "west",     "construc", "ndurman", "trcommpu", "trade",    "services", "profserv", "profocc", "clerocc"),
                            selected = "nonwhite"
                          ))),
                          
                          
                          plotOutput(outputId = "barplot")
                          
                          
                      
                    ),
            tabItem(tabName = "Histogramme", h3("Histogramme"),
                    fluidRow(
                      box(background = "blue",
                          radioButtons(
                            inputId = "w",
                            label = "Nom des variables",
                            choices = c("wage",   "educ", "exper", "tenure", "servocc",  "lwage",    "expersq",  "tenursq"),
                            selected = "wage"
                          ),
                          sliderInput("bins", label = "Nombre de classe",
                                      min = 1,
                                      max = 50,
                                      value = 30))),
                          
                          plotOutput(outputId = "histo"),
                          textOutput(outputId = "texte")
                          
                      
                    ),
            tabItem(tabName = "regression", h3("Regression linéeaire"),
                    fluidRow(
                      box( background = "blue",
                           selectInput(
                             inputId = "var_dep",
                             label = "y-axis:",
                             choices = names(wage1),
                             selected = "wage"
                           ),
                           br(),
                           br(),
                           selectInput(
                             inputId = "var_ind",
                             label = "x-axis:",
                             choices = names(wage1),
                             selected = "educ", multiple = TRUE
                           ))),
                           verbatimTextOutput(outputId = "regression"),
                           plotOutput(outputId = "plot")
                           
                      
                      
                    ),
            tabItem(tabName = "boxplot", h3("boxplot inter actif"),
                    fluidRow(
                      box(background = "blue",
                          selectInput(
                            inputId = "variable",
                            label = "Y-axis:",
                            choices = colnames(wage1),
                            selected = "wage", multiple = TRUE
                          )
                          ),
                          br(),
                          br(),
                          
                          
                          ),
                          amChartsOutput(outputId = "boxplott", width = "100%", height = "500px"),
                          textOutput(outputId = "boxplot")
                      )
                    )
            ),
    skin =  "blue"
            
          )
        
        
        
server <- function(input, output)
{ 
  output$resume <- renderText({
    paste("  Rappel : vous travaillez sur la dataframe Wage1 du package Wooldridge")
  })
  

  output$tableau <- renderDataTable({
   print(wage1)
    
            
  })
  output$summary <- renderPrint( {
    wage1
    summary(wage1)
  })
  output$str <- renderPrint({
    wage1
    str(wage1)
  })
  
  
  output$scatterplot <- renderPlot( {
    
    ggplot(wage1, aes_string(x = input$x, y = input$y, color = input$color)) +  
      geom_point() + labs(title = "Nuage de points interactif",
                                             x = input$x,
                                             y = input$y,
                                             color = input$color)  + theme_classic()
      
  })
  
  output$densityplot <- renderPlot({
    ggplot(data = wage1, aes_string(x = input$x)) +
      geom_density()
  })
  
  output$barplot <- renderPlot({
      h <- table(wage1[, input$h])
      bins <- seq(min(h), max(h))
      barplot(h, breaks = bins, border = "black", main = input$h)
     
    
  })
  output$histo <- renderPlot({
    w <- wage1[, input$w]
    bins <- seq(min(w), max(w), length.out = input$bins + 1)
    hist(w, breaks = bins, border = "black", main = paste("Histogramme de ", input$w )  , coul = "blue")
    
    
  })
  
  output$texte <- renderText({
    paste("Nombre de classe : ", input$bins)
  })
  
  
  model <- reactive({
    formulaa <- paste(input$var_dep, "~",  paste(input$var_ind, collapse = "+"))
  lm(formulaa, data = wage1)
  })
  output$regression <- renderPrint( {
    summary(model())
   
    
  } )
  output$plot <-  renderPlot( {
    plot(model())
  }
    )
  
  output$boxplott <- renderAmCharts({
    amBoxplot(wage1[, input$variable]) 
  })
  renderText({
    paste("le Boxplot de la variable ", input$variable)
  })
  options(rsconnect.max.bundle.size = 5000000000)
    }
    


shinyApp(ui, server)


 
 