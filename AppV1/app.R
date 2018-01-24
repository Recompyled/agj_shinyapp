#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.tree)

root <- Node$new("Root")
n <- root
  nn <- n$AddChild("Technique 1")
  nn$tag <- 'T1'
  
    nnn <- nn$AddChild("Method 1.1")
    nnn$tag <- ("M1")
    
      nnnn <- nnn$AddChild("Analyte 1.1.1")
      nnnn$tag <- ("A1")
      
      nnnn <- nnn$AddChild("Analyte 1.1.2")
      nnnn$tag <- ("A2")
      
      nnnn <- nnn$AddChild("Analyte 1.1.3")
      nnnn$tag <- ("A3")
      
    nnn <- nn$AddChild("Method 1.2")
    nnn$tag <- ("M2")
    
      nnnn <- nnn$AddChild("Analyte 1.2.1")
      nnnn$tag <- ("A1")
      
      nnnn <- nnn$AddChild("Analyte 1.2.2")
      nnnn$tag <- ("A2")
      
      nnnn <- nnn$AddChild("Analyte 1.2.3")
      nnnn$tag <- ("A3")
      
    nnn <- nn$AddChild("Method 1.3")
    nnn$tag <- ("M3")
    
      nnnn <- nnn$AddChild("Analyte 1.3.1")
      nnnn$tag <- ("A1")
      
      nnnn <- nnn$AddChild("Analyte 1.3.2")
      nnnn$tag <- ("A2")
      
      nnnn <- nnn$AddChild("Analyte 1.3.3")
      nnnn$tag <- ("A3")
      

  nn <- n$AddChild("Technique 2")
  
  nn$tag <- ("T1")
  
    nnn <- nn$AddChild("Method 2.1")
    nnn$tag <- ("M1")
    
      nnnn <- nnn$AddChild("Analyte 2.1.1")
      nnnn$tag <- ("A1")
      
      nnnn <- nnn$AddChild("Analyte 2.1.2")
      nnnn$tag <- ("A2")
      
      nnnn <- nnn$AddChild("Analyte 2.1.3")
      nnnn$tag <- ("A3")
      
    nnn <- nn$AddChild("Method 2.2")
    nnn$tag <- ("M2")
    
      nnnn <- nnn$AddChild("Analyte 2.2.1")
      nnnn$tag <- ("A1")
      
      nnnn <- nnn$AddChild("Analyte 2.2.2")
      nnnn$tag <- ("A2")
      
      nnnn <- nnn$AddChild("Analyte 2.2.3")
      nnnn$tag <- ("A3")
      
    nnn <- nn$AddChild("Method 2.3")
    nnn$tag <- ("M3")
    
      nnnn <- nnn$AddChild("Analyte 2.3.1")
      nnnn$tag <- ("A1")
      
      nnnn <- nnn$AddChild("Analyte 2.3.2")
      nnnn$tag <- ("A2")
      
      nnnn <- nnn$AddChild("Analyte 2.3.3")
      nnnn$tag <- ("A3")
      
    
  nn <- n$AddChild("Technique 3")
  
    nnn <- nn$AddChild("Method 3.1")
    nnn$tag <- ("M1")
    
      nnnn <- nnn$AddChild("Analyte 3.1.1")
      nnnn$tag <- ("A1")
      
      nnnn <- nnn$AddChild("Analyte 3.1.2")
      nnnn$tag <- ("A2")
      
      nnnn <- nnn$AddChild("Analyte 3.1.3")
      nnnn$tag <- ("A3")
      
    nnn <- nn$AddChild("Method 3.2")
    nnn$tag <- ("M2")
    
      nnnn <- nnn$AddChild("Analyte 3.2.1")
      nnnn$tag <- ("A1")
      
      nnnn <- nnn$AddChild("Analyte 3.2.2")
      nnnn$tag <- ("A2")
      
      nnnn <- nnn$AddChild("Analyte 3.2.3")
      nnnn$tag <- ("A3")
      
    nnn <- nn$AddChild("Method 3.3")
    nnn$tag <- ("M3")
    
      nnnn <- nnn$AddChild("Analyte 3.3.1")
      nnnn$tag <- ("A1")
      
      nnnn <- nnn$AddChild("Analyte 3.3.2")
      nnnn$tag <- ("A2")
      
      nnnn <- nnn$AddChild("Analyte 3.3.3")
      nnnn$tag <- ("A3")
      
projects = c('', 'Proj One', 'Proj Two', 'Proj Three')

# Create an empty c
techs = c('')
for (node in root$children)
{ 
  techs <- c(techs, node$name) 
}

print(root)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("App V1"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         dateInput(
           inputId = "date",
           label = "Analysis Date",
           value = Sys.Date(),
           format = "yyyy-mm-dd"
         ),
         selectInput(inputId = "project",
                     label = "Project",
                     choices = projects
         ),
         selectInput(inputId = "technique",
                     label = "Fundamental Technique",
                     choices = techs
         ),
         uiOutput("methodInput"),
         uiOutput("analyteInput"),
         uiOutput("stringInput")
      ),
      # Show a plot of the generated distribution
      mainPanel(
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$methodInput <- renderUI({
    
    techName = input$technique
    techNode = Navigate(root, techName)
    
    methods <- c('')
    for (node in techNode$children)
    { 
      methods <- c(methods, node$name) 
    }
    
    selectInput(inputId = "method",
                label = "Methodology",
                choices = methods
    )
    
  })
  
  output$analyteInput <- renderUI({
    
    techName = input$technique
    techNode = Navigate(root, techName)
    
    methodName = input$method
    methodNode = Navigate(techNode, methodName)
    
    analytes <- c('')
    for (node in methodNode$children)
    { 
      analytes <- c(analytes, node$name) 
    }
    
    selectInput(inputId = "analyte",
                label = "Analyte",
                choices = analytes
    )
    
  })
  
  output$stringInput <- renderUI({
    
    dateString = format(input$date, format="%Y%m%d")
    project = input$project
    
    techName = input$technique
    techNode = Navigate(root, techName)
    techName = techNode$tag
    
    methodName = input$method
    methodNode = Navigate(techNode, methodName)
    methodName = methodNode$tag
    
    analyteName = input$analyte
    analyteNode = Navigate(methodNode, analyteName)
    analyteName = analyteNode$tag
    
    textInput(inputId = "string",
              label = "Experiment Name",
              value = paste(dateString, project, techName, methodName, analyteName, sep = "_")
    )
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

