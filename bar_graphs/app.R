#You're gonna need all of these paakcges
library(shiny)
library(htmlwidgets)
library(pivottabler)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(DT)
library(rsconnect)
library(shinythemes)
library(shiny)


#df <- read.csv('./sa_budget_v5.csv') 
#saveRDS(df, 'sa_budget_v5.rds')
df <- readRDS('./sa_budget_v5.rds')


vars <- c(
  'Amount Requested' = 'requested',
  'Amount Denied' = 'denied',
  'Amount Allocated' = 'approved'
)



ui <- navbarPage('', theme = shinytheme('flatly'),
                 
                 # Give the page a title
                 tabPanel('Amount Requested',
                          
                          # Generate a row with a sidebar
                          sidebarLayout(      
                            
                            sidebarPanel(
                              selectInput("group_type", "Group Type:", 
                                          choices= unique(df$group_type)),
                              hr(),
                              helpText("Choose a group type to see how funds are requested within a category.")
                            ),
                            
                            plotOutput("group_plot", width = 750, height = 500)  
                            
                          )),
                 
                 tabPanel("Amount Allocated",
                          
                          # Generate a row with a sidebar
                          sidebarLayout(      
                            
                            sidebarPanel(
                              selectInput("group_type_allocated", "Group Type:", 
                                          choices= unique(df$group_type)),
                              hr(),
                              helpText("Choose a group type to see how funds are allocated within a category.")
                            ),
                            
                            plotOutput("group_plot_allocated", width = 750, height = 500)  
                            
                          )),
                 
                 
                 tabPanel("Amount Denied",
                          
                          # Generate a row with a sidebar
                          sidebarLayout(      
                            
                            sidebarPanel(
                              selectInput("group_type_denied", "Group Type:", 
                                          choices= unique(df$group_type)),
                              hr(),
                              helpText("Choose a group type to see how funds are denied within a category.")
                            ),
                            
                            plotOutput("group_plot_denied", width = 750, height = 500)  
                            
                          ))
                 
                 
                 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$group_plot <- renderPlot({
    
    par(mar=c(20, 4.1, 1, 2.1))
    
    df <- df[df$group_type == input$group_type,]
    
    barplot(df$requested, 
            main=input$region,
            names.arg = df$group,
            las=2,
            col = c('#84344E'),
            horiz = FALSE,
            border = '#5B6770',
            ylab="Amount",
            beside=TRUE)
  })
  
  
  output$group_plot_allocated <- renderPlot({
    
    par(mar=c(20, 4.1, 1, 2.1))
    
    df <- df[df$group_type == input$group_type_allocated,]
    
    barplot(df$approved, 
            main=input$region,
            names.arg = df$group,
            las=2,
            col = c('#84344E'),
            horiz = FALSE,
            border = '#5B6770',
            ylab="Amount",
            beside=TRUE)
  }) 
  
  
  output$group_plot_denied <- renderPlot({
    
    par(mar=c(20, 4.1, 1, 2.1))
    
    df <- df[df$group_type == input$group_type_denied,]
    
    barplot(df$denied, 
            main=input$region,
            names.arg = df$group,
            las=2,
            col = c('#84344E'),
            horiz = FALSE,
            border = '#5B6770',
            ylab="Amount",
            beside=TRUE)
  }) 
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)

