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

#df2 <- read.csv('./sa_budget_v5.csv')
#saveRDS(df2, './sa_budget_v5.rds')
df2 <- readRDS('./sa_budget_v5.rds')

# df2$requested <- as.numeric(df2$requested)
# df2$approved <- as.numeric(df2$approved)
# df2$denied <- as.numeric(df2$denied)


ui <- fluidPage(theme = shinytheme('flatly'),
   
   # Application title
   titlePanel("Student Assembly Budget Table"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column(2,
            selectInput("group_type",
                        "Group Type:",
                        c("All",
                          unique(as.character(df2$group_type))))
     ),
     column(2,
            selectInput("group",
                        "Group:",
                        c("All",
                          unique(as.character(df2$group))))
     ),
     column(2,
            sliderInput("requested", "Amount Requested", min(df2$requested, na.rm = TRUE), max(df2$requested, na.rm = TRUE),
                        value = range(df2$requested, na.rm = TRUE), step = 20
            )
     ),
     column(2,
            sliderInput("allocated", "Amount Allocated", min(df2$approved, na.rm = TRUE), max(df2$approved, na.rm = TRUE),
                        value = range(df2$approved, na.rm = TRUE), step = 20
            )
     ),
     column(4,
            sliderInput("denied", "Amount Denied", min(df2$denied, na.rm = TRUE), max(df2$denied, na.rm = TRUE),
                        value = range(df2$denied, na.rm = TRUE), step = 20
            )
     )
     
     
       
   ),
   hr(),
   DT::dataTableOutput("sa_budget_table")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filterData <- reactive({
    
    
    if (input$group_type != 'All') {
      df2 <- df2[df2$group_type == input$group_type,]
    }
    if (input$group != 'All') {
      df2 <- df2[df2$group == input$group,]
    }
    
    df2 <- df2[df2$requested >= input$requested[1] & df2$requested <= input$requested[2],]
    df2 <- df2[df2$approved >= input$allocated[1] & df2$approved <= input$allocated[2],]
    df2 <- df2[df2$denied >= input$denied[1] & df2$denied <= input$denied[2],]
    
    df2
    
  })
    
  
  
  output$sa_budget_table <- DT::renderDataTable(DT::datatable(data = filterData(), colnames = c('Group Type', 'Group', 'Dollar Amount Requested', 'Dollar Amount Approved', 'Dollar Amount Denied'), options = list(autoWidth = FALSE)))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

