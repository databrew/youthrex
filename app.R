library(shiny)
library(googleVis)
library(DT)
library(leaflet)
library(RColorBrewer)
library(networkD3)
options(gvis.plot.tag = 'chart')
library(shinyBS)
library(shinyLP)
library(ggplot2)
library(shinythemes)
library(plotly)

source('global.R')


ui <- dashboardPage(skin = 'blue',
                    
                    dashboardHeader(disable = TRUE),
                    
                    dashboardSidebar(disable = TRUE),
                    
                    dashboardBody(
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                        fluidRow(column(12,
                                        align = 'center',
                                        h1('Welcome to the Ontario Youth Compass data portal')))
                      ),
                      
                      fluidPage(
                        fluidRow(shinydashboard::box(
                          title = '',
                          solidHeader = TRUE,
                          width = 12,
                          collapsible = FALSE,
                          collapsed = FALSE,
                          column(3,
                                 selectInput('location',
                                             'Location',
                                             choices = unique(census$Geography),
                                             selected = 'Ontario')),
                          column(3,
                                 selectInput('years',
                                             'Years',
                                             choices = unique(census$year),
                                             selected = unique(census$year),
                                             multiple = TRUE)),
                          fluidRow(column(12,
                                          textOutput('map_text'),
                                          leafletOutput('the_map')))
                        )
                        
                        
                        
                        ),
                        
                        # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                        
                        fluidRow(
                          shinydashboard::box(
                            title = 'Demographics',
                            status = 'primary',
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = FALSE,
                            
                            column(3,
                                   plotOutput('demo_plot_pie')),
                            column(6,
                                   DT::dataTableOutput('demo_table'))
                            
                          )
                        ),
                        
                        fluidRow(
                          shinydashboard::box(
                            title = 'Family status',
                            status = 'success',
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            
                            column(6,
                                   plotOutput('fam_plot')),
                            column(6,
                                   DT::dataTableOutput('fam_table'))
                            
                          )
                        ),
                        
                        fluidRow(
                          shinydashboard::box(
                            title = 'Education',
                            status = 'info',
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            
                            column(6,
                                   plotOutput('ed_plot')),
                            column(6,
                                   DT::dataTableOutput('ed_table'))
                            
                          )
                        ),
                        
                        fluidRow(
                          shinydashboard::box(
                            title = 'Employment',
                            status = 'warning',
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            
                            column(6,
                                   plotOutput('emp_plot')),
                            column(6,
                                   DT::dataTableOutput('emp_table'))
                            
                          )
                        ),
                        
                        fluidRow(
                          shinydashboard::box(
                            title = 'Housing',
                            status = 'danger',
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            
                            column(6,
                                   plotOutput('house_plot')),
                            column(6,
                                   DT::dataTableOutput('house_table'))
                            
                          )
                        ),
                        
                        fluidRow(
                          shinydashboard::box(
                            title = 'Income',
                            solidHeader = TRUE,
                            width = 12,
                            collapsible = TRUE,
                            collapsed = TRUE,
                            
                            column(6,
                                   plotOutput('income_plot')),
                            column(6,
                                   DT::dataTableOutput('income_table'))
                            
                          )
                        )
                        
                        
                      )
                      
                      
                    )
                    
)


# Define server 
server <- function(input, output) {
  
  get_demo_data <- reactive({
    
    
    # subset data by inputs 
    location <- input$location
    years <- input$years
    
    demo_vars <- c("Geography",  "geo_code", "year", "Age group", "Sex", "Place of Birth","Visible minority", "Aboriginal identity", 'Population')
    x <- census[ , demo_vars]
    # location = 'Toronto'
    # years = c(2001, 2006, 2011, 2016)
    
   
    temp <- x %>% filter(Geography %in% location) %>% 
      filter(year %in% years) %>% filter(!grepl('Total',`Age group`)) %>%
      filter(grepl('Total',`Sex`)) %>% filter(grepl('Total',`Place of Birth`)) %>%
      filter(grepl('Total',`Visible minority`)) %>% filter(grepl('Total',`Aboriginal identity`)) 
    
    # keep only age group, year, and population
    temp <- temp[, c('year','Age group','Population')]
    
    return(temp)
  })
  
  
  # demo_plot_pie
  output$demo_plot_pie <- renderPlot({

    temp_data <- get_demo_data()
    location <- input$location
    x = c(1,2,3,4,5,6,7,8,45,32,6)
    y = c(1,6,3,4,1,6,3,8,44,32,6)
    
    plot(x, y)
    

  })
 
  output$demo_plot_bar <- renderPlot({
    
    temp <- get_demo_data
    
  })
  
  
  output$demo_table <- renderDataTable({
    
    temp <- get_demo_data()
    temp <- spread(temp, key = `Age group`, value = Population)
    datatable(temp)
    
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui,
         # htmlTemplate("www/index.html"), 
         server)

