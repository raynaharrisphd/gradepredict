#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(shinyWidgets)


# create file with dependencies for hosting
# rerun if adding new packages
#library(rsconnect)
#rsconnect::writeManifest()

# setup
Assessment <- c("Assignments", "Quizzes", "Labs", "Mid-term exam", "Final exam")
#Grades <- c(70,80,90,80,70)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("CHM 100 Grade Predict"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           p("Estimate your grades for these assessments:") ,
           
           # All your styles will go here
  
           sliderInput("Assig",
                        "Assignments",
                        min = 0,
                        max = 100,
                        value = 51, ),
            sliderInput("Quiz",
                        "Quizzes",
                        min = 0,
                        max = 100,
                        value = 80),
            sliderInput("Lab",
                        "Labs",
                        min = 0,
                        max = 100,
                        value = 91),
            sliderInput("Midterm",
                        "Mid-term exam",
                        min = 0,
                        max = 100,
                        value = 76),
            sliderInput("Final",
                        "Final exam",
                        min = 0,
                        max = 100.0,
                        value = 100),
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           p("Final Grade Estimate"),
           tableOutput("table"),
           p("Stacked Bar Plot of Estimated Grade"),
           plotOutput("plot"),
           p("Table of Input Grades to Points"),
           tableOutput("summary")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
    output$table <- renderTable({
      
      Grades <- c(input$Assig, input$Quiz, input$Lab, input$Midterm, input$Final)
      
      df <- as.data.frame(cbind(Assessment, Grades)) %>%
        mutate(Percent = 0.2,
               Grades = as.numeric(Grades),
               Points = Grades * Percent) %>%
        summarize("Grade" = sum(Points)) %>%
        mutate(Letter = ifelse(Grade > 59.5 & Grade < 69.4999, "D",
                               ifelse(Grade > 69.5 & Grade < 79.4999, "C",
                                      ifelse(Grade > 79.5  & Grade < 89.4999, "B",
                                             ifelse(Grade > 89.5, "A", "F")))))
      
      print(df)
    })
    
    output$plot <- renderPlot({
      
      
      Grades <- c(input$Assig, input$Quiz, input$Lab, input$Midterm, input$Final)
      
      
      df <- as.data.frame(cbind(Assessment, Grades)) %>%
        mutate(Percent = 0.2,
               Total = "Total",
               Grades = as.numeric(Grades),
               Grade = Grades * Percent) %>%
        select(Total, Assessment, Grade) 
      
      p <- ggplot(df, aes(x = Total, y = Grade, fill = Assessment)) +
        geom_bar(stat = "identity") +
        theme_bw(base_size = 16) +
        theme(axis.text.x = element_blank()) +
        scale_y_continuous(limits = c(0,100),
                           breaks = c(0,60,70,70,80,90,100)) +
        scale_fill_manual(values = c("#377eb8", "#4daf4a",
                                     "#984ea3", "#e7298a", "#1b9e77"))
      print(p)
      
    })
    
    output$summary <- renderTable({
      

      Grades <- c(input$Assig, input$Quiz, input$Lab, input$Midterm, input$Final)

      df <- as.data.frame(cbind(Assessment, Grades)) %>%
        mutate(Percent = 0.2,
               Grades = as.numeric(Grades),
               Points = Grades * Percent) 
      print(df)
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
