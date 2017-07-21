#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(cowplot)
library(markdown)

# Define colors
palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Pregnancy probabilities"),
   
   sidebarLayout(
     # Sidebar with a slider input for number of weeks and a radio button for firstbirth
      sidebarPanel(
        sliderInput("weekspregnant", "Weeks pregnant:", min = 23, max = 42, value = 30),
        radioButtons("firstbirth",  "First birth?",  c("Yes" = T, "No" = F),  inline = T)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   ),
   
   # Explanation
   fluidRow(
     column(4),
     column(8,  
       # p("")
      includeMarkdown("explanation.md")
      )
   )
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
    
    dat <- read_rds("condprob.rds") %>% 
       filter(firstbirth == input$firstbirth, 
              weekspregnant == input$weekspregnant) %>% 
      gather("weekspregnant_prob", "condprob", starts_with("weekspregnant_")) %>% 
      mutate(weekspregnant_prob = as.numeric(gsub("weekspregnant_", "", weekspregnant_prob))) %>% 
      filter(weekspregnant_prob >= input$weekspregnant)
     
    labels <- dat %>% 
      select(weekspregnant_prob, condprob)
    labels$condproblab <- paste0(formatC(labels$condprob * 100, digits = 1, format = "f"), "%")
     
    ggplot(dat) + 
      geom_bar(aes(weekspregnant_prob, condprob), 
               stat = "identity", position = "dodge", fill = palette[2]) +
      annotate(geom = "text", x = labels$weekspregnant_prob, 
               y = labels$condprob+0.03, label = labels$condproblab) +
      coord_cartesian(xlim = c(input$weekspregnant-0.25, 42.25), ylim= c(0, 1)) +
      xlab("Weeks pregnant") + ylab("Conditional probability") +
      theme_cowplot()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

