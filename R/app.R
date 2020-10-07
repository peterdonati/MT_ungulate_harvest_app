################################################################################
#
# Shiny app for MT ungulate harvest
#
################################################################################
library(shiny)
library(RCurl)
library(dplyr)
library(ggplot2)
library(rlang)
library(shinythemes)

elk <- read.csv(text = getURL("https://raw.githubusercontent.com/peterdonati/MT_ungulate_harvest_app/main/Datasets/elk_harvest.csv"))
elk$District <- as.character(elk$District)
################################################################################

# UI ===========================================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Montana ungulate harvest estimates by district"),
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "dist",
        label = "Hunting district you want to see:
      (Invalid districts will create empty plots)",
      ),
      selectInput(
        inputId = "split",
        label = "Split data by:",
        choices = c("Nothing" = "Nothing", 
                    "Sex" = "Sex", 
                    "Weapon type" = "Weapon"),
      )
    ),
    mainPanel(
      plotOutput(outputId = "plot", height = 600, width = 800)
    )
  )
)

# server =======================================================================
server <- function(input, output, session){
  
  mytheme <- theme(
    text = element_text(family = "serif", size = 14),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(fill = NA, color = "black"),
    legend.key = element_rect(fill = NA)
  )
  
  output$plot <- renderPlot({
    
    if (input$split == "Nothing"){
      # Filtering to desired district:
      plot_dat <- elk %>% 
        filter(District == input$dist)
      
      #Actual plot:
      return(
        ggplot(plot_dat, aes(x = Year, y = N)) +
          geom_line(size = 1) +
          labs(
            title = paste("Elk harvest estimate for HD", input$dist),
            y = "Estimated harvest"
          ) +
          mytheme
      )
    } else {
      # Filtering to desired district:
      plot_dat <- elk %>% 
        filter(District == input$dist)
      
      # Defining y variable and split:
      if (input$split == "Sex") { 
        y_var <- sym("n_sex")
      } else if (input$split == "Weapon") {
        y_var <- sym("n_weapon")
      } 
      split <- sym(input$split)
      
      #Actual plot:
      return(
        ggplot(plot_dat, aes(x = Year, y = !!y_var)) +
          geom_line(aes(color = !!split), size = 1) +
          labs(
            title = paste("Elk harvest estimate for HD", input$dist),
            y = "Estimated harvest"
          ) +
          mytheme
      )
    }
  })
}


# The app ======================================================================
shinyApp(ui, server)
