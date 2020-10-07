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
    text = element_text(family = "serif", size = 18),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(fill = NA, color = "black"),
    legend.key = element_rect(fill = NA)
  )
  years <- 2015:2019
  
  output$plot <- renderPlot({
    
    # Filter to desired district:
    plot_dat <- elk %>% 
      filter(District == input$dist)
    
    if (input$split == "Nothing"){
      return(
        ggplot(plot_dat, aes(x = Year, y = N)) +
          geom_line() +
          geom_point(size = 2) +
          labs(
            title = paste("Elk harvest estimate for HD", input$dist),
            y = "Estimated harvest"
          ) +
          scale_x_continuous(breaks = years, minor_breaks = F) +
          mytheme
      )
    } else {
      # Defining y variable and split:
      if (input$split == "Sex") { 
        y_var <- sym("n_sex")
      } else if (input$split == "Weapon") {
        y_var <- sym("n_weapon")
      } 
      split <- sym(input$split)
      
      return(
        ggplot(plot_dat, aes(x = Year, y = !!y_var)) +
          geom_line(aes(color = !!split)) +
          geom_point(aes(color = !!split), size = 2) + 
          labs(
            title = paste("Elk harvest estimate for HD", input$dist),
            y = "Estimated harvest"
          ) +
          scale_x_continuous(breaks = years, minor_breaks = F) +
          mytheme
      )
    }
  })
}


# The app ======================================================================
shinyApp(ui, server)
