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

deer <- read.csv(text = getURL("https://raw.githubusercontent.com/peterdonati/MT_ungulate_harvest_app/main/Datasets/deer_harvest.csv"))
deer$District <- as.character(deer$District)
################################################################################

# UI ===========================================================================
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel("Montana ungulate harvest estimates by district"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "ung_sp",
        label = "Explore dharvest estimates for:",
        choices = c("Deer", "Elk")
      ),
      sidebarPanel(
        selectInput(
          inputId = "sp",
          label = "Explore dharvest estimates for:",
          choices = c("Deer", "Elk")
        ),
        sliderInput(
          inputID = "yr",
          label = "Years:",
          min = 2010,
          max = 2019,
          value = c(2010, 2019),
          step = 1,
          ticks = FALSE
        ),
      
        # Need to make separate UI for each species because of differing data
      conditionalPanel(
        condition = "input.sp == Elk",
        selectInput(
          inputId = "dist",
          label = "Hunting district(s) you want to see:",
          choices = unique(elk$District),
          multiple = TRUE
        ),
        selectInput(
          inputId = "split",
          label = "Split by:",
          choices = c("Just the districts I chose" = "Nothing", 
                      "Sex" = "Sex", 
                      "Weapon type" = "Weapon"),
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.sp == Deer",
      selectInput(
        inputId = "dist",
        label = "Hunting district(s) you want to see:",
        choices = unique(deer$District),
        multiple = TRUE
      ),
      selectInput(
        inputId = "deer_sp",
        label = "Species:",
        choices = c("Whitetail", "Mule", "Combined")
      ),
      selectInput(
        inputId = "split",
        label = "Split by:",
        choices = c("Just the districts I chose" = "Nothing", 
                    "Sex" = "Sex", 
                    "Weapon type" = "Weapon"),
      )
    )
  ),
    
    mainPanel(
      plotOutput(outputId = "plot", height = 700, width = 1000),
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
  
  # Later used in scale_x_continuous() to standardize x axis for when
  # districts are missing data for some years:
  years <- 2015:2019
  
  # Plot creation:
  output$plot <- renderPlot({
    
    if (!is.null(input$dist)){
      #Generating a new dataset containing only selected variables:
      
      if (input$ung_sp == "Elk"){
        plot_dat <- elk[which(elk$District %in% input$dist), ]
      } else if (input$ung_sp == "Deer"){
        plot_dat <- deer[which(deer$District %in% input$dist), ] %>% 
          
      }
      
      if (input$split == "Nothing"){
        return(
          ggplot(plot_dat, aes(x = Year, y = N)) +
            geom_line(aes(color = District)) +
            geom_point(size = 2) +
            labs(
              title = "Elk harvest estimates",
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
            geom_line(aes(linetype = !!split)) +
            geom_point(size = 2) + 
            facet_wrap(~District, ncol = 2) +
            labs(
              title = "Elk harvest estimates",
              y = "Estimated harvest"
            ) +
            scale_x_continuous(breaks = years, minor_breaks = F) +
            mytheme
        )
      }
    } else {
      # Placeholder for when no district is chosen:
      ggplot() +
        annotate("text", 
                 label = "Pick a district!", 
                 x = .5, y= .5, 
                 size = 16,
                 family = "serif") +
        theme_void() +
        theme(
          panel.border = element_rect(fill = NA, color = "black")
        )
    }
  })
}


# The app ======================================================================
shinyApp(ui, server)
